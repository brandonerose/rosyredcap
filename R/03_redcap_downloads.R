redcap_api_base <- function(url,token,content,additional_args=NULL){
  body <-list(
    "token"= token,
    content=content,
    format='csv',
    returnFormat='json'
  )
  if(!missing(additional_args)||is.null(additional_args)){
    body <- body %>% append(additional_args)
  }
  httr::POST(
    url = url,
    body = body,
    encode = "form"
  )
}

process_response <- function(response,error_action){
  content <- httr::content(response)
  if(httr::http_error(response)){
    if(!is.missing(error_action)){
      if(!error_action%in%c("stop","warn"))stop("error_action must be 'stop' or 'warn'")
      general_error<-response$status_code
      specific_error<-response$status_code
      message <- paste0("HTTP error ",response$status_code, ". ",content[["error"]])
      if(error_action=="stop"){
        stop(message)
      }
      warning(message,immediate. = T)
    }
    return(NA)
  }
  return(all_character_cols(content))
}

test_redcap <- function(DB){
  ERROR <-T
  while(ERROR){
    version <- redcap_api_base(DB$redcap_uri,validate_redcap_token(DB),"version")
    ERROR <-version %>% process_response()
    if(ERROR){
      warning('Your REDCap API token check failed. Invalid token or API privileges. Contact Admin! Consider rerunnning `setup_DB()`',immediate. = T)
      if(!missing(project_info))warning("HTTP error ",project_info %>% httr::status_code(), ". Check your token, internet connection, and redcap base link.",immediate. = T)
      message("Try going to REDCap --> 'https://redcap.miami.edu/redcap_v13.1.29/API/project_api.php?pid=6317' or run `link_API_token(DB)`")
      set_redcap_token(DB)
    }
  }
  message("Connected to REDCap!")
  DB$version <- version %>% httr::content(as="text") %>% as.character()
  DB
}

get_redcap_info <- function(DB,content,error_action=NULL,additional_args=NULL){
  allowed_content <- c("project","arm","event","metadata","instrument","repeatingFormsEvents","user","userRole","userRoleMapping","log")
  if(!content%in%allowed_content)stop("Must use the following content... ",paste0(allowed_content,collapse = ", "))
  redcap_api_base(DB$redcap_uri,validate_redcap_token(DB),content,additional_args=additional_args) %>% process_response(error_action)
}

get_redcap_file <- function(){
  #placeholder
}

get_redcap_metadata<-function(DB){
  DB$last_metadata_update=Sys.time()
  DB$project_info <- get_redcap_info(DB,"project")
  DB$title=DB$project_info$project_title
  DB$PID=DB$project_info$project_id
  DB$arms <- get_redcap_info(DB,"arm")
  DB$events <- get_redcap_info(DB,"event","warn")
  DB$metadata <- get_redcap_info(DB,"metadata","warn")
  DB$metadata<-DB$metadata %>%dplyr::bind_rows(
    data.frame(
      field_name=paste0(unique(DB$metadata$form_name),"_complete"),form_name=unique(DB$metadata$form_name),field_type="radio",select_choices_or_calculations="0, Incomplete | 1, Unverified | 2, Complete"
    )
  ) %>% unique()
  if(is.data.frame(DB$metadata)){
    radios<-which(DB$metadata$field_type=="radio")
    if(length(radios)>0){
      for(field in DB$metadata$field_name[radios]){
        DB[["choices"]][[field]]<-DB$metadata$select_choices_or_calculations[which(DB$metadata$field_name==field)] %>% split_choices()
      }
    }
  }
  if(any(DB$metadata$field_type=="checkbox")){
    for(field_name in DB$metadata$field_name[which(DB$metadata$field_type=="checkbox")]){
      x<-DB$metadata$select_choices_or_calculations[which(DB$metadata$field_name==field_name)] %>% split_choices()
      DB$metadata<-DB$metadata %>%dplyr::bind_rows(
        data.frame(
          field_name=paste0(field_name,"___",x$code),
          form_name=DB$metadata$form_name[which(DB$metadata$field_name==field_name)]  ,
          field_label=paste0(DB$metadata$field_label[which(DB$metadata$field_name==field_name)]," - ",x$name),
          field_type="checkbox_choice",
          select_choices_or_calculations=c("0, Unchecked | 1, Checked")
        )
      ) %>% all_character_cols()
    }
  }

  DB$id_col<-DB$metadata[1,1] %>% as.character() #RISKY?
  DB$instruments <- get_redcap_info(DB,"instrument","warn")
  DB$instruments$repeating <- F
  # if(DB$project_info$has_repeating_instruments_or_events=="1")
  repeating <- get_redcap_info(DB,"repeatingFormsEvents")
  if(is.data.frame(repeating)){
    DB$instruments$repeating <- DB$instruments$instrument_name%in%repeating
    DB$metadata<-DB$metadata %>%dplyr::bind_rows(
      data.frame(
        field_name="redcap_repeat_instance",form_name=DB$instruments$instrument_name[which(DB$instruments$repeating)] ,field_label="REDCap Repeat Instance",field_type="text",select_choices_or_calculations=NA
      )
    ) %>% unique()
    DB$metadata<-DB$metadata %>%dplyr::bind_rows(
      data.frame(
        field_name="redcap_repeat_instrument",form_name=DB$instruments$instrument_name[which(DB$instruments$repeating)] ,field_label="REDCap Repeat Instrument",field_type="text",select_choices_or_calculations=NA
      )
    ) %>% unique()
  }
  DB$users <- get_redcap_users(DB)
  DB$codebook <- make_codebook(DB)
  DB$missing_codes <- missing_codes2(DB)
  DB$log<-check_redcap_log(DB,last = 2,units = "mins")
  DB$users$current_user<-DB$users$username==DB$log$username[which(DB$log$details=="Export REDCap version (API)") %>% dplyr::first()]
  DB$home_link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/index.php?pid=",DB$PID)
  DB$records_link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/DataEntry/record_status_dashboard.php?pid=",DB$PID)
  DB$API_link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/API/project_api.php?pid=",DB$PID)
  DB$API_playground_link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/API/playground.php?pid=",DB$PID)
  DB
}

get_redcap_data<-function(DB,clean=T,records=NULL){
  DB$last_data_update=Sys.time()
  raw <- REDCapR::redcap_read(redcap_uri=DB$redcap_uri, token=validate_redcap_token(DB),batch_size = 2000, interbatch_delay = 0.1,records = records, raw_or_label = ifelse(clean,"label","raw"))$data %>% all_character_cols()
  DB <- raw_process_redcap(raw = raw,DB = DB)
  if(is.null(records)){
    DB$all_records <- all_records(DB)
  }
  DB
}

get_redcap_users<-function(DB){
  userRole <-get_redcap_info(DB,"userRole") %>% dplyr::select("unique_role_name","role_label")
  userRoleMapping<- get_redcap_info(DB,"userRoleMapping")
  user<- get_redcap_info(DB,"user")
  merge(merge(userRole,userRoleMapping,by="unique_role_name"),user, by="username")
}

#' @title Check the REDCap log
#' @inheritParams save_DB
#' @param last numeric paired with units. Default is 24.
#' @param units character paired with last. Options are "mins","hours","days". Default is "hours".
#' @param begin_time character of time where the log should start from. Example 2023-07-11 13:15:06.
#' @return data.frame of log that has been cleaned and has extra summary columns
#' @export
check_redcap_log <- function(DB,last=24,units="hours",begin_time=""){
  if(units=="days"){
    x<-(Sys.time()-lubridate::days(last)) %>% as.character()
  }
  if(units=="hours"){
    x<-(Sys.time()-lubridate::hours(last)) %>% as.character()
  }
  if(units=="mins"){
    x<-(Sys.time()- lubridate::minutes(last)) %>% as.character()
  }
  if(begin_time!=""){
    x<-begin_time
  }
  get_redcap_info(DB,"log",additional_args = list(beginTime=x)) %>% clean_redcap_log()
}
