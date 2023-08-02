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
    if(!missing(error_action)){
      if(!is.null(error_action)){
        if(!error_action%in%c("stop","warn"))stop("error_action must be 'stop' or 'warn'")
        general_error<-response$status_code
        specific_error<-http_errors$Description[which(http_errors$Value==response$status_code)]
        message <- paste0("HTTP error ",general_error, ". ",specific_error,". ",content[["error"]])
        if(error_action=="stop")stop(message)
        warning(message,immediate. = T)
      }
    }
    return(NA)
  }
  return(all_character_cols(content))
}

test_redcap <- function(DB){
  ERROR <-T
  while(ERROR){
    version <- redcap_api_base(DB$redcap_uri,validate_redcap_token(DB),"version")
    ERROR <-version %>% httr::http_error()
    if(ERROR){
      warning('Your REDCap API token check failed. Invalid token or API privileges. Contact Admin! Consider rerunnning `setup_DB()`',immediate. = T)
      warning("HTTP error ",version %>% httr::status_code(), ". Check your token, internet connection, and redcap base link.",immediate. = T)
      message("Try going to REDCap --> 'https://redcap.miami.edu/redcap_v13.1.29/API/project_api.php?pid=6317' or run `link_API_token(DB)`")
      set_redcap_token(DB)
    }
  }
  message("Connected to REDCap!")
  DB$version <- version %>% httr::content(as="text") %>% as.character()
  DB
}

get_redcap_info <- function(DB,content,error_action=NULL,additional_args=NULL){
  allowed_content <- c("project","arm","event","metadata","instrument","repeatingFormsEvents","user","userRole","userRoleMapping","log","formEventMapping")
  if(!content%in%allowed_content)stop("Must use the following content... ",paste0(allowed_content,collapse = ", "))
  redcap_api_base(url=DB$redcap_uri,token = validate_redcap_token(DB),content = content,additional_args=additional_args) %>% process_response(error_action)
}

#' @title Drop redcap files to directory
#' @inheritParams save_DB
#' @param original_file_names logical for using original uploaded filenames vs system defined
#' @param overwrite logical rewriting over the downladed files in your directory. A better alternative is deleting files you want to update.
#' @return message
#' @export
get_redcap_files <- function(DB,original_file_names = F,overwrite = F){
  file_rows <- which(DB$metadata$field_type=="file")
  out_dir <- file.path(DB$dir_path,"REDCap","files")
  if(length(file_rows)>0){
    dir.create(out_dir,showWarnings = F)
    for(field_name in DB$metadata$field_name[file_rows]){
      out_dir_folder <- file.path(out_dir,field_name)
      dir.create(out_dir_folder,showWarnings = F)
      form_name <- DB$metadata$form_name[which(DB$metadata$field_name == field_name)]
      is_repeating <- DB$instruments$repeating[which(DB$instruments$instrument_name==form_name)]
      form <- DB$data[[form_name]]
      rows_to_save <- which(!is.na(form[[field_name]]))
      for(i in rows_to_save){
        file_name <-form[[field_name]][i]
        record_id <- form[[DB$id_col]][i]
        repeat_instrument = form[["redcap_repeat_instrument"]][i]
        repeat_instance = form[["redcap_repeat_instance"]][i]
        if(!original_file_names){
          if(anyDuplicated(file_name)>0){
            warning(paste0("You have duplicate file names in ",form_name,", ",field_name,". Therefore will use system generated names"),immediate. = T)
            original_file_names <- F
          }
        }
        file_name <- ifelse(original_file_names,file_name,paste0(form_name,"_",field_name,"_",ifelse(is_repeating,"inst_",""),repeat_instance,"ID_",record_id,".",tools::file_ext(file_name)))
        if(!file.exists(file.path(out_dir_folder,file_name))||overwrite){
          REDCapR::redcap_download_file_oneshot(
            redcap_uri = DB$redcap_uri,
            token = validate_redcap_token(DB),
            field = field_name,
            record = DB$data$results$record_id[i],
            directory = out_dir_folder,
            file_name = file_name,
            repeat_instrument = repeat_instrument,
            repeat_instance = repeat_instance,
            verbose = F
          )
          message("`",file_name,"` saved at --> ",out_dir_folder)
        }
      }
    }
  }
  message("Checked for files!")
}

get_redcap_metadata<-function(DB){
  DB$last_metadata_update=Sys.time()
  DB$project_info <- get_redcap_info(DB,"project")
  DB$title=DB$project_info$project_title
  DB$PID=DB$project_info$project_id
  DB$metadata <- get_redcap_info(DB,"metadata","stop")
  DB$metadata$section_header <- DB$metadata$section_header %>% remove_html_tags()
  DB$metadata$field_label <- DB$metadata$field_label %>% remove_html_tags()
  DB$arms <- get_redcap_info(DB,"arm")
  DB$events <- get_redcap_info(DB,"event","warn")
  DB$event_mapping  <- get_redcap_info(DB,"formEventMapping","warn")

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
  if(any(DB$metadata$field_type=="yesno")){
    DB$metadata$select_choices_or_calculations[which(DB$metadata$field_type=="yesno")] <- c("0, No | 1, Yes")
  }

  DB$id_col<-DB$metadata[1,1] %>% as.character() #RISKY?
  DB$instruments <- get_redcap_info(DB,"instrument","warn")
  DB$instruments$repeating <- F
  # if(DB$project_info$has_repeating_instruments_or_events=="1")
  repeating <- get_redcap_info(DB,"repeatingFormsEvents")
  if(is.data.frame(repeating)){
    DB$instruments$repeating <- DB$instruments$instrument_name%in%repeating$form_name
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
  DB <- raw_process_redcap(raw = raw,DB = DB,clean = clean)
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
