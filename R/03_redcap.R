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

get_redcap_info <- function(DB,content,error_action=NULL,additional_args=NULL){
  allowed_content <- c("project","arm","event","metadata","instrument","repeatingFormsEvents","user","userRole","userRoleMapping","log")
  if(!content%in%allowed_content)stop("Must use the following content... ",paste0(allowed_content,collapse = ", "))
  redcap_api_base(DB$redcap_uri,validate_redcap_token(DB),content,additional_args=additional_args) %>% process_response(error_action)
}

get_redcap_file <-

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

raw_process_redcap <- function(raw,DB,clean=T){
  if(nrow(raw)>0){
    for(instrument_name in DB$instruments$instrument_name){
      if(instrument_name%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        DB[["data"]][[instrument_name]]<-raw[which(raw$redcap_repeat_instrument==instrument_name),unique(c(DB$id_col,"redcap_repeat_instance",DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&DB$metadata$field_name%in%colnames(raw))]))]
      }
      if(!instrument_name%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        if("redcap_repeat_instrument" %in% colnames(raw)){
          DB[["data"]][[instrument_name]]<-raw[which(is.na(raw$redcap_repeat_instrument)),unique(c(DB$id_col,DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&DB$metadata$field_name%in%colnames(raw))]))]
        }else{
          DB[["data"]][[instrument_name]]<-raw[,unique(c(DB$id_col,DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&DB$metadata$field_name%in%colnames(raw))]))]
        }
      }
      DB[["data"]][[instrument_name]] <- DB[["data"]][[instrument_name]] %>% all_character_cols()
      if(instrument_name%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        if (nrow(DB[["data"]][[instrument_name]])>0){
          DB[["data"]][[instrument_name]]$redcap_repeat_instrument<-instrument_name
        }#had to add for empty instruments
      }
    }
  }
  # if(  use_missing_codes) warning("You have missing codes in your redcap. such as UNK for unknown.")
  DB$clean<-clean
  DB
}

raw_to_clean_redcap <- function(DB){
  if(DB$clean)stop("DB is already clean (not raw coded values)")
  use_missing_codes <- !is.data.frame(DB$missing_codes)
  if(nrow(raw)>0){
    for(instrument_name in DB$instruments$instrument_name){
      for (field_name in DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&DB$metadata$field_type%in%c("radio","dropdown"))]){
        z<-DB$metadata$select_choices_or_calculations[which(DB$metadata$field_name==field_name)] %>% split_choices()
        DB[["data"]][[instrument_name]][[field_name]]<-DB[["data"]][[instrument_name]][[field_name]] %>% sapply(function(C){
          OUT<-NA
          if(!is.na(C)){
            coded_redcap<-which(z$code==C)
            if(length(coded_redcap)>0){
              OUT<-z$name[coded_redcap]
            }else{
              if(use_missing_codes){
                coded_redcap2<-which(DB$missing_codes$code==C)
                if(length(coded_redcap2)>0){
                  OUT<-DB$missing_codes$name[coded_redcap2]
                }else{
                  stop("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C,". Also not a missing code.")
                }
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C)
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()

      }
      for (field_name in DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&DB$metadata$field_type=="yesno")]){
        z<-data.frame(
          code=c(0,1),
          name=c("No","Yes")
        )
        DB[["data"]][[instrument_name]][[field_name]]<-DB[["data"]][[instrument_name]][[field_name]] %>% sapply(function(C){
          OUT<-NA
          if(!is.na(C)){
            D<-which(z$code==C)
            if(length(D)>0){
              OUT<-z$name[D]
            }
            if(length(D)==0){
              if(use_missing_codes){
                E<-which(DB$missing_codes$code==C)
                if(length(E)==0){
                  stop("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C,". Also not a missing code.")
                }
                if(length(E)>0){
                  OUT<-DB$missing_codes$name[E]
                }
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C)
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
      for (field_name in DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&DB$metadata$field_type=="checkbox_choice")]){
        z<-data.frame(
          code=c(0,1),
          name=c("Unchecked","Checked")
        )
        DB[["data"]][[instrument_name]][[field_name]]<-DB[["data"]][[instrument_name]][[field_name]] %>% sapply(function(C){
          OUT<-NA
          if(!is.na(C)){
            D<-which(z$code==C)
            if(length(D)>0){
              OUT<-z$name[D]
            }
            if(length(D)==0){
              if(use_missing_codes){
                E<-which(DB$missing_codes$code==C)
                if(length(E)==0){
                  stop("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C,". Also not a missing code.")
                }
                if(length(E)>0){
                  OUT<-DB$missing_codes$name[E]
                }
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C)
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
      if(use_missing_codes){
        for(field_name in DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&!DB$metadata$field_type%in%c("radio","dropdown","yesno","checkbox","checkbox_choice","descriptive"))]){
          z<-DB$missing_codes
          DB[["data"]][[instrument_name]][[field_name]]<-DB[["data"]][[instrument_name]][[field_name]] %>% sapply(function(C){
            OUT<-C
            if(!is.na(C)){
              D<-which(z$code==C)
              if(length(D)>0){
                OUT<-z$name[D]
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }
      }
    }
  }
  # if(  use_missing_codes) warning("You have missing codes in your redcap. such as UNK for unknown.")
  DB$clean<-clean
  DB
}

get_redcap_users<-function(DB){
  userRole <-get_redcap_info(DB,"userRole") %>% dplyr::select("unique_role_name","role_label")
  userRoleMapping<- get_redcap_info(DB,"userRoleMapping")
  user<- get_redcap_info(DB,"user")
  merge(merge(userRole,userRoleMapping,by="unique_role_name"),user, by="username")
}

select_redcap_records<-function(DB, records=NULL){
  DB_selected<-DB
  DB_selected$data<-list()
  if(!is.null(records)){
    BAD <-records[which(!records%in%DB$data$identity[[DB$id_col]])]
    if(length(BAD)>0)stop(message("Following records are not found in DB: ", paste0(BAD,collapse = ", ")))
  }
  for(x in DB$instruments$instrument_name){
    OUT <- DB[["data"]][[x]]
    if(!is.null(records)){
      OUT<-OUT[which(OUT[[DB$id_col]]%in%records),]
    }
    DB_selected[["data"]][[x]]<-OUT
  }
  DB_selected
}

clean_to_raw_redcap <- function(DB){
  DB <- validate_DB(DB)
  for(TABLE in names(DB[["data"]])){
    DB[["data"]][[TABLE]] <- clean_to_raw_form(FORM = DB[["data"]][[TABLE]],DB=DB)
  }
  DB$clean<-F
  DB
}

clean_to_raw_form <- function(FORM,DB){
  use_missing_codes <- !is.na(DB$missing_codes)
  # if(!deparse(substitute(FORM))%in%DB$instruments$instrument_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  if(any(!colnames(FORM)%in%DB$metadata$field_name))stop("All column names in your form must match items in your metadata, `DB$metadata$field_name`")
  instrument <- DB$metadata$form_name[
    which(
      DB$metadata$field_name%in%colnames(FORM)&
        !DB$metadata$field_name%in%c(DB$id_col,"redcap_repeat_instance","redcap_repeat_instrument")
    )
  ] %>% unique()
  if(length(instrument)>1)stop("All column names in your form must match only one form in your metadata, `DB$instruments$instrument_name`")
  metadata<-DB$metadata[which(DB$metadata$form_name==instrument),]
  for(COL_NAME in FORM %>% colnames()){
    if(COL_NAME%in%metadata$field_name[which(metadata$field_type%in%c("radio","dropdown","checkbox_choice"))]){
      z<-metadata$select_choices_or_calculations[which(metadata$field_name==COL_NAME)] %>% split_choices()
      FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          coded_redcap<-which(z$name==C)
          if(length(coded_redcap)>0){
            OUT<-z$code[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2<-which(DB$missing_codes$name==C)
              if(length(coded_redcap2)>0){
                OUT<-DB$missing_codes$code[coded_redcap2]
              }else{
                stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }
    if(COL_NAME%in%metadata$field_name[which(metadata$field_type=="yesno")]){
      z<-data.frame(
        code=c(0,1),
        name=c("No","Yes")
      )
      FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          D<-which(z$name==C)
          if(length(D)>0){
            OUT<-z$code[D]
          }
          if(length(D)==0){
            if(use_missing_codes){
              E<-which(DB$missing_codes$name==C)
              if(length(E)==0){
                stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
              }
              if(length(E)>0){
                OUT<-DB$missing_codes$code[E]
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C)
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }
    if(use_missing_codes){
      if(COL_NAME%in%metadata$field_name[which(!metadata$field_type%in%c("radio","dropdown","yesno","checkbox_choice"))]){
        FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
          OUT<-C
          if(!is.na(C)){
            D<-which(DB$missing_codes$name==C)
            if(length(D)>0){
              OUT<-DB$missing_codes$code[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  FORM
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

clean_redcap_log <- function(log){
  log$record_id <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),gsub("Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation| |[:):]|[:(:]","",A),NA)})
  log$action_type <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),(A %>% strsplit(" ") %>% unlist())[1],NA)})
  comments <- which(log$action=="Manage/Design"&grepl("Add field comment|Edit field comment|Delete field comment",log$details))
  if(length(comments)>0){
    log$record_id[comments] <- stringr::str_extract(log$details[comments], "(?<=Record: )[^,]+")
    log$action_type[comments] <- "Comment"
  }
  log
}

all_missing_codes <- function(){
  data.frame(
    code = c(
      'NI',
      'INV',
      'UNK',
      'NASK',
      'ASKU',
      'NAV',
      'MSK',
      'NA',
      'NAVU',
      'NP',
      'QS',
      'QI',
      'TRC',
      'UNC',
      'DER',
      'PINF',
      'NINF',
      'OTH'
    ),
    name = c(
      'No information',
      'Invalid',
      'Unknown',
      'Not asked',
      'Asked but unknown',
      'Temporarily unavailable',
      'Masked',
      'Not applicable',
      'Not available',
      'Not present',
      'Sufficient quantity',
      'Insufficient quantity',
      'Trace',
      'Unencoded',
      'Derived',
      'Positive infinity',
      'Negative infinity',
      'Other'
    )
  )
}

missing_codes2 <- function(DB){
  included <- "missing_data_codes"%in%colnames(DB$project_info)
  if(included){
    is_na <-is.na(DB$project_info$missing_data_codes)
    if(!is_na){
      return(DB$project_info$missing_data_codes %>% split_choices())
    }
    if(is_na){
      return(NA)
    }
  }
  if(!included){
    return(NA)
  }
}

#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param records character vector of records you want dropped to your directory
#' @return messages for confirmation
#' @export
drop_redcap_dir<-function(DB,records=NULL){
  DB <- validate_DB(DB)
  dir.create(file.path(get_dir(DB),"REDCap"),showWarnings = F)
  dir.create(file.path(get_dir(DB),"REDCap","other"),showWarnings = F)
  dir.create(file.path(get_dir(DB),"REDCap","upload"),showWarnings = F)
  DB_selected<- DB %>% select_redcap_records(records)
  for(x in DB$instruments$instrument_name){
    DB_selected[["data"]][[x]] %>% write_xl(DB,path=file.path(get_dir(DB),"REDCap",paste0(x,".xlsx")))
  }
  for (x in c("metadata","instruments","users")){ #,"log" #taking too long
    DB_selected[[x]] %>% write_xl(DB,path=file.path(get_dir(DB),"REDCap","other",paste0(x,".xlsx")))
  }
}

#' @title Reads DB from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
read_redcap_dir<-function(DB){
  DB <- validate_DB(DB)
  path<-file.path(get_dir(DB),"REDCap","upload")
  if(!file.exists(path))stop("No REDCap files found at path --> ",path)
  x<-list.files.real(path)
  x<-x[which(gsub("\\.xlsx","",x)%in%DB$instruments$instrument_name)]
  DB_import<-DB
  DB_import[["data"]]<-list()
  for(y in x){
    DB_import[["data"]][[gsub("\\.xlsx","",y)]] <- readxl::read_xlsx(file.path(path,y)) %>% all_character_cols()
  }
  DB_import
}

#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue. Missing rows and columns are fine!
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean data pass your data to `clean_to_raw_form(FORM,DB)` first.
#' @inheritParams save_DB
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
#' @export
upload_form_to_redcap<-function(to_be_uploaded,DB,batch_size=500){
  REDCapR::redcap_write(
    ds_to_write = to_be_uploaded %>% all_character_cols(),
    batch_size=batch_size,
    interbatch_delay=0.2,
    continue_on_error=FALSE,
    redcap_uri = DB$redcap_uri,
    token = validate_redcap_token(DB),
    overwrite_with_blanks=TRUE
  )
}

#' @title Upload from your directory to REDCap
#' @description
#' This function is meant to be run after `DB_import <- read_redcap_dir(DB)`.
#' It compares DB_import to DB and only uploads the changes.
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue.
#' @inheritParams save_DB
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @param ask logical for if you want to preview uploads first
#' @return messages
#' @export
upload_DB_to_redcap<-function(DB,batch_size=500,ask=T){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB<-validate_DB(DB)
  if(ask){
    if(count_DB_cells(DB)>4000){
      stop <-utils::menu(choices = c("YES - I want to stop and double check what I'm about to upload","NO - Move forward with larger upload"),title = "This is a large upload. Do you want to stop and double check it first?")
      if(stop==1)stop("Double check DB object prior to upload")
    }
  }
  if(DB$clean){
    DB<-clean_to_raw_redcap(DB)
  }
  warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(is.na(DB[["data"]]))stop("`DB$data` is empty")
  for(TABLE in names(DB[["data"]])){
    to_be_uploaded <- DB[["data"]][[TABLE]]
    if(nrow(to_be_uploaded)>0){
      do_it <- 1
      if(ask){
        print.data.frame(to_be_uploaded)
        do_it <-utils::menu(choices = c("Yes upload","No and go to next"),title = "Do you want to upload this?")
      }
      if(do_it==1){
        upload_form_to_redcap(to_be_uploaded=all_character_cols(to_be_uploaded),DB=DB,batch_size=batch_size)
      }
    }
  }
}

#' @title Find the DB_import and DB differences
#' @description
#' This function is meant to be run after `DB_import <- read_redcap_dir(DB)`.
#' It compares DB_import to DB and only uploads the changes.
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue.
#' @param DB_import obtained from your directory 'REDCap/upload' folder using, `DB_import <- read_redcap_dir(DB)`
#' @inheritParams upload_form_to_redcap
#' @param ignore_instruments character vector of instruments to be ignored if you know they are unchanged.
#' @return DB_import but only the differences
#' @export
find_DB_diff <- function(DB_import,DB,ignore_instruments){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB_import<-validate_DB(DB_import)
  DB<-validate_DB(DB)
  if(!missing(ignore_instruments)){
    if(any(!ignore_instruments%in%names(DB_import[["data"]])))stop("ignore_instruments must be included in the set of instrument names, `names(DB_import$data)`")
    for(DROP in ignore_instruments){
      DB_import$data[[DROP]]<-NULL
    }
  }
  if(DB_import$clean&!DB$clean){
    DB_import<-clean_to_raw_redcap(DB_import)
  }
  warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(any(!names(DB_import[["data"]])%in%names(DB[["data"]])))stop("All file names and data.table names from your directory a.k.a. `names(DB_import$data)` must match the DB instrument names, `DB$instruments$instrument_name`")
  if(is.na(DB_import[["data"]]))stop("`DB_import$data` is empty")
  for(TABLE in names(DB_import[["data"]])){
    ref_cols <- DB$id_col
    if(TABLE%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
      ref_cols <- c(DB$id_col,"redcap_repeat_instrument","redcap_repeat_instance")
    }
    DB_import[["data"]][[TABLE]] <- find_df_diff(new= DB_import[["data"]][[TABLE]] , old =  DB[["data"]][[TABLE]], ref_cols = ref_cols)
  }
  DB_import
}
