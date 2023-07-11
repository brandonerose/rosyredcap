get_redcap_project_info <- function(DB){
  httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"= validate_redcap_token(DB,silent = F) ,
      content='project',
      format='csv',
      returnFormat='json'
    ),
    encode = "form"
  )
}

get_redcap_metadata<-function(DB){
  DB$last_metadata_update=Sys.time()
  DB$metadata=REDCapR::redcap_metadata_read(redcap_uri=DB$redcap_uri, token=validate_redcap_token(DB))$data
  DB$metadata<-DB$metadata %>%dplyr::bind_rows(
    data.frame(
      field_name=paste0(unique(DB$metadata$form_name),"_complete"),form_name=unique(DB$metadata$form_name),field_type="radio",select_choices_or_calculations="0, Incomplete | 1, Unverified | 2, Complete"
    )
  ) %>% unique()
  DB$id_col<-DB$metadata[1,1] %>% as.character() #RISKY?
  if(!is.null(DB$metadata)){
    x<-which(DB$metadata$field_type=="radio")
    if(length(x)>0){
      for(field in DB$metadata$field_name[x]){
        DB[["choices"]][[field]]<-DB$metadata$select_choices_or_calculations[which(DB$metadata$field_name==field)] %>% split_choices()
      }
    }
  }
  DB$instruments=REDCapR::redcap_instruments(redcap_uri=DB$redcap_uri, token=validate_redcap_token(DB))$data
  repeating<-httr::content(
    httr::POST(
      url = DB$redcap_uri,
      body = list(
        "token"=validate_redcap_token(DB),
        content='repeatingFormsEvents',
        format='csv',
        returnFormat='json'
      ),
      encode = "form"
    )
  )$form_name
  DB$instruments$repeating <- DB$instruments$instrument_name%in%repeating
  if(length(repeating)>0){
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
      )
    }
  }
  DB$users<-get_redcap_users(DB)
  DB$version=paste0(unlist(REDCapR::redcap_version(redcap_uri=DB$redcap_uri, token=validate_redcap_token(DB))),collapse = ".")
  DB$codebook <- make_codebook(DB)
  missing_codes <- missing_codes2(DB)
  if(!is.null(missing_codes)){
    DB$missing_codes <- missing_codes
  }
  DB$log<-check_redcap_log(DB,last = 2,units = "mins")
  DB$users$current_user<-DB$users$username==DB$log$username[which(DB$log$details=="Export REDCap version (API)") %>% dplyr::first()]
  DB$home_link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/index.php?pid=",DB$PID)
  DB$records_link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/DataEntry/record_status_dashboard.php?pid=",DB$PID)
  DB$API_link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/API/project_api.php?pid=",DB$PID)
  DB$API_playground_link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/API/playground.php?pid=",DB$PID)
  DB
}

get_redcap_users<-function(DB){
  merge(
    merge(
      httr::content(
        httr::POST(
          url = DB$redcap_uri,
          body = list(
            "token"=validate_redcap_token(DB),
            content='userRole',
            format='csv',
            returnFormat='json'
          ), encode = "form")
      ) %>% dplyr::select("unique_role_name","role_label"),
      httr::content(
        httr::POST(
          url = DB$redcap_uri,
          body = list(
            "token"=validate_redcap_token(DB),
            content='userRoleMapping',
            format='csv',
            returnFormat='json'
          ), encode = "form")
      ),
      by="unique_role_name"),
    httr::content(
      httr::POST(
        url = DB$redcap_uri,
        body = list(
          "token"=validate_redcap_token(DB),
          content='user',
          format='csv',
          returnFormat='json'
        ), encode = "form")
    ),
    by="username"
  )
}

get_redcap_data<-function(DB,clean=T,records=NULL){
  DB$last_data_update=Sys.time()
  raw <- REDCapR::redcap_read(redcap_uri=DB$redcap_uri, token=validate_redcap_token(DB),batch_size = 2000, interbatch_delay = 0.1,records = records)$data
  DB<-DB %>% raw_process_redcap(raw = raw,clean = clean)
  DB$all_records <- all_records(DB)
  DB
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

raw_process_redcap <- function(DB,raw,clean=T){
  use_missing_codes <- !is.null(DB$missing_codes)
  if(nrow(raw)>0){
    for(x in DB$instruments$instrument_name){
      if(x%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        DB[["data"]][[x]]<-raw[which(raw$redcap_repeat_instrument==x),unique(c(DB$id_col,"redcap_repeat_instance",DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_name%in%colnames(raw))]))]
      }
      if(!x%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        if("redcap_repeat_instrument" %in% colnames(raw)){
          DB[["data"]][[x]]<-raw[which(is.na(raw$redcap_repeat_instrument)),unique(c(DB$id_col,DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_name%in%colnames(raw))]))]
        }else{
          DB[["data"]][[x]]<-raw[,unique(c(DB$id_col,DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_name%in%colnames(raw))]))]
        }
      }
      for(COL in colnames(DB[["data"]][[x]])){
        DB[["data"]][[x]][[COL]]<-DB[["data"]][[x]][[COL]] %>% as.character()
      }
      if(clean){
        for (y in DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_type%in%c("radio","dropdown"))]){
          z<-DB$metadata$select_choices_or_calculations[which(DB$metadata$field_name==y)] %>% split_choices()
          DB[["data"]][[x]][[y]]<-DB[["data"]][[x]][[y]] %>% sapply(function(C){
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
                    stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C,". Also not a missing code.")
                  }
                }else{
                  stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C)
                }
              }
            }
            OUT
          }) %>% unlist() %>% as.character()

        }
        for (y in DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_type=="yesno")]){
          z<-data.frame(
            code=c(0,1),
            name=c("No","Yes")
          )
          DB[["data"]][[x]][[y]]<-DB[["data"]][[x]][[y]] %>% sapply(function(C){
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
                    stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C,". Also not a missing code.")
                  }
                  if(length(E)>0){
                    OUT<-DB$missing_codes$name[E]
                  }
                }else{
                  stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C)
                }
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }
        for (y in DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_type=="checkbox_choice")]){
          z<-data.frame(
            code=c(0,1),
            name=c("Unchecked","Checked")
          )
          DB[["data"]][[x]][[y]]<-DB[["data"]][[x]][[y]] %>% sapply(function(C){
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
                    stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C,". Also not a missing code.")
                  }
                  if(length(E)>0){
                    OUT<-DB$missing_codes$name[E]
                  }
                }else{
                  stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C)
                }
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }

        if(use_missing_codes){
          for(y in DB$metadata$field_name[which(DB$metadata$form_name==x&!DB$metadata$field_type%in%c("radio","dropdown","yesno","checkbox","checkbox_choice","descriptive"))]){
            z<-DB$missing_codes
            DB[["data"]][[x]][[y]]<-DB[["data"]][[x]][[y]] %>% sapply(function(C){
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
      if(x%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        if (nrow(DB[["data"]][[x]])>0){
          DB[["data"]][[x]]$redcap_repeat_instrument<-x
        }#had to add for empty instruments
      }
    }
  }
  # if(  use_missing_codes) warning("You have missing codes in your redcap. such as UNK for unknown.")
  DB$clean<-clean
  DB
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
  use_missing_codes <- !is.null(DB$missing_codes)
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
                stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
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
                stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
              }
              if(length(E)>0){
                OUT<-DB$missing_codes$code[E]
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", COL_NAME,", Choice: ",C)
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
  httr::content(
    httr::POST(
      url = DB$redcap_uri,
      body = list(
        "token"=validate_redcap_token(DB),
        content='log',
        logtype='',
        user='',
        record='',
        beginTime=x,
        endTime='',
        format='csv',
        returnFormat='csv'
      ),
      encode = "form"
    )
  ) %>% clean_redcap_log()
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
      return(NULL)
    }
  }
  if(!included){
    return(NULL)
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
  if(!file.exists(path))stop("No REDCap files found")
  x<-list.files.real(path)
  x<-x[which(gsub("\\.xlsx","",x)%in%DB$instruments$instrument_name)]
  DB_import<-DB
  DB_import[["data"]]<-list()
  for(y in x){
    DB_import[["data"]][[gsub("\\.xlsx","",y)]] <- readxl::read_xlsx(file.path(path,y))
  }
  DB_import
}

#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue.
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean data pass your data to `clean_to_raw_form(FORM,DB)` first.
#' @inheritParams save_DB
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
#' @export
upload_to_redcap<-function(to_be_uploaded,DB,batch_size=500){
    REDCapR::redcap_write(
      ds_to_write = to_be_uploaded,
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
#' @param DB_import obtained from your directory 'REDCap/upload' folder using, `DB_import <- read_redcap_dir(DB)`
#' @inheritParams upload_to_redcap
#' @return messages
#' @export
upload_DB_import_to_redcap<-function(DB_import,DB,batch_size=500){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB_import<-validate_DB(DB_import)
  DB<-validate_DB(DB)
  if(DB_import$clean){
    DB_import<-clean_to_raw_redcap(DB_import)
  }
  if(DB$clean){
    DB<-clean_to_raw_redcap(DB)
  }
  warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(any(!names(DB_import[["data"]])%in%names(DB[["data"]])))stop("All file names and data.table names from your directory a.k.a. `names(DB_import$data)` must match the DB instrument names, `DB$instruments$instrument_name`")
  if(is.null(DB_import[["data"]]))stop("`DB_import$data` is empty")
  for(TABLE in names(DB_import[["data"]])){
    ref_cols <- DB$id_col
    if(TABLE%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
      ref_cols <- c(DB$id_col,"redcap_repeat_instrument","redcap_repeat_instance")
    }
    to_be_uploaded <- find_the_diff(new= DB_import[["data"]][[TABLE]] , old =  DB[["data"]][[TABLE]], ref_cols = ref_cols)
    if(nrow(to_be_uploaded)==0){
      message(paste0("No changes -> ",TABLE))
    }
    if(nrow(to_be_uploaded)>0){
      upload_to_redcap(to_be_uploaded=to_be_uploaded,DB=DB,batch_size=batch_size)
    }
  }
}











