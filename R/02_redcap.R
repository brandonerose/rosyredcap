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

get_redcap_data<-function(DB,clean=T,records=NULL,use_missing_codes = T){
  DB$last_data_update=Sys.time()
  raw <- REDCapR::redcap_read(redcap_uri=DB$redcap_uri, token=validate_redcap_token(DB),batch_size = 2000, interbatch_delay = 0.1,records = records)$data
  DB<-DB %>% raw_process_redcap(raw = raw,clean = clean,use_missing_codes = use_missing_codes)
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

raw_process_redcap <- function(DB,raw,clean=T,use_missing_codes = T){
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
                  coded_redcap2<-which(missing_codes()$code==C)
                  if(length(coded_redcap2)>0){
                    OUT<-missing_codes()$name[coded_redcap2]
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
                  E<-which(missing_codes()$code==C)
                  if(length(E)==0){
                    stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C,". Also not a missing code.")
                  }
                  if(length(E)>0){
                    OUT<-missing_codes()$name[E]
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
                  E<-which(missing_codes()$code==C)
                  if(length(E)==0){
                    stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C,". Also not a missing code.")
                  }
                  if(length(E)>0){
                    OUT<-missing_codes()$name[E]
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
            z<-missing_codes()
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

clean_to_raw_redcap <- function(DB_import,use_missing_codes = T){
  for(x in names(DB_import[["data"]])){
    for(y in DB_import[["data"]][[x]] %>% colnames()){
      if(y%in%DB_import$metadata$field_name[which(DB_import$metadata$form_name==x&DB_import$metadata$field_type%in%c("radio","dropdown"))]){
        z<-DB_import$metadata$select_choices_or_calculations[which(DB_import$metadata$field_name==y)] %>% split_choices()
        DB_import[["data"]][[x]][[y]]<-DB_import[["data"]][[x]][[y]] %>% sapply(function(C){
          OUT<-NA
          if(!is.na(C)){
            coded_redcap<-which(z$name==C)
            if(length(coded_redcap)>0){
              OUT<-z$code[coded_redcap]
            }else{
              if(use_missing_codes){
                coded_redcap2<-which(missing_codes()$name==C)
                if(length(coded_redcap2)>0){
                  OUT<-missing_codes()$code[coded_redcap2]
                }else{
                  stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C)
                }
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C,". Also not a missing code.")
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
      if(y%in%DB_import$metadata$field_name[which(DB_import$metadata$form_name==x&DB_import$metadata$field_type=="yesno")]){
        z<-data.frame(
          code=c(0,1),
          name=c("No","Yes")
        )
        DB_import[["data"]][[x]][[y]]<-DB_import[["data"]][[x]][[y]] %>% sapply(function(C){
          OUT<-NA
          if(!is.na(C)){
            D<-which(z$name==C)
            if(length(D)>0){
              OUT<-z$code[D]
            }
            if(length(D)==0){
              if(use_missing_codes){
                E<-which(missing_codes()$name==C)
                if(length(E)==0){
                  stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C,". Also not a missing code.")
                }
                if(length(E)>0){
                  OUT<-missing_codes()$code[E]
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
        if(y%in%DB$metadata$field_name[which(DB$metadata$form_name==x&!DB$metadata$field_type%in%c("radio","dropdown","yesno"))]){
          DB[["data"]][[x]][[y]]<-DB[["data"]][[x]][[y]] %>% sapply(function(C){
            OUT<-C
            if(!is.na(C)){
              D<-which(missing_codes()$name==C)
              if(length(D)>0){
                OUT<-missing_codes()$code[D]
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }
      }
    }
  }
  DB_import$clean<-F
  DB_import
}

clean_redcap_log <- function(log){
  log$record_id <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),gsub("Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation| |[:):]|[:(:]","",A),NA)})
  comments <- which(log$action=="Manage/Design"&grepl("Add field comment|Edit field comment|Delete field comment",log$details))
  if(length(comments)>0){
    log$record_id[comments] <- stringr::str_extract(log$details[comments], "(?<=Record: )[^,]+")
    log$action_type[comments] <- "Comment"
  }
  log$action_type <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),(A %>% strsplit(" ") %>% unlist())[1],NA)})
  log
}

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

missing_codes <- function(){
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

has_redcap_token <- function(DB,silent=T){
  DB <- validate_DB(DB)
  DB$token_name %>% validate_env_name()
  DB$token_name %>% Sys.getenv() %>% is_redcap_token()
}

is_redcap_token <- function(token){
  pattern <- "^([0-9A-Fa-f]{32})(?:\\n)?$"
  token2 <- token %>% sub(pattern,"\\1", ., perl = TRUE) %>% trimws(whitespace = "[\\h\\v]")
  info_message <- paste0("You can set it each session with `Sys.setenv('",DB$token_name,"'='YoUrNevErShaReToKeN')...` or for higher safety run `usethis::edit_r_environ()` and add `",DB$token_name,"='YoUrNevErShaReToKeN'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the code, `Sys.getenv('",DB$token_name,"')` or `Sys.getenv(DB$token_name)` and see if it returns your token!...")
  if(is.null(token)){
    message("The token is `NULL`, not a valid 32-character hexademical value.")
    return(F)
  }else if (is.na(token)) {
    message("The token is `NA`, not a valid 32-character hexademical value.")
    return(F)
  }else if (nchar(token) == 0L) {
    message("`Sys.getenv(",DB$token_name,")` returned no token or is an empty string. ",info_message)
    return(F)
  }else if(token2 != token){
    message("remove whitespace or extra lines from your token.")
    return(F)
  }else if (!grepl(pattern, token, perl = TRUE)) {
    message("The token from `Sys.getenv('",DB$token_name,"')` is not a valid 32-character hexademical value.",info_message)
    return(F)
  }
  return(T)
}

#' @title Loop that ensures you have a valid token for this session
#' @param DB the object generated using `setup_DB()`
#' @return messages for confirmation. Runs a loop with `has_redcap_token(DB)` to make sure the pattern is correct
#' @export
validate_redcap_token <- function(DB,silent=T,return=T){
  while (!has_redcap_token(DB)) {
    set_redcap_token(DB)
    message("Try going to REDCap --> 'https://redcap.miami.edu/redcap_v13.1.29/API/project_api.php?pid=6317' or run `link_API_token(DB)`")
  }
  if(!silent){
    message("You have a valid token set in your session!")
  }
  if(return){
    return(Sys.getenv(DB$token_name))
  }
}

#' @title Sets a valid token for this session
#' @param DB the object generated using `setup_DB()`
#' @return messages for confirmation
#' @export
set_redcap_token <- function(DB){
  token <- readline("What is your PSDB REDCap API token: ")
  while (!is_redcap_token(token)) {
    warning("You set an invalid token. Try going to REDCap --> 'https://redcap.miami.edu/redcap_v13.1.29/API/project_api.php?pid=6317' or run `link_API_token(DB)`",immediate. = T)
    token <- readline("What is your PSDB REDCap API token: ")
  }
  args =list(args =list(token))
  names(args) = DB$token_name
  do.call(Sys.setenv, args)
  message("Token was set for this session only using `Sys.getenv('",DB$token_name,"')` <- 'TheSecretTokenYouJustEntered'")
  message("For higher safety run `usethis::edit_r_environ()` and add `",DB$token_name,"='YoUrNevErShaReToKeN'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the code, `Sys.getenv('",DB$token_name,"')` or `Sys.getenv(DB$token_name)` or `has_redcap_token(DB)`, and see if it returns your token!...'")
}

#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return messages for confirmation
#' @export
drop_redcap_dir<-function(DB,records=NULL){
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
#' @param DB list for the package that contains applications and reference files
#' @return messages for confirmation
#' @export
read_redcap_dir<-function(DB){
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

#development only!
upload_redcap<-function(DB_import,DB,unsafe=F,batch_size=500, use_missing_codes = T){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  validate_DB(DB_import)
  validate_DB(DB)
  x<-DB_import
  y<-DB
  if(x$clean){
    x<-clean_to_raw_redcap(x,use_missing_codes = use_missing_codes)
  }
  if(y$clean){
    y<-clean_to_raw_redcap(y,use_missing_codes = use_missing_codes)
  }
  if(unsafe){
    warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  }
  for(TABLE in names(x[["data"]])){
    a<-c<-x[["data"]][[TABLE]] %>% lapply(as.character) %>% as.data.frame()
    b<-y[["data"]][[TABLE]] %>% lapply(as.character) %>% as.data.frame()
    if(!unsafe){
      c<-data.frame()
      if(ncol(a)!=ncol(b)){stop("Import and DB need same columns. Did you use drop dir? Don't delete cols.")}
      if(!all(colnames(a)==colnames(b))){stop("Import and DB need same columns. Did you use drop dir? Don't delete or rearrange cols.")}
      if(TABLE %in% DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        a<-a[order(a[["redcap_repeat_instance"]]),]
        b<-b[order(b[["redcap_repeat_instance"]]),]
      }
      a<-a[order(a[[DB$id_col]]),]
      b<-b[order(b[[DB$id_col]]),]
      if(TABLE %in% DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        if(!all(b[["redcap_repeat_instance"]]==a[["redcap_repeat_instance"]]))stop("Import and DB have to same IDs. Did you use drop dir? Don't delete rows.")
      }
      if(!all(b[[DB$id_col]]==a[[DB$id_col]]))stop("Import and DB have to same IDs. Did you use drop dir? Don't delete rows.")
      kill<-NULL
      for(i in which(colnames(a)%in%c(DB$id_col,"redcap_repeat_instance","redcap_repeat_instrument"))){
        a_<-a[,i]
        a_[is.na(a_)]<-"NA"
        b_<-b[,i]
        b_[is.na(b_)]<-"NA"
        if(!all(a_==b_)){
          stop("You cannot change the following columns ... `",DB$id_col, "`, `redcap_repeat_instance`, or `redcap_repeat_instrument`")
        }
      }
      for(i in which(!colnames(a)%in%c(DB$id_col,"redcap_repeat_instance","redcap_repeat_instrument"))){
        a_<-a[,i]
        a_[is.na(a_)]<-"NA"
        b_<-b[,i]
        b_[is.na(b_)]<-"NA"
        if(all(a_==b_)){
          kill<-kill %>% append(i)
        }
      }
      if(length(kill)>0){
        a<-a[,-kill] %>% as.data.frame()
        b<-b[,-kill] %>% as.data.frame()
      }
      c<- dplyr::anti_join(a,b)
      d<- dplyr::inner_join(a,b)
      message(TABLE,": ",nrow(c)," rows have updates")
      message(TABLE,": ",nrow(d)," rows are the same")
    }
    if(nrow(c)==0){
      message(paste0("No changes -> ",TABLE))
    }
    if(nrow(c)>0){
      REDCapR::redcap_write(
        c,
        batch_size=batch_size,
        interbatch_delay=0.2,
        continue_on_error=FALSE,
        DB$redcap_uri,
        token,
        overwrite_with_blanks=TRUE
      )
    }
  }
}

link_API_token<- function(DB){
  DB$API_link %>% browseURL()
}

#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return messages for confirmation
#' @export
link_API_playground <- function(DB){
  DB$API_playground_link %>% browseURL()
}

#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return messages for confirmation
#' @export
link_REDCap_home <- function(DB){
  DB$redcap_base_link %>% browseURL()
}

#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return messages for confirmation
#' @export
link_REDCap_project <- function(DB){
  DB$home_link %>% browseURL()
}

all_possible_records <- function(DB){
  records <- NULL
  for(NAME in names(DB$data)){
    records<- records %>% append(DB$data[[NAME]][[DB$id_col]])
  }
  records %>% unique()
}

#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return messages for confirmation
#' @export
link_REDCap_record <- function(DB,record,page,instance){
  link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/DataEntry/record_home.php?pid=",DB$PID)
  if(!missing(record)){
    if(!record%in%all_possible_records(DB))stop(record," is not one of the records inside DB")
    link <- link %>% paste0("&id=",record)

  }
  if(!missing(page)){
    link <- gsub("record_home","index",link)
    if(!page%in%DB$instruments$instrument_name)stop(page," has to be one of the instrument names: ",paste0(DB$instruments$instrument_name,collapse = ", "))
    link <- link %>% paste0("&page=",page)
    if(!missing(instance)){
      if(!page%in%DB$instruments$instrument_name)stop(page," has to be one of the instrument names: ",paste0(DB$instruments$instrument_name,collapse = ", "))
      link <- link %>% paste0("&instance=",instance)
    }
  }
  link %>% browseURL()
}







