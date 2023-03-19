redcap_uri<-function(){
  paste0(redcap_link,"api/")
}

get_redcap_metadata<-function(DB,token){
  DB$last_metadata_update=Sys.time()
  RC_proj<-httr::content(
    httr::POST(
      redcap_uri(),
      body = list(
        "token"=token,
        content='project',
        format='csv',
        returnFormat='json'
      ),
      encode = "form"
    )
  )
  DB$title=RC_proj$project_title
  DB$PID=RC_proj$project_id
  DB$metadata=REDCapR::redcap_metadata_read(redcap_uri=redcap_uri(), token=token)$data
  DB$metadata<-DB$metadata %>%dplyr::bind_rows(
    data.frame(
      field_name=paste0(unique(DB$metadata$form_name),"_complete"),form_name=unique(DB$metadata$form_name),field_type="radio",select_choices_or_calculations="0, Incomplete | 1, Unverified | 2, Complete"
    )
  )
  DB$id_col<-DB$metadata[1,1] %>% as.character() #RISKY?

  DB$instruments=REDCapR::redcap_instruments(redcap_uri=redcap_uri(), token=token)$data
  repeating<-httr::content(
    httr::POST(
      redcap_uri(),
      body = list(
        "token"=token,
        content='repeatingFormsEvents',
        format='csv',
        returnFormat='json'
      ),
      encode = "form"
    )
  )$form_name
  DB$instruments$repeating <- DB$instruments$instrument_name%in%repeating
  DB$users<-get_redcap_users(token)
  DB$version=paste0(unlist(REDCapR::redcap_version(redcap_uri=redcap_uri(), token=token)),collapse = ".")
  DB$log<-check_redcap_log(token,last = 2,units = "mins")
  DB$users$current_user<-DB$users$username==DB$log$username[which(DB$log$details=="Export REDCap version (API)") %>% dplyr::first()]
  DB$home_link <- paste0(redcap_link,"redcap_v",DB$version,"/index.php?pid=",DB$PID)
  DB$records_link <- paste0(redcap_link,"redcap_v",DB$version,"/DataEntry/record_status_dashboard.php?pid=",DB$PID)
  DB$API_link <- paste0(redcap_link,"redcap_v",DB$version,"/API/project_api.php?pid=",DB$PID)
  DB$API_playground_link <- paste0(redcap_link,"redcap_v",DB$version,"/API/playground.php?pid=",DB$PID)
  DB
}

get_redcap_data<-function(DB,token,clean=T,records=NULL){
  DB$last_data_update=Sys.time()
  raw=REDCapR::redcap_read(redcap_uri=redcap_uri(), token=token,batch_size = 2000, interbatch_delay = 0.1,records = records)$data
  raw$redcap_repeat_instrument[which(is.na(raw$redcap_repeat_instrument))]<-"patient"
  DB<-DB %>% raw_process_redcap(raw,clean)
  DB
}

select_redcap_records<-function(DB, records=NULL){
  DB_selected<-DB
  DB_selected$data<-list()
  if(!is.null(records)){
    BAD <-records[which(!records%in%DB$data$patient[[DB$id_col]])]
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
  if(nrow(raw)>0){
    for(x in DB$instruments$instrument_name){
      if(x%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        DB[["data"]][[x]]<-raw[which(raw$redcap_repeat_instrument==x),unique(c(DB$id_col,"redcap_repeat_instance",DB$metadata$field_name[which(DB$metadata$form_name==x)]))]
      }
      if(!x%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        DB[["data"]][[x]]<-raw[which(raw$redcap_repeat_instrument==x),unique(c(DB$id_col,DB$metadata$field_name[which(DB$metadata$form_name==x)]))]
      }
      for(COL in colnames(DB[["data"]][[x]])){
        DB[["data"]][[x]][[COL]]<-DB[["data"]][[x]][[COL]] %>% as.character()
      }
      if(clean){
        for (y in DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_type=="radio")]){
          z<-DB$metadata$select_choices_or_calculations[which(DB$metadata$field_name==y)] %>% split_choices()
          DB[["data"]][[x]][[y]]<-DB[["data"]][[x]][[y]] %>% sapply(function(C){
            OUT<-NA
            if(!is.na(C)){
              OUT<-z$name[which(z$code==C)]
            }
            OUT
          }) %>% unlist()

        }
      }
      if(x%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
        if (nrow(DB[["data"]][[x]])>0){
          DB[["data"]][[x]]$redcap_repeat_instrument<-x
        }#had to add for empty instruments
      }
    }
  }
  DB$clean<-clean
  DB
}

clean_to_raw_redcap <- function(DB_import){
  for(x in names(DB_import[["data"]])){
    for (y in DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_type=="radio")]){
      z<-DB$metadata$select_choices_or_calculations[which(DB$metadata$field_name==y)] %>% split_choices()
      DB_import[["data"]][[x]][[y]]<-DB_import[["data"]][[x]][[y]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          OUT<-z$code[which(z$name==C)]
        }
        OUT
      }) %>% unlist()
    }
  }
  DB_import$clean<-F
  DB_import
}

clean_redcap_log <- function(log){
  log$record_id <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),gsub("Update record|Delete record|Create record|[:(:]|API[:):]|Auto|calculation| ","",A),NA)})
  log$action_type <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),(A %>% strsplit(" ") %>% unlist())[1],NA)})
  log
}

check_redcap_log <- function(token,last=24,units="hours",begin_time=""){
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
      redcap_uri(),
      body = list(
        "token"=token,
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

test_redcap<-function(token){
  x<-httr::http_error(
    httr::POST(
      redcap_uri(),
      body = list(
        "token"=token,
        content='project',
        format='csv',
        returnFormat='json'
      ),
      encode = "form"
    )
  )
  !x
}

get_redcap_users<-function(token){
  merge(
    merge(
      httr::content(
        httr::POST(
          redcap_uri(),
          body = list(
            "token"=token,
            content='userRole',
            format='csv',
            returnFormat='json'
          ), encode = "form")
      ) %>% dplyr::select("unique_role_name","role_label"),
      httr::content(
        httr::POST(
          redcap_uri(),
          body = list(
            "token"=token,
            content='userRoleMapping',
            format='csv',
            returnFormat='json'
          ), encode = "form")
      ),
      by="unique_role_name"),
    httr::content(
      httr::POST(
        redcap_uri(),
        body = list(
          "token"=token,
          content='user',
          format='csv',
          returnFormat='json'
        ), encode = "form")
    ),
    by="username"
  )
}


#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return messages for confirmation
#' @export
drop_redcap_dir<-function(DB,records=NULL){
  dir.create(file.path(get_dir(),"REDCap"),showWarnings = F)
  DB_selected<- DB %>% select_redcap_records(records)
  for(x in DB$instruments$instrument_name){
    DB_selected[["data"]][[x]] %>% write_xl(DB,path=file.path(get_dir(),"REDCap",paste0(x,".xlsx")))
  }
}





