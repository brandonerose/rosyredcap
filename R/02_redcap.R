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
  raw <- REDCapR::redcap_read(redcap_uri=redcap_uri(), token=token,batch_size = 2000, interbatch_delay = 0.1,records = records)$data
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
        if("redcap_repeat_instrument" %in% colnames(raw)){
          DB[["data"]][[x]]<-raw[which(is.na(raw$redcap_repeat_instrument)),unique(c(DB$id_col,DB$metadata$field_name[which(DB$metadata$form_name==x)]))]
        }else{
          DB[["data"]][[x]]<-raw[,unique(c(DB$id_col,DB$metadata$field_name[which(DB$metadata$form_name==x)]))]
        }
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
        for (y in DB$metadata$field_name[which(DB$metadata$form_name==x&DB$metadata$field_type=="yesno")]){
          z<-data.frame(
            code=c(0,1),
            name=c("No","Yes")
          )
          DB[["data"]][[x]][[y]]<-DB[["data"]][[x]][[y]] %>% sapply(function(C){
            OUT<-NA
            if(!is.na(C)){
              D<-which(z$code==C)
              if(length(D)==0){
                print(z)
                stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C)
              }
              OUT<-z$name[D]
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
    for (y in DB_import$metadata$field_name[which(DB_import$metadata$form_name==x&DB_import$metadata$field_type=="radio")]){
      z<-DB_import$metadata$select_choices_or_calculations[which(DB_import$metadata$field_name==y)] %>% split_choices()
      DB_import[["data"]][[x]][[y]]<-DB_import[["data"]][[x]][[y]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          OUT<-z$code[which(z$name==C)]
        }
        OUT
      }) %>% unlist()
    }
    for (y in DB_import$metadata$field_name[which(DB_import$metadata$form_name==x&DB_import$metadata$field_type=="yesno")]){
      z<-data.frame(
        code=c(0,1),
        name=c("No","Yes")
      )
      DB_import[["data"]][[x]][[y]]<-DB_import[["data"]][[x]][[y]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          D<-which(z$name==C)
          if(length(D)==0){
            print(z)
            stop("Mismatch in choices compared to REDCap (above)! Table: ",x,", Column: ", y,", Choice: ",C)
          }
          OUT<-z$code[D]
        }
        OUT
      }) %>% unlist()
    }
  }
  DB_import$clean<-F
  DB_import
}

clean_redcap_log <- function(log){
  log$record_id <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),gsub("Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation| |[:):]|[:(:]","",A),NA)})
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
  dir.create(file.path(get_dir(),"REDCap","other"),showWarnings = F)
  dir.create(file.path(get_dir(),"REDCap","upload"),showWarnings = F)
  DB_selected<- DB %>% select_redcap_records(records)
  for(x in DB$instruments$instrument_name){
    DB_selected[["data"]][[x]] %>% write_xl(DB,path=file.path(get_dir(),"REDCap",paste0(x,".xlsx")))
  }
  for (x in c("metadata","instruments","users")){ #,"log" #taking too long
    DB_selected[[x]] %>% write_xl(DB,path=file.path(get_dir(),"REDCap","other",paste0(x,".xlsx")))
  }
}


read_redcap_dir<-function(DB){
  path<-file.path(get_dir(),"REDCap","upload")
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
upload_redcap<-function(DB_import,DB,token,unsafe=F,batch_size=500){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  validate_DB(DB_import)
  validate_DB(DB)
  x<-DB_import
  y<-DB
  if(x$clean){
    x<-clean_to_raw_redcap(x)
  }
  if(y$clean){
    y<-clean_to_raw_redcap(y)
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
        redcap_uri(),
        token,
        overwrite_with_blanks=TRUE
      )
    }
  }
}

