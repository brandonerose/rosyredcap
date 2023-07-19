#' @title Shows DB in the env
#' @param DB DB from load_DB or setup_DB
#' @param force logical for force a fresh update
#' @param day_of_log numbers of days to be checked in the log
#' @param clean logical for whether or not to return raw or clean REDCap. Default is TRUE.
#' @return messages for confirmation
#' @export
update_DB<-function(DB,force=F,day_of_log = 10,clean = T){
  IDs<-NULL
  DB <- validate_DB(DB)
  if(!is.null(DB$clean)){
    if(DB$clean!=clean){
      if(!force){
        force <- T
        warning("The DB that was loaded was ",ifelse(DB$clean,"CLEAN","RAW"), " and you chose ",ifelse(clean,"CLEAN","RAW"),". Therefore, we will set force to TRUE for a full update of data to avoid data conflicts",immediate. = T)
      }
    }
  }
  DB <- test_redcap(DB)
  if(!force){
    if(is.null(DB$last_metadata_update)||is.null(DB$last_data_update)){
      force<-T
    }else{
      time<-c(DB$last_metadata_update,DB$last_data_update)
      time<-time %>% min() %>% as.character()
      ilog<-check_redcap_log(DB,begin_time = time)

      ilog3<-ilog[which(is.na(ilog$record_id)),]
      ilog3$timestamp<-NULL
      ilog3<-ilog3 %>% unique()
      ilog3<-ilog3[grep("export|download |edit report|Switch DAG|Copy report|Multi-Language|File Repository |record dashboard |User regenerate own API token",ilog3$details,ignore.case = T,invert = T) %>% unique(),]
      if(nrow(ilog3)>0){
        force<-T
        message(paste0("Update because: " ,ilog3$action, " - ", ilog3$details))
      }else{
        ilog2<-ilog[which(!is.na(ilog$record_id)),]
        ilog2$timestamp<-NULL
        ilog2<-ilog2 %>% unique()
        if(any(ilog2$action_type%in%c("Create","Delete"))){
          force<-T
        }else{
          IDs<-ilog2$record_id %>% unique()
          if(length(IDs)==0){IDs<-NULL}
        }
        # if(Sys.time()>=(DB$last_metadata_update+lubridate::days(2))){
        #   force<-T
        # }#not needed anymore because of log check?
      }
    }
  }
  if(force){
    DB<-DB %>% get_redcap_metadata()
    DB<-DB %>% get_redcap_data(clean = clean)
    DB$log<-DB %>% check_redcap_log(last = day_of_log,units = "days") %>% unique()
    message("Full update!")
    DB %>% save_DB()
  }else{
    if(!is.null(IDs)){
      time<-c(DB$last_metadata_update,DB$last_data_update)
      time<-time %>% min() %>% magrittr::subtract(lubridate::minutes(3)) %>% as.character()
      DB2<-DB %>% get_redcap_data(clean = clean,records = IDs)
      DB$last_metadata_update<-DB$last_data_update<-DB2$last_data_update
      DB$log<-DB$log %>% dplyr::bind_rows(check_redcap_log(DB,begin_time = time)) %>% unique() %>% all_character_cols()
      for(TABLE  in names(DB$data)){
        DB$data[[TABLE]]<-DB$data[[TABLE]][which(!DB$data[[TABLE]][[DB$id_col]]%in%IDs),] %>% dplyr::bind_rows(DB2$data[[TABLE]][which(DB2$data[[TABLE]][[DB2$id_col]]%in%IDs),])
      }
      message("updated: ",paste0(IDs,collapse = ", "))
      DB %>% save_DB()
    }else{
      message("Up to date already!")
    }
  }
  DB
}

