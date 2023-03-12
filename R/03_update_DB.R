#' @title Shows DB in the env
#' @param token REDCap API token
#' @param force logical for force a fresh update
#' @return messages for confirmation
#' @export
update_DB<-function(token,force=F){
  DB<-load_DB()
  IDs<-NULL
  TEST<-test_redcap(token)
  if(!TEST)warning("Invalid token or API privileges. Contact Admin!",immediate. = T)
  if(TEST){
    if(!force){
      if(is.null(DB$last_metadata_update)||is.null(DB$last_data_update)){
        force<-T
      }else{
        time<-c(DB$last_metadata_update,DB$last_data_update)
        time<-time %>% min() %>% as.character()
        ilog<-check_redcap_log(token,begin_time = time)

        ilog3<-ilog[which(is.na(ilog$record_id)),]
        ilog3$timestamp<-NULL
        ilog3<-ilog3 %>% unique()
        ilog3<-ilog3[grep("export|download |edit report|Switch DAG|Copy report|Multi-Language",ilog3$details,ignore.case = T,invert = T) %>% unique(),]
        if(nrow(ilog3)>0){
          force<-T
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
      DB<-DB %>% get_redcap_metadata(token)
      DB<-DB %>% get_redcap_data(token)
      DB$log<-check_redcap_log(token,last = 7,units = "days") %>% unique()
      message("Full update!")
      DB %>% save_DB()
    }else{
      if(!is.null(IDs)){
        time<-c(DB$last_metadata_update,DB$last_data_update)
        time<-time %>% min() %>% magrittr::subtract(lubridate::minutes(3)) %>% as.character()
        DB2<-DB %>% get_redcap_data(token,records = IDs)
        DB$last_metadata_update<-DB$last_data_update<-DB2$last_data_update
        DB$log<-DB$log %>% dplyr::bind_rows(check_redcap_log(token,begin_time = time)) %>% unique() %>% nrow()
        for(TABLE  in names(DB$data)){
          DB$data[[TABLE]]<-DB$data[[TABLE]][which(!DB$data[[TABLE]][[DB$id_col]]%in%IDs),] %>% dplyr::bind_rows(DB2$data[[TABLE]][which(DB2$data[[TABLE]][[DB2$id_col]]%in%IDs),])
        }
        message("updated: ",paste0(IDs,collapse = ", "))
        DB %>% save_DB()
      }else{
        message("Up to date already!")
      }
    }
  }
  DB
}














