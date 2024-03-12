ignore_redcap_log <- function(collapse = T){
  ignores <- c(
    'export',
    'download ',
    'edit report',
    'Switch DAG',
    'Copy report',
    'Multi-Language',
    'File Repository ',
    'custom record dashboard',
    'User regenerate own API token',
    'Create report',
    ' external module'
  )
  if(collapse)return(paste0(ignores,collapse = "|"))
  return(ignores)
}
log_details_that_trigger_refresh <- function(){
  c(
    "Edit project field",
    "Delete project field",
    "Create project field",
    "Make project customizations",
    "Delete data collection instrument",
    "Download instrument from Shared Library",
    "Create data collection instrument",
    "Tag new identifier fields"
  )
}
#' @title Shows DB in the env
#' @param DB DB from load_DB or setup_DB
#' @param force logical for force a fresh update
#' @param day_of_log numbers of days to be checked in the log
#' @param labelled logical for whether or not to return raw or labelled REDCap. Default is TRUE.
#' @param get_files logical for whether or not to get files from redcap.
#' @param original_file_names logical for whether or not to use original file names.
#' @return messages for confirmation
#' @export
update_DB <- function(
    DB,
    force = F,
    day_of_log = 10,
    labelled = T,
    get_files = F,
    original_file_names = F
) {
  IDs <- NULL
  will_update <- T
  was_updated <- F
  DB <- validate_DB(DB)
  if(!is.null(DB$internals$data_extract_labelled)){
    if(DB$internals$data_extract_labelled!=labelled){
      if(!force){
        force <- T
        warning("The DB that was loaded was ",ifelse(DB$internals$data_extract_labelled,"labelled","RAW"), " and you chose ",ifelse(labelled,"labelled","RAW"),". Therefore, we will set force to TRUE for a full update of data to avoid data conflicts",immediate. = T)
      }
    }
  }
  DB <- test_redcap(DB)
  DB$internals$last_metadata_check <- Sys.time()
  DB$internals$last_data_check <- Sys.time()
  # DB$internals$last_metadata_update <- Sys.time()-lubridate::days(1)
  # DB$internals$last_data_update <- Sys.time()-lubridate::days(1)
  if(!force){ # check log interim
    if(is.null(DB$internals$last_metadata_update)||is.null(DB$internals$last_data_update)){
      force <- T
    }else{
      ilog <- check_redcap_log(
        DB,
        begin_time =  (DB$internals$last_data_update - lubridate::minutes(1)) %>% format( "%Y-%m-%d %H:%M") %>% as.character()
      )
      ilog$timestamp <- NULL
      ilog <- ilog %>% unique()
      ilog_metadata <- ilog[which(is.na(ilog$record)),]
      ilog_metadata <- ilog_metadata[which(ilog_metadata$details%in%log_details_that_trigger_refresh()),] #inclusion
      # ilog_metadata <- ilog_metadata[grep(ignore_redcap_log(),ilog_metadata$details,ignore.case = T,invert = T) %>% unique(),]
      if(nrow(ilog_metadata)>0){
        force <- T
        message(paste0("Update because: Metadata was changed! "))
      }else{
        ilog_data <- ilog[which(!is.na(ilog$record)),]
        deleted_records<-which(ilog_data$action_type%in%c("Delete"))
        if(length(deleted_records)>0){
          warning("There were recent records deleted from redcap. As a default, rosyredcap will not delete redcap records from the R DB object. If it was correctly deleted consider running with 'force = T'. Records: ",deleted_records %>% paste0(collapse = ", "),immediate. = T)
        }
        IDs <- ilog_data$record %>% unique()
        if(length(IDs)==0){
          IDs <- NULL
          will_update <- F
        }
      }
    }
  }
  if(will_update||force){
    if(is.null( DB$internals$data_extract_merged)){
      DB$internals$data_extract_merged <- F
    }
    was_merged <- DB$internals$data_extract_merged
    if(was_merged){
      warning(" I would prefer if you run updates on unmerged data sets. Consider saving the merge step as a final save.", immediate. = T)
      DB <- unmerge_non_repeating_DB(DB)
    }
  }
  if(force){
    DB$data_extract <- list()
    DB$data_transform <- list()
    DB$data_upload <- list()
    DB <- DB %>% get_redcap_metadata()
    DB <- DB %>% get_redcap_data(labelled = labelled)
    forms <- DB$redcap$instruments$instrument_name
    DB$redcap$log <- DB %>% check_redcap_log(last = day_of_log,units = "days") %>% unique()
    message("Full update!")
    was_updated <- T
  }else{
    if(will_update){
      time <- c(DB$internals$last_metadata_update,DB$internals$last_data_update)
      time <- time %>% min() %>% magrittr::subtract(lubridate::minutes(3)) %>% as.character()
      DB2 <- DB %>% get_redcap_data(labelled = labelled,records = IDs)
      DB2$summary$all_records <-  all_records(DB2)
      DB2 <-DB2 %>% find_DB_diff(DB)
      DB$internals$last_data_update <- DB2$internals$last_data_update
      DB$redcap$log <- DB$redcap$log %>% dplyr::bind_rows(check_redcap_log(DB,begin_time = time)) %>% unique() %>% rosyutils::all_character_cols()
      forms <- names(DB2$data_extract)[
        which(
          names(DB2$data_extract) %>%
            sapply(function(form){
              nrow(DB2$data_extract[[form]])>0
            })
        )]
      for(TABLE in forms){
        DB$data_extract[[TABLE]] <- DB$data_extract[[TABLE]][which(!DB$data_extract[[TABLE]][[DB$redcap$id_col]]%in%IDs),] %>% dplyr::bind_rows(DB2$data_extract[[TABLE]][which(DB2$data_extract[[TABLE]][[DB$redcap$id_col]]%in%IDs),])
      }
      message("updated: ",paste0(IDs,collapse = ", "))
      was_updated <- T
    }else{
      message("Up to date already!")
    }
  }
  if(get_files){
    get_redcap_files(DB,original_file_names=original_file_names)
  }
  if(was_updated){
    if(was_merged){
      DB <- merge_non_repeating_DB(DB)
    }
  }
  if(!is.null(DB$dir_path))  save_DB(DB)
  DB
}
#' @title Run Quality Checks
#' @inheritParams save_DB
#' @export
run_quality_checks <- function(DB){
  DB <- validate_DB(DB)
  if(is_something(DB$quality_checks)){
    for (qual_check in names(DB$quality_checks)){
      the_function <- DB$quality_checks[[qual_check]]
      if(is.function(the_function)){
        the_function(DB)
      }
    }
  }
}
