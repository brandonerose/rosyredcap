#' @title Link to get a new API token for your project (if you access)
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
link_API_token<- function(DB){
  DB$API_link %>% utils::browseURL()
}

#' @title Link view the API playground for your project (if you access)
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
link_API_playground <- function(DB){
  DB$API_playground_link %>% utils::browseURL()
}

#' @title Link view the REDCap project home page
#' @inheritParams save_DB
#' @return opens browser link
#' @export
link_REDCap_home <- function(DB){
  DB$redcap_base_link %>% utils::browseURL()
}

#' @title Shows DB in the env
#' @inheritParams save_DB
#' @return opens browser link
#' @export
link_REDCap_project <- function(DB){
  DB$home_link %>% utils::browseURL()
}

#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param record REDCap record id or study id etc, any column names that match `DB$id_col`
#' @param page REDCap page for the record. Must be one of `DB$instruments$instrument_name`
#' @param instance REDCap instance if it's a repeating instrument
#' @return opens browser link
#' @export
link_REDCap_record <- function(DB,record,page,instance){
  link <- paste0(DB$redcap_base_link,"redcap_v",DB$version,"/DataEntry/record_home.php?pid=",DB$PID)
  if(!missing(record)){
    if(!record%in%DB$all_records[[DB$id_col]])stop(record," is not one of the records inside DB")
    if("arm_num"%in%colnames(DB$all_records)){
      link <- link %>% paste0("&arm=", DB$all_records$arm_num[which(DB$all_records$participant_id==record)])
    }
    link <- link %>% paste0("&id=",record)
  }
  if(!missing(page)){
    link <- gsub("record_home","index",link)
    if(!page%in%DB$instruments$instrument_name)stop(page," has to be one of the instrument names: ",paste0(DB$instruments$instrument_name,collapse = ", "))
    link <- link %>% paste0("&page=",page)
    if(!missing(instance)){
      if(!page%in%DB$instruments$instrument_name[which(DB$instruments$repeating)])stop("If you provide an instance, it has to be one of the repeating instrument names: ",paste0(DB$instruments$instrument_name[which(DB$instruments$repeating)],collapse = ", "))
      link <- link %>% paste0("&instance=",instance)
    }
  }
  link %>% utils::browseURL()
}
