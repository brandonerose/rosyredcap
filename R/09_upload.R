
#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue. Missing rows and columns are fine!
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean data pass your data to `labelled_to_raw_form(FORM,DB)` first.
#' @inheritParams save_DB
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
#' @export
upload_form_to_redcap <- function(to_be_uploaded,DB,batch_size=500){
  REDCapR::redcap_write(
    ds_to_write = to_be_uploaded %>% all_character_cols(),
    batch_size=batch_size,
    interbatch_delay=0.2,
    continue_on_error=FALSE,
    redcap_uri = DB$links$redcap_uri,
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
upload_DB_to_redcap <- function(DB,batch_size=500,ask=T){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB <- validate_DB(DB)
  if(ask){
    if(count_DB_upload_cells(DB)>5000){
      stop  <- utils::menu(choices = c("YES - I want to stop and double check what I'm about to upload","NO - Move forward with larger upload"),title = "This is a large upload. Do you want to stop and double check it first?")
      if(stop==1)stop("Double check DB object prior to upload")
    }
  }
  warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(!is_something(DB$data_upload))stop("`DB$data_extract` is empty")
  for(TABLE in names(DB$data_upload)){
    to_be_uploaded_raw <- DB$data_upload[[TABLE]]
    if(nrow(to_be_uploaded_raw)>0){
      if(DB$internals$data_extract_labelled){
        to_be_uploaded_clean <- to_be_uploaded_raw
        to_be_uploaded_raw <- to_be_uploaded_clean %>% labelled_to_raw_form(DB)
      }
      do_it <- 1
      if(ask){
        if(DB$internals$data_extract_labelled){
          print("Clean Data")
          print.data.frame(to_be_uploaded_clean%>% utils::head(n=40))
        }
        print("Raw Data")
        print.data.frame(to_be_uploaded_raw %>% utils::head(n=40))
        do_it  <- utils::menu(choices = c("Yes upload","No and go to next"),title = "Do you want to upload this?")
      }
      if(do_it==1){
        to_be_uploaded_raw$arm_num <- NULL
        to_be_uploaded_raw$event_name <- NULL
        upload_form_to_redcap(to_be_uploaded=to_be_uploaded_raw,DB=DB,batch_size=batch_size)
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
#' @inheritParams upload_form_to_redcap
#' @param DB2 The comparison DB object
#' @param ignore_instruments character vector of instruments to be ignored if you know they are unchanged.
#' @return DB_import but only the differences
#' @export
find_DB_diff <- function(DB2,DB,ignore_instruments){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB <- validate_DB(DB)
  DB2 <- validate_DB(DB2)
  DB$data_extract <- filter_DB(DB, records = DB2$summary$all_records[[DB$redcap$id_col]])
  if(!missing(ignore_instruments)){
    if(any(!ignore_instruments%in%names(DB2[["data_extract"]])))stop("ignore_instruments must be included in the set of instrument names, `names(DB2$data_extract)`")
    for(DROP in ignore_instruments){
      DB2$data_extract[[DROP]] <- NULL
    }
  }
  # if(DB2$internals$data_extract_labelled&!DB$internals$data_extract_labelled){
  #   DB2 <- labelled_to_raw_DB(DB2)
  # }
  # warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(any(!names(DB2[["data_extract"]])%in%names(DB$data_extract)))stop("All file names and data.table names from your directory a.k.a. `names(DB2$data_extract)` must match the DB instrument names, `DB$redcap$instruments$instrument_name`")
  if(is.null(DB2[["data_extract"]]))stop("`DB2$data_extract` is empty")
  for(TABLE in names(DB2[["data_extract"]])){#TABLE <- names(DB2[["data_extract"]]) %>% sample(1)
    new <-  DB2[["data_extract"]][[TABLE]]
    old <-  DB$data_extract[[TABLE]]
    ref_cols <- DB$redcap$raw_structure_cols
    ref_cols <- ref_cols[which(ref_cols%in%c(colnames(old),colnames(new)))]
    DB2[["data_extract"]][[TABLE]] <- rosyutils::find_df_diff(new= new , old =  old, ref_cols = ref_cols, message_pass = paste0(TABLE,": "))
  }
  DB2
}
find_upload_diff <- function(DB,compare = "data_upload", to = "data_transform"){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB <- validate_DB(DB)
  upload_list <- DB[[compare]]
  old_list <- DB[[to]]
  if(any(!names(upload_list)%in%names(old_list)))stop("All file names and data.table names from your directory a.k.a. `names(DB2$data_extract)` must match the DB instrument names, `DB$redcap$instruments$instrument_name`")
  if(is.null(upload_list))stop("`DB2$data_extract` is empty")
  for(TABLE in names(upload_list)){#TABLE <- names(upload_list) %>% sample(1)
    new <-  upload_list[[TABLE]]
    old <-  old_list[[TABLE]]
    ref_cols <- DB$redcap$raw_structure_cols
    ref_cols <- ref_cols[which(ref_cols%in%c(colnames(old),colnames(new)))]
    upload_list[[TABLE]] <- rosyutils::find_df_diff(new= new , old =  old, ref_cols = ref_cols, message_pass = paste0(TABLE,": "))
  }
  return(upload_list)
}

