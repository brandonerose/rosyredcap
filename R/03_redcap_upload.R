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
    if(count_DB_cells(DB)>5000){
      stop <-utils::menu(choices = c("YES - I want to stop and double check what I'm about to upload","NO - Move forward with larger upload"),title = "This is a large upload. Do you want to stop and double check it first?")
      if(stop==1)stop("Double check DB object prior to upload")
    }
  }
  warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(is.null(DB[["data"]]))stop("`DB$data` is empty")
  for(TABLE in names(DB[["data"]])){
    to_be_uploaded_raw <- DB[["data"]][[TABLE]]
    if(nrow(to_be_uploaded_raw)>0){
      if(DB$clean){
        to_be_uploaded_clean <- to_be_uploaded_raw
        to_be_uploaded_raw <- to_be_uploaded_clean %>% clean_to_raw_form(DB)
      }
      do_it <- 1
      if(ask){
        if(DB$clean){
          print("Clean Data")
          print.data.frame(to_be_uploaded_clean%>% utils::head(n=40))
        }
        print("Raw Data")
        print.data.frame(to_be_uploaded_raw %>% utils::head(n=40))
        do_it <-utils::menu(choices = c("Yes upload","No and go to next"),title = "Do you want to upload this?")
      }
      if(do_it==1){
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
  if(is.null(DB_import[["data"]]))stop("`DB_import$data` is empty")
  for(TABLE in names(DB_import[["data"]])){
    ref_cols <- DB$id_col
    if(TABLE%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]){
      ref_cols <- c(DB$id_col,"redcap_repeat_instrument","redcap_repeat_instance")
    }
    DB_import[["data"]][[TABLE]] <- find_df_diff(new= DB_import[["data"]][[TABLE]] , old =  DB[["data"]][[TABLE]], ref_cols = ref_cols)
  }
  DB_import
}
