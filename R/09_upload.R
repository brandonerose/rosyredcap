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
    ds_to_write = to_be_uploaded %>% rosyutils::all_character_cols(),
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
#' @title Find the DB_import and DB differences
#' @inheritParams save_DB
#' @param compare what data_choice to be compare
#' @param to what data_choice to be compared to
#' @return upload_list
#' @export
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
#' @export
check_field <- function(DB,DF, field_name,autofill_new=T){
  form <- rosyredcap:::field_names_to_instruments(DB,field_name)
  records <- DF[[DB$redcap$id_col]] %>% unique()
  BAD<-records[which(!records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  if(length(BAD)>0)stop("Records not included in DB: ",records %>% paste0(collapse = ", "))
  # is_repeating <- form%in% DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
  cols_mandatory_structure <- DB$redcap$raw_structure_cols
  cols_mandatory <- c(cols_mandatory_structure,field_name)
  old <- DB$data_extract[[form]][,cols_mandatory]
  cols_mandatory_structure <- cols_mandatory_structure[which(cols_mandatory_structure %in% colnames(old))]
  old <- old[which(old[[DB$redcap$id_col]]%in% records),]
  new <- DF
  missing_structure_cols<-cols_mandatory_structure[which(!cols_mandatory_structure%in%colnames(new))]
  cols <- cols_mandatory[which(cols_mandatory %in% colnames(new))]
  new <- new[,cols]
  included_records <- records[which(records %in% old[[DB$redcap$id_col]])]
  if(length(missing_structure_cols)>0){
    included_records_many_rows <- included_records[which(included_records %>% sapply(function(ID){
      length(which(old[[DB$redcap$id_col]]==ID))>1
    }))]
    if(length(included_records_many_rows)>0)stop("DF is missing structural columns (",missing_structure_cols %>% paste0(collapse = ", "),") and has ",form," rows with multiple entries... remove them or add the intended columns: ",included_records_many_rows %>% paste0(collapse = ", "))
    if("redcap_repeat_instrument"%in%missing_structure_cols)new$redcap_repeat_instrument<- form
    if("redcap_repeat_instance"%in%missing_structure_cols){
      new$redcap_repeat_instance<- new[[DB$redcap$id_col]] %>% sapply(function(ID){
        if(ID %in% included_records)return(old$redcap_repeat_instance[which(old[[DB$redcap$id_col]]==ID)])
        return("1")
      })
    }
    #add event?
  }
  z<- new %>% rosyutils::find_df_diff(old,ref_cols = cols_mandatory_structure)
  if(!is.null(z)){
    i_of_old_name_change <- which(!colnames(old)%in% cols_mandatory_structure)
    colnames(old)[i_of_old_name_change] <- paste0(colnames(old)[i_of_old_name_change],"_old")
    z_old <- z %>% merge(old,by =cols_mandatory_structure)
    # add autoallow NA
    if(nrow(z)>0){
      # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
      choices <- c("upload new","keep old","manual entry","launch redcap link only")
      for ( i in 1:nrow(z)){
        OUT <- z[i,]
        IN<-z_old[i,]
        new_answer <- IN[[field_name]]
        old_answer <- IN[[paste0(field_name,"_old")]]
        ask <- T
        if(autofill_new){
          if(is.na(old_answer)&&!is.na(new_answer)){
            ask <- F
          }
        }
        if(ask){
          print.data.frame(z_old[i,])
          choice <- utils::menu(choices,title=paste0("What would you like to do?"))
        }else{
          choice <- 1
        }
        if(choice==1){
          OUT %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
          message("Uploaded: ",OUT %>% paste0(collapse = " | "))
        }
        if(choice==2){
          message("Did not change anything")
        }
        if(choice==3){
          DB %>% rosyredcap:::link_REDCap_record(OUT[[DB$redcap$id_col]])
          OUT[[field_name]] <- readline("What would you like it to be? ")
          print.data.frame(OUT)
          OUT %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
        }
        if(choice==4){#account for repeat? instance
          DB %>% rosyredcap:::link_REDCap_record(OUT[[DB$redcap$id_col]],form,instance = OUT[["redcap_repeat_instance"]])
        }
      }
    }
  }
}
#' @export
edit_redcap_while_viewing <- function(DB,records, field_name_to_change, field_names_to_view=NULL,optional_DF,upload_individually = T){
  form <- rosyredcap:::field_names_to_instruments(DB,field_name_to_change)
  if(missing(records))records <- DB$summary$all_records[[DB$redcap$id_col]]
  BAD<-records[which(!records%in%DB$summary$all_records[[DB$redcap$id_col]])]
  if(length(BAD)>0)stop("Records not included in DB: ",records %>% paste0(collapse = ", "))
  if(length(records)==0)return(message("records are length zero"))
  records <- records %>% unique()
  # form2 <- rosyredcap:::field_names_to_instruments(DB,field_names_to_view)
  field_names_to_view <- c(field_name_to_change,field_names_to_view) %>% unique()
  old_list <-filter_DB(DB,records = records, field_names = field_names_to_view)
  old_ref <- old_list[[form]]
  old_list[[form]] <- NULL
  supplement <- NULL
  if(length(old_list)>0){
    supplement <- old_ref
    for(i in 1:length(old_list)){
      supplement <- supplement %>% merge( old_list[[i]],by =  DB$redcap$id_col)
    }
    if(!missing(optional_DF)){
      matching_optional_cols<-colnames(optional_DF)[which(colnames(optional_DF)%in%DB$redcap$raw_structure_cols)]
      supplement <- supplement %>% merge( optional_DF,by =  matching_optional_cols)
    }
  }
  if(nrow(old_ref)>0){
    # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
    rows_of_choices <- which(DB$redcap$codebook$field_name==field_name_to_change)
    has_choices <- length(rows_of_choices)>0
    use_sup <- !is.null(supplement)
    if(has_choices){
      choices <- c(DB$redcap$codebook$name[rows_of_choices],"Do Nothing","Launch Redcap Link Only")
    }else{
      choices <- c("Manual Entry","Do Nothing","Launch Redcap Link Only")
    }
    for ( i in 1:nrow(old_ref)){
      VIEW  <- OUT <- old_ref[i,]
      if(use_sup)VIEW <-supplement[which(supplement[[DB$redcap$id_col]]==OUT[[DB$redcap$id_col]]),]
      old_answer <- OUT[[field_name_to_change]] %>% as.character()
      print.data.frame(VIEW)
      choice <- utils::menu(choices,title=paste0("What would you like to do?"))
      choice <- choices[choice]
      changed <- F
      if(choice %in% c("Manual Entry","Do Nothing","Launch Redcap Link Only")){
        if(choice=="Manual Entry"){
          OUT[[field_name_to_change]] <- readline("What would you like it to be? ")
          changed <- T
        }
        if(choice=="Do Nothing"){
          message("Did not change anything")
        }
        if(choice=="Launch Redcap Link Only"){#account for repeat? instance
          DB %>% rosyredcap:::link_REDCap_record(OUT[[DB$redcap$id_col]],form,instance = OUT[["redcap_repeat_instance"]])
        }
      }else{
        OUT[[field_name_to_change]] <- choice
        if(upload_individually){
          OUT %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
          message("Uploaded: ",OUT %>% paste0(collapse = " | "))
          changed <- T
        }
      }
      if(changed)old_ref[i,] <- OUT
    }
    if(!upload_individually)old_ref %>% rosyredcap::labelled_to_raw_form(DB) %>% upload_form_to_redcap(DB)
  }
}
