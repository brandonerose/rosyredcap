#' @import rosyutils
#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param records character vector of records you want dropped to your directory
#' @param allow_mod logical for whether non-instrument names are allowed
#' @param deidentify logical for deidentification
#' @param dir_other optional character string of another file path where the files should be saved
#' @param smart logical for whether to only save when data is new
#' @param include_metadata logical for whether to only include redcap and not metadata
#' @param include_other logical for whether to only include redcap and not metadata
#' @param append_name optional character string for adding to the front of file names
#' @param str_trunc_length optional integer for truncation
#' @param with_links optional logical for including links in excel sheets
#' @param merge_non_repeating optional logical for merging non-repeating instruments
#' @param forms optional character vector for only selected forms
#' @return messages for confirmation
#' @export
drop_redcap_dir <- function(DB, smart=T,include_metadata=T,include_other=T,with_links = F,forms,merge_non_repeating = T,separate = F,str_trunc_length=32000){
  DB <- validate_DB(DB)
  root_dir <- get_dir(DB)
  output_dir <- file.path(root_dir,"output")
  redcap_dir <- file.path(root_dir,"REDCap")
  redcap_metadata_dir <- file.path(redcap_dir,"metadata")
  redcap_other_dir <- file.path(redcap_dir,"other")
  redcap_upload_dir <- file.path(redcap_dir,"upload")
  due_for_save_metadata <- T
  due_for_save_data <- T
  if(smart){
    if(!is.null(DB$internals$last_metadata_dir_save)) due_for_save_metadata <- DB$internals$last_metadata_update > DB$internals$last_metadata_dir_save
    if(!is.null(DB$internals$last_data_dir_save)) due_for_save_data <- DB$internals$last_data_update > DB$internals$last_data_dir_save
  }
  redcap_dir %>% dir.create(showWarnings = F)
  redcap_metadata_dir %>% dir.create(showWarnings = F)
  redcap_other_dir %>% dir.create(showWarnings = F)
  redcap_upload_dir %>% dir.create(showWarnings = F)
  if(due_for_save_metadata){
    if(include_metadata){
      DB$internals$last_metadata_dir_save <- DB$internals$last_metadata_update
      for (x in c("project_info","metadata","instruments","codebook")){ #,"log" #taking too long
        DB$redcap[x] %>%list_to_excel(redcap_metadata_dir,file_name = x,str_trunc_length = str_trunc_length,overwrite = TRUE)
      }
    }
    if(include_other){
      for (x in c("log","users")){ #,"log" #taking too long
        DB$redcap[x] %>% list_to_excel(redcap_other_dir,file_name = x,str_trunc_length = str_trunc_length,overwrite = TRUE)
      }
    }
  }
  if(due_for_save_data){
    DB$internals$last_data_dir_save <- DB$internals$last_data_update
    if(merge_non_repeating) DB <- merge_non_repeating_DB(DB)
    # to_save <- names(DB$data_extract)
    # if(!missing(forms)){
    #   to_save <- to_save[which(to_save %in% forms)]
    # }
    # to_save_list<-DB[["data_extract"]]
    # to_save_list < to_save_list[[to_save]]
    # for(x in to_save){
    #   [x] %>%
    to_save_list <- DB[["data_extract"]]
    link_col_list <- list()
    if(with_links){
      to_save_list <-to_save_list %>% lapply(function(DF){add_redcap_links_to_DF(DF,DB)})
      link_col_list <- list(
        "redcap_link"
      )
      names(link_col_list) <- DB$redcap$id_col
    }
    names(to_save_list)
    file_name <- NULL
    if(!separate)file_name <- DB$short_name
    to_save_list %>% list_to_excel(
      dir = redcap_dir,
      separate = separate,
      link_col_list = link_col_list,
      file_name = file_name,
      str_trunc_length = str_trunc_length,
      overwrite = TRUE
    )
    # wb <- to_save_list %>% list_to_wb(
    #   link_col_list = link_col_list,
    #   str_trunc_length = str_trunc_length
    # )
    if(merge_non_repeating) DB <- unmerge_non_repeating_DB(DB)
  }
  return(DB)
}
drop_redcap_dir_old <- function(DB,records,allow_mod=T,dir_other, smart=T,include_metadata=T,include_other=F,deidentify=F,append_name,str_trunc_length=32000,with_links = T,forms,merge_non_repeating = T){
  DB <- validate_DB(DB)
  if(deidentify){
    DB <- deidentify_DB(DB) #right now not passing up option for additional non redcap marked identifiers (drop text fields)
  }
  root_dir <- get_dir(DB)
  output_dir <- file.path(root_dir,"output")
  redcap_dir <- file.path(root_dir,"REDCap")
  redcap_metadata_dir <- file.path(redcap_dir,"metadata")
  redcap_other_dir <- file.path(redcap_dir,"other")
  redcap_upload_dir <- file.path(redcap_dir,"upload")
  trigger_other <- F
  if(!missing(dir_other)){
    if(!file.exists(dir_other)){
      choice <- utils::menu(c("Yes","No"),title = "dir_other doesn't exist. Should I create?")
      if(choice==1){
        dir.create(dir_other)
      }
    }
    if(file.exists(dir_other)){
      output_dir<- redcap_other_dir <- redcap_metadata_dir <- redcap_dir <- root_dir <- dir_other
      trigger_other <- T
    }
  }
  appended_name <- ""
  if(!missing(append_name)){
    appended_name <- paste0(append_name,"_")
  }
  if(missing(records))records <-DB$summary$all_records[[DB$redcap$id_col]]
  DB[["data_extract"]] <-  DB %>% filter_DB(data_choice = "data_extract",records)
  DB[["data_transform"]] <-  DB %>% filter_DB(data_choice = "data_transform",records)
  due_for_save_metadata <- T
  due_for_save_data <- T
  if(smart){
    if(!is.null(DB$internals$last_metadata_dir_save)) due_for_save_metadata <- DB$internals$last_metadata_update > DB$internals$last_metadata_dir_save
    if(!is.null(DB$internals$last_data_dir_save)) due_for_save_data <- DB$internals$last_data_update > DB$internals$last_data_dir_save
  }
  if(!trigger_other){
    redcap_dir %>% dir.create(showWarnings = F)
    redcap_metadata_dir %>% dir.create(showWarnings = F)
    redcap_other_dir %>% dir.create(showWarnings = F)
    redcap_upload_dir %>% dir.create(showWarnings = F)
    if(due_for_save_metadata){
      if(include_metadata){
        DB$internals$last_metadata_dir_save <- DB$internals$last_metadata_update
        for (x in c("project_info","metadata","instruments","codebook")){ #,"log" #taking too long
          DB$redcap[[x]] %>% write_xl(DB,path=file.path(redcap_metadata_dir,paste0(x,".xlsx")))
        }
      }
      if(include_other){
        for (x in c("log","users")){ #,"log" #taking too long
          DB$redcap[[x]] %>% write_xl(DB,path=file.path(redcap_other_dir,paste0(x,".xlsx")))
        }
      }
    }
    if(due_for_save_data){
      DB$internals$last_data_dir_save <- DB$internals$last_data_update
      if(merge_non_repeating) DB <- merge_non_repeating_DB(DB)
      to_save <- names(DB$data_extract)
      if(!missing(forms)){
        to_save <- to_save[which(to_save %in% forms)]
      }
      for(x in to_save){
        DB[["data_extract"]][[x]] %>% write_xl(DB,path=file.path(redcap_dir,paste0(x,".xlsx")),str_trunc_length = str_trunc_length, with_links=with_links)
      }
      if(merge_non_repeating) DB <- unmerge_non_repeating_DB(DB)
    }
  }
  if(DB$data_transform %>% is_something()){
    save_it <- T
    if(!is.null(DB$internals$last_data_transformation)){
      if(smart){
        save_it <- DB$internals$last_data_transformation < DB$internals$last_data_update
      }
    }
    if(save_it){
      DB$internals$last_data_transformation <- DB$internals$last_data_update
      to_save <- names(DB$data_transform)
      if(!missing(forms)){
        to_save <- to_save[which(to_save %in% forms)]
      }
      for(x in to_save){
        DB[["data_transform"]][[x]] %>% write_xl(DB,path=file.path(output_dir,paste0(appended_name,x,".xlsx")),str_trunc_length = str_trunc_length, with_links=with_links)
      }
      if(DB$summary %>% is_something()){
        for(x in names(DB$summary)[which(DB$summary %>% lapply(is.data.frame) %>% unlist())]){
          DB[["summary"]][[x]] %>% write_xl(DB,path=file.path(output_dir,paste0(appended_name,"summary_",x,".xlsx")),str_trunc_length = str_trunc_length, with_links=with_links)
        }
      }
    }
  }
}
#' @title Reads DB from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_DB
#' @param allow_all logical TF for allowing DB$data_extract names that are not also instrument names
#' @param drop_nonredcap_vars logical TF for dropping non-redcap variable names
#' @param drop_non_instrument_vars logical TF for dropping non-instrument variable names
#' @param stop_or_warn character string of whether to stop, warn, or do nothing when forbidden cols are present
#' @return messages for confirmation
#' @export
read_redcap_dir <- function(DB,allow_all=T,drop_nonredcap_vars=T,drop_non_instrument_vars=T,stop_or_warn="warn"){
  DB <- validate_DB(DB)
  path <- file.path(get_dir(DB),"REDCap","upload")
  if(!file.exists(path))stop("No REDCap files found at path --> ",path)
  x <- list.files.real(path)
  df <- data.frame(
    file_name = x,
    file_name_no_ext = gsub("\\.xlsx|\\.xls","",x),
    match = NA
  )
  df$match <- strsplit(df$file_name_no_ext,"_") %>% sapply(function(IN){IN[length(IN)]})
  df$match[which(!df$match%in%c(DB$internals$merge_form_name,DB$redcap$instruments$instrument_name))] <- NA
  if(!allow_all){
    df <- df[which(!is.na(df$match)),]
  }
  if(DB$data_upload %>% is_something())stop("Already files in DB$data_upload, clear that first")
  DB[["data_upload"]] <- list()
  for(i in 1:nrow(df)){#not done yet
    the_file <- readxl::read_xlsx(file.path(path,df$file_name[i]),col_types = "text") %>% rosyutils::all_character_cols() # would
    drop_cols <- NULL
    if(drop_nonredcap_vars){
      x <- colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$redcap$metadata$field_name))]
      drop_cols<-drop_cols %>%
        append(x) %>%
        unique()
    }
    if(drop_non_instrument_vars){
      form <- df$match[i]
      if(form == DB$internals$merge_form_name)form <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]
      x<-colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name%in%form)]))]
      drop_cols<-drop_cols %>%
        append(x) %>%
        unique()
    }
    message1 <- paste0("forbidden cols name: ",df$file_name[i],"; ",x %>% paste0(collapse = ", "))
    if(length(x)>0){
      if(stop_or_warn=="stop") stop(message1)
      if(stop_or_warn=="warn") warning(message1,immediate. = T)
    }
    the_file <- the_file[,which(!colnames(the_file)%in%drop_cols)]
    DB[["data_upload"]][[df$match[i]]] <- the_file
  }
  DB
}
