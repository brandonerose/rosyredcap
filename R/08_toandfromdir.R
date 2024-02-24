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
#' @param annotated_codebook optional logical for adding annotations of n and percent to codebook file
#' @param with_links optional logical for including links in excel sheets
#' @param forms optional character vector for only selected forms
#' @return messages for confirmation
#' @export
drop_redcap_dir <- function(DB,records=NULL,allow_mod=T,dir_other, smart=T,include_metadata=T,include_other=F,deidentify=F,append_name,str_trunc_length=32000,with_links = T,annotated_codebook=T,forms){
  DB <- validate_DB(DB)
  if(deidentify){
    DB <- deidentify_DB(DB) #right now not passing up option for additional non redcap marked identifiers
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
  if(!trigger_other){
    redcap_dir %>% dir.create(showWarnings = F)
    redcap_metadata_dir %>% dir.create(showWarnings = F)
    redcap_other_dir %>% dir.create(showWarnings = F)
    redcap_upload_dir %>% dir.create(showWarnings = F)
  }
  DB_selected <-  DB %>% select_redcap_records(records)
  if(allow_mod){
    to_save <- names(DB$data_extract)
  }else{
    to_save <- DB$redcap$instruments$instrument_name
  }
  if(!missing(forms)){
    to_save <- to_save[which(to_save %in% forms)]
  }
  due_for_save_metadata <- T
  due_for_save_data <- T
  if(smart){
    if(!is.null(DB$internals$last_metadata_dir_save)) due_for_save_metadata <- DB$internals$last_metadata_update > DB$internals$last_metadata_dir_save
    if(!is.null(DB$internals$last_data_dir_save)) due_for_save_data <- DB$internals$last_data_update > DB$internals$last_data_dir_save
  }
  if(due_for_save_metadata){
    if(include_metadata){
      DB$internals$last_metadata_dir_save <- DB$internals$last_metadata_update
      for (x in c("project_info","metadata","instruments","codebook")){ #,"log" #taking too long
        DB_selected$redcap[[x]] %>% write_xl(DB,path=file.path(redcap_metadata_dir,paste0(appended_name,x,".xlsx")))
      }
    }
    if(include_other){
      for (x in c("log","users")){ #,"log" #taking too long
        DB_selected$redcap[[x]] %>% write_xl(DB,path=file.path(redcap_other_dir,paste0(appended_name,x,".xlsx")))
      }
    }
  }
  if(due_for_save_data){
    DB$internals$last_data_dir_save <- DB$internals$last_data_update
    for(x in to_save){
      DB_selected[["data_extract"]][[x]] %>% write_xl(DB,path=file.path(redcap_dir,paste0(appended_name,x,".xlsx")),str_trunc_length = str_trunc_length, with_links=with_links)
    }
  }
  # if(annotated_codebook){
  #   DB_selected$redcap[[x]] %>% write_xl(DB,path=file.path(redcap_other_dir,paste0(appended_name,x,".xlsx")))
  # }
  if(DB$data_transform %>% is_something()){
    save_it <- T
    if(!is.null(DB$internals$last_data_transformation)){
      if(smart){
        save_it <- DB$internals$last_data_transformation < DB$internals$last_data_update
      }
    }
    if(save_it){
      DB$internals$last_data_transformation <- DB$internals$last_data_update
      for(x in names(DB$data_transform)){
        DB_selected[["data_transform"]][[x]] %>% write_xl(DB,path=file.path(output_dir,paste0(appended_name,x,".xlsx")),str_trunc_length = str_trunc_length, with_links=with_links)
      }
    }
  }
  return(DB)
}
#' @title Reads DB from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_DB
#' @param allow_all logical TF for allowing DB$data_extract names that are not also instrument names
#' @param allow_nonredcap_vars logical TF for allowing non-redcap variable names
#' @return messages for confirmation
#' @export
read_redcap_dir <- function(DB,allow_all=T,allow_nonredcap_vars=F){
  DB <- validate_DB(DB)
  path <- file.path(get_dir(DB),"REDCap","upload")
  if(!file.exists(path))stop("No REDCap files found at path --> ",path)
  x <- list.files.real(path)
  if(!allow_all){
    x <- x[which(gsub("\\.xlsx|\\.xls","",x)%in%DB$redcap$instruments$instrument_name)]
  }
  if(DB$data_upload %>% is_something())stop("Already files in DB$data_upload, clear that first")
  DB[["data_upload"]] <- list()
  for(y in x){#not done yet
    the_file <- readxl::read_xlsx(file.path(path,y)) %>% all_character_cols()
    if(!allow_nonredcap_vars){
      x<-colnames(the_file)[which(!colnames(the_file)%in%c(DB$redcap$raw_structure_cols,DB$redcap$metadata$field_name))]
      if(length(x)>0)stop("forbidden col name: ",x %>% paste0(collapse = ", "))
    }
    DB[["data_upload"]][[gsub("\\.xlsx","",y)]] <- readxl::read_xlsx(file.path(path,y)) %>% all_character_cols()
  }
  DB
}
