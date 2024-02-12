#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param records character vector of records you want dropped to your directory
#' @param allow_mod logical for whether non-instrument names are allowed
#' @param deidentify logical for deidentification
#' @param dir_other optional character string of another file path where the files should be saved
#' @param only_redcap logical for whether to only include redcap and not metadata
#' @param append_name optional character string for adding to the front of file names
#' @param str_trunc_length optional integer for truncation
#' @param annotated_codebook optional logical
#' @param forms optional character vector for only selected forms
#' @return messages for confirmation
#' @export
drop_redcap_dir<-function(DB,records=NULL,allow_mod=T,dir_other,only_redcap=F,deidentify=F,append_name,str_trunc_length=32000,with_links = T,annotated_codebook=T,forms){
  DB <- validate_DB(DB)
  if(deidentify){
    DB <- deidentify_DB(DB) #right now not passing up option for additional non redcap marked identifiers
  }
  root_dir <- get_dir(DB)
  sub_dir <- file.path(root_dir,"REDCap")
  sub_dir2 <- file.path(root_dir,"REDCap","other")
  trigger_other <- F
  if(!missing(dir_other)){
    if(file.exists(dir_other)){
      sub_dir2 <- sub_dir <- root_dir <- dir_other
      trigger_other <- T
    }
  }
  appended_name <- ""
  if(!missing(append_name)){
    appended_name <- paste0(append_name,"_")
  }
  if(!trigger_other){
    dir.create(file.path(root_dir,"REDCap"),showWarnings = F)
    dir.create(file.path(root_dir,"REDCap","other"),showWarnings = F)
    dir.create(file.path(root_dir,"REDCap","upload"),showWarnings = F)
  }

  DB_selected<- DB %>% select_redcap_records(records)
  if(allow_mod){
    to_save <- names(DB$data)
  }else{
    to_save <- DB$instruments$instrument_name
  }
  if(!missing(forms)){
    to_save <- to_save[which(to_save %in% forms)]
  }
  for(x in to_save){
    DB_selected[["data"]][[x]] %>% write_xl(DB,path=file.path(sub_dir,paste0(appended_name,x,".xlsx")),str_trunc_length = str_trunc_length, with_links=with_links)
  }
  if(!only_redcap){
    for (x in c("metadata","instruments","users","codebook")){ #,"log" #taking too long
      DB_selected[[x]] %>% write_xl(DB,path=file.path(sub_dir2,paste0(appended_name,x,".xlsx")))
    }
  }
  if(annotated_codebook){
    DB_selected[["codebook"]] %>% write_xl(DB,path=file.path(sub_dir2,paste0(appended_name,"annotated_codebook.xlsx")))
  }
}

#' @title Reads DB from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_DB
#' @param allow_all logical TF for allowing DB$data names that are not also instrument names
#' @return messages for confirmation
#' @export
read_redcap_dir<-function(DB,allow_all=T){
  DB <- validate_DB(DB)
  path<-file.path(get_dir(DB),"REDCap","upload")
  if(!file.exists(path))stop("No REDCap files found at path --> ",path)
  x<-list.files.real(path)
  if(!allow_all){
    x<-x[which(gsub("\\.xlsx|\\.xls","",x)%in%DB$instruments$instrument_name)]
  }
  DB_import<-DB
  DB_import[["data"]]<-list()
  for(y in x){#not done yet
    DB_import[["data"]][[gsub("\\.xlsx","",y)]] <- readxl::read_xlsx(file.path(path,y)) %>% all_character_cols()
  }
  DB_import
}
