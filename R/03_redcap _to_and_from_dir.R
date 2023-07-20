#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param records character vector of records you want dropped to your directory
#' @return messages for confirmation
#' @export
drop_redcap_dir<-function(DB,records=NULL){
  DB <- validate_DB(DB)
  dir.create(file.path(get_dir(DB),"REDCap"),showWarnings = F)
  dir.create(file.path(get_dir(DB),"REDCap","other"),showWarnings = F)
  dir.create(file.path(get_dir(DB),"REDCap","upload"),showWarnings = F)
  DB_selected<- DB %>% select_redcap_records(records)
  for(x in DB$instruments$instrument_name){
    DB_selected[["data"]][[x]] %>% write_xl(DB,path=file.path(get_dir(DB),"REDCap",paste0(x,".xlsx")))
  }
  for (x in c("metadata","instruments","users")){ #,"log" #taking too long
    DB_selected[[x]] %>% write_xl(DB,path=file.path(get_dir(DB),"REDCap","other",paste0(x,".xlsx")))
  }
}

#' @title Reads DB from the dropped REDCap files in dir/REDCap/upload
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
read_redcap_dir<-function(DB){
  DB <- validate_DB(DB)
  path<-file.path(get_dir(DB),"REDCap","upload")
  if(!file.exists(path))stop("No REDCap files found at path --> ",path)
  x<-list.files.real(path)
  x<-x[which(gsub("\\.xlsx","",x)%in%DB$instruments$instrument_name)]
  DB_import<-DB
  DB_import[["data"]]<-list()
  for(y in x){
    DB_import[["data"]][[gsub("\\.xlsx","",y)]] <- readxl::read_xlsx(file.path(path,y)) %>% all_character_cols()
  }
  DB_import
}
