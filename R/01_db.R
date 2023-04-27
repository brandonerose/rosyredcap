blank_DB<-list(
  title=NULL,
  PID=NULL,
  id_col=NULL,
  version=NULL,
  last_metadata_update=NULL,
  last_data_update=NULL,
  metadata=NULL,
  choices=NULL,
  instruments=NULL,
  log=NULL,
  users=NULL,
  home_link = NULL,
  records_link = NULL,
  API_link = NULL,
  API_playground_link = NULL,
  clean = NULL,
  data=NULL
)

#' @title Validates DB
#' @return Message
#' @param DB DB from load_DB or update_DB
validate_DB<-function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  if( ! all(names(blank_DB)%in%names(DB))){
    stop("`DB` does not have the appropriate names. Did you use `load_DB()` to generate it?")
  }
  if((length(DB[["data"]])==0)>0){
    warning("Valid list but no data yet!",immediate. = T)
  }
  message("`DB` validated!")
}
#add year check

#' @title Reads DB from the directory
#' @param blank logical for blank load or last save
#' @return DB
#' @export
load_DB<-function(blank=F){
  DB_path<-file.path(get_dir(),"R_objects","DB.rdata")
  PID_path<-file.path(get_dir(),"R_objects","PID.rdata")
  if(file.exists(DB_path)&!blank){
    load(file=DB_path)
    validate_DB(DB)
    if(file.exists(PID_path)){
      load(file=PID_path)
      if(DB$PID!=PID) stop("Do you have more than REDCap project? The current directory contains a Project ID that does not match!")
    }
  }else{
    DB <- blank_DB
    if(!blank) message("`DB` was empty and created. This should only happen with a new directory!")
  }
  # if(!blank){
  #   # x<-names(DB)
  #   # x<-x[which(!x%in%names(blank_DB))]
  #   # message("Studies: ",paste0(x,collapse = ", "))
  #   # message("Last Update: ",DB$last_update)
  # }
  DB
}

#' @title Saves DB in the directory
#' @param DB DB from load_DB or update_DB
#' @return Message
#' @export
save_DB<-function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  validate_DB(DB)
  PID<-DB$PID
  save(PID,file=file.path(get_dir(),"R_objects","PID.rdata"))
  save(DB,file=file.path(get_dir(),"R_objects","DB.rdata"))
  # save_xls_wrapper(DB)
  message("Saved!")
}

#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return DB tables in your environment
#' @export
show_DB<-function(DB,also_metadata=T){
  validate_DB(DB)
  for(NAME in names(DB$data)){
    assign(NAME,DB$data[[NAME]],pos = 1)
  }
  if(also_metadata){
    for(NAME in c("metadata","instruments","log","users")){
      assign(NAME,DB[[NAME]],pos = 1)
    }
  }
}

