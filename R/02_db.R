#' @title blank DB object
#' @return blank_DB list for reference
blank_DB<- function(){ # can sort this better in version 3.0.0
  list(
    title=NULL,
    PID=NULL,
    short_name=NULL,
    dir_path=NULL,
    token_name=NULL,
    id_col=NULL,
    version=NULL,
    last_metadata_update=NULL,
    last_data_update=NULL,
    project_info=NULL,
    metadata=NULL,
    instruments=NULL,
    arms=NULL,
    events=NULL,
    event_mapping = NULL,
    codebook=NULL,
    choices=NULL,
    missing_codes=NULL,
    log=NULL,
    users=NULL,
    redcap_base_link = NULL,
    redcap_uri = NULL,
    home_link = NULL,
    records_link = NULL,
    API_link = NULL,
    API_playground_link = NULL,
    clean = NULL,
    has_event_mappings = NULL,
    has_repeating = NULL,
    all_records = NULL,
    data=NULL
  )
}

validate_DB<-function(DB,silent = T){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  if( ! all(names(blank_DB())%in%names(DB))){
    stop("`DB` does not have the appropriate names. Did you use `load_DB()` or `setup_DB()` to generate it?")
  }
  if(is.null(DB$dir_path)){
    stop("`DB$dir_path` is NULL!, Did you use `setup_DB()`?")
  }else{
    if( ! DB$dir_path %>% file.exists()) warning("`DB$dir_path`, '",DB$dir_path,"', does not exist!, Did you use `setup_DB()`?\nThis can also happen with shared directories.",immediate. = T)
  }
  if(is.null(DB$short_name)){
    stop("`DB$short_name` is NULL!, Did you use `setup_DB()`?")
  }else{
    DB$short_name %>% validate_env_name()
  }
  if(is.null(DB$token_name)){
    stop("`DB$token_name` is NULL!, Did you use `setup_DB()`?")
  }else{
    DB$token_name %>% validate_env_name()
  }
  if(is.null(DB$redcap_base_link)){
    stop("`DB$redcap_base_link` is NULL!, Did you use `setup_DB()`?")
  }else{
    DB$redcap_base_link %>% validate_web_link()
  }
  # for (CHECK in c("title","PID","version","last_metadata_update","last_data_update","home_link","API_playground_link")){
  #   if(is.null(DB[[CHECK]])){
  #     stop("`DB$",CHECK,"` is NULL!, Did you use `setup_DB()`?")
  #   }
  # }
  if(!silent){
    if((length(DB[["data"]])==0)>0||is.null(DB$project_info)){
      warning("Valid list but no data yet!",immediate. = T)
    }
    message("`DB` validated!")
  }
  DB
}
#add year check

#' @title Setup for DB including token
#' @param short_name character name as a shortcut
#' @param dir_path character file path of the directory
#' @param token_name character string of what the token is called when using Sys.setenv and Sys.getenv
#' @param redcap_base_link character of the base REDCap link, ex. https://redcap.miami.edu
#' @param force logical for force blank load vs last save
#' @return DB
#' @export
setup_DB <- function(short_name,dir_path,token_name,redcap_base_link,force = F){
  #param check
  dir_path<-set_dir(dir_path)
  DB<-load_DB(dir_path,blank = force)
  DB$dir_path <- dir_path
  if(
    force |
    is.null(DB$last_metadata_update) |
    is.null(DB$project_info) |
    is.null(DB$short_name) |
    is.null(DB$token_name) |
    is.null(DB$redcap_uri) |
    is.null(DB$title) |
    is.null(DB$PID)
  ){
    if(missing(short_name))stop("`short_name` is required for DBs that haven't been validated")
    if(missing(token_name))stop("`token_name` is required for DBs that haven't been validated")
    if(missing(redcap_base_link))stop("`redcap_base_link` is required for DBs that haven't been validated")
    DB$short_name <- short_name %>% validate_env_name()
    DB$token_name <- token_name %>% validate_env_name()
    DB$redcap_base_link <- redcap_base_link %>% validate_web_link()
    DB$redcap_uri <- DB$redcap_base_link  %>% paste0("api/")
    DB <- validate_DB(DB)
  }else{
    if(! missing(short_name)){
      if(DB$short_name != short_name)stop("The `short_name`, ",short_name,", you provided does not match the one the was loaded ",DB$short_name)
    }
    if(! missing(token_name)){
      if(DB$token_name != token_name)stop("The `token_name`, ",token_name,", you provided does not match the one the was loaded ",DB$token_name)
    }
    if(! missing(redcap_base_link)){
      if(DB$redcap_base_link != redcap_base_link)stop("The `redcap_base_link`, ",redcap_base_link,", you provided does not match the one the was loaded ",DB$redcap_base_link)
    }
  }
  DB
}

#' @title Reads DB from the directory
#' @inheritParams setup_DB
#' @param blank logical for blank load or last save
#' @return DB
#' @export
load_DB<-function(dir_path,blank=F){
  if(blank){
    DB <- blank_DB()
  }else{
    DB_path<-file.path(dir_path,"R_objects","DB.rdata")
    if(file.exists(DB_path)){
      DB <-readRDS(file=DB_path)
      validate_DB(DB)
    }else{
      DB <- blank_DB()
    }
  }
  #DB summary----
  DB
}

#' @title Saves DB in the directory
#' @param DB object generated using `load_DB()` or `setup_DB()`
#' @return Message
#' @export
save_DB<-function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  DB$choices
  DB %>% validate_DB() %>% saveRDS(file=file.path(DB$dir_path,"R_objects","DB.rdata"))
  add_project(DB)
  # save_xls_wrapper(DB)
  message("Saved!")
}

#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param also_metadata logical for including metadata
#' @description
#' To add all of these to your global environment you could run the following code...
#' `data_list <- show_DB(DB)`
#' `for(NAME in names(data_list)){assign(NAME,data_list[[NAME]],pos = 1)}`
#' This code is not allowed on CRAN to avoid name conflicts
#' @return DB tables
#' @export
show_DB <- function(DB,also_metadata=T){
  DB <- validate_DB(DB)
  data_list <- list()
  for(NAME in names(DB$data)){
    L <- list(DB$data[[NAME]])
    names(L) <- NAME
    data_list <- data_list %>% append(L)
  }
  if(also_metadata){
    for(NAME in c("metadata","instruments","arms","events","log","users","codebook","project_info")){
      L <- list(DB[[NAME]])
      names(L) <- NAME
      data_list <- data_list %>% append(L)    }
  }
  data_list %>% list2env(envir = .GlobalEnv)
}

#' @title Deletes DB object from directory (solves occasional problems)
#' @inheritParams save_DB
#' @inheritParams setup_DB
#' @param dir_path character file path of the directory
#' @return message
#' @export
delete_DB <- function(DB,dir_path){
  if(!missing(DB)){
    DB <- validate_DB(DB)
    DIR <- DB$dir_path
    if(!missing(dir_path))warning("You only need to provide a directory path using a DB object OR dir_path. DB will be used by default.",immediate. = T)
  } else {
    if(missing(dir_path))stop("You must provide a directory path using a DB object or dir_path")
    DIR <- dir_path
  }
  DIR <-validate_dir(DIR,silent = F)
  delete_this <- file.path(DIR,"R_objects","DB.Rdata")
  if(file.exists(delete_this)){
    unlink(delete_this)
    message("Deleted saved DB")
  }else{
    warning("The DB object you wanted to is not there. Did you delete already? ",delete_this)
  }
}

all_records <- function(DB){
  records <- NULL
  for(NAME in names(DB$data)){
    records<- records %>% append(DB$data[[NAME]][[DB$id_col]])
  }
  records %>% unique()
}
