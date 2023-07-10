#' @title blank DB object
#' @return blank_DB list
blank_DB<- function(){
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
    metadata=NULL,
    choices=NULL,
    instruments=NULL,
    log=NULL,
    users=NULL,
    redcap_base_link = NULL,
    redcap_uri = NULL,
    home_link = NULL,
    records_link = NULL,
    API_link = NULL,
    API_playground_link = NULL,
    clean = NULL,
    data=NULL
  )
}

#' @title Validates DB
#' @return Message
#' @param DB DB from load_DB or setup_DB
validate_DB<-function(DB,silent = T){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  if( ! all(names(blank_DB())%in%names(DB))){
    stop("`DB` does not have the appropriate names. Did you use `load_DB()` or `setup_DB()` to generate it?")
  }
  if((length(DB[["data"]])==0)>0){
    if(!silent){
      warning("Valid list but no data yet!",immediate. = T)
    }
  }
  if(is.null(DB$dir_path)){
    stop("`DB$dir_path` is NULL!, Did you use `setup_DB()`?")
  }else{
    if( ! DB$dir_path %>% file.exists()) stop("`DB$dir_path`, '",DB$dir_path,"', does not exist!, Did you use `setup_DB()`?")
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
  for (CHECK in c("title","PID","version","last_metadata_update","last_data_update","home_link","API_playground_link")){
    if(is.null(DB[CHECK])){
      stop("`DB$",CHECK,"` is NULL!, Did you use `setup_DB()`?")
    }
  }
  if(!silent){
  message("`DB` validated!")
  }
  DB
}
#add year check

#' @title Reads DB from the directory
#' @param dir_path character file path of the directory
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
    is.null(DB$short_name) |
    is.null(DB$token_name) |
    is.null(DB$redcap_uri) |
    is.null(DB$title) |
    is.null(DB$PID)
  ){
    if(missing(short_name))stop("`short_name` is required for DBs that haven't been validated")
    if(missing(token_name))stop("`token_name` is required for DBs that haven't been validated")
    if(missing(redcap_base_link))stop("`redcap_base_link` is required for DBs that haven't been validated")
    DB$short_name <- short_name
    DB$token_name <- token_name
    DB$redcap_base_link <- redcap_base_link
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


#' @title Saves DB in the directory
#' @param DB DB from load_DB or update_DB
#' @return Message
#' @export
save_DB<-function(DB){
  #param check
  if( ! is.list(DB)) stop("DB must be a list")
  #function
  validate_DB(DB) %>% saveRDS(file=file.path(DB$dir_path,"R_objects","DB.rdata"))
  add_project(DB)
  # save_xls_wrapper(DB)
  message("Saved!")
}

#' @title Shows DB in the env
#' @param DB list for the package that contains applications and reference files
#' @return DB tables in your environment
#' @export
show_DB <- function(DB,also_metadata=T){
  validate_DB(DB)
  for(NAME in names(DB$data)){
    assign(NAME,DB$data[[NAME]],pos = 1)
  }
  if(also_metadata){
    for(NAME in c("metadata","instruments","log","users")){
      assign(NAME,DB[[NAME]],pos = 1)
    }
    assign("codebook",make_codebook(DB),pos = 1)
  }
}

#' @title Shows DB in the env
#' @return message
#' @export
delete_DB <- function(DB){
  unlink(file.path(DB$dir_path,"R_objects","DB.Rdata"))
  message("Deleted saved DB")
}


