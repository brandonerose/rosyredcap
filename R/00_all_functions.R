#setup-----

cache<-NULL
.onLoad<-function(libname, pkgname){
  x<-hoardr::hoard()
  x$cache_path_set(path=.packageName,type="user_cache_dir")
  x$mkdir()
  cache<<-x
}

cache_path <- function(){
  cache$cache_path_get() %>% normalizePath()
}

cache_exists<- function(){
  cache_path() %>% file.exists()
}

#' @title Clear your cached projects
#' @return message confirmation
#' @export
cache_clear<- function(){
  cache$delete_all()
  message("Cache cleared!")
}

cache_projects_exists<- function(){
  if(cache_exists()){
    cache_path() %>% file.path("projects.rds") %>% file.exists()
  }else{
    warning("Cache doesn't exist",immediate. = T)
    FALSE
  }
}

#' @title Get your REDCap projects used by rosyredcap
#' @description
#' Everytime a setup or update is performed rosyredcap stores the most basic information
#' about that project to the cache so the user has a running log of everywhere there project information is stored,
#' which can be used to find, move, edit, delete that data.
#' @return data.frame of projects from the cache
#' @export
get_projects <- function(){
  if(cache_projects_exists()){
    projects<-cache_path() %>% file.path("projects.rds") %>% readRDS()
    return(projects)
  }else{
    "You have no projects cached. Try `add_project`" %>% message()
    return(
      blank_projects()
    )
  }
}

blank_project_cols <- function(){
  c("short_name","dir_path","token_name","PID","version","title","last_metadata_update","last_data_update","home_link","API_playground_link")
}

blank_projects <- function(){
  x<-matrix(data = character(0),ncol = length(blank_project_cols())) %>% as.data.frame()
  colnames(x)<-blank_project_cols()
  x
}

add_project <- function(DB){
  projects <- get_projects()
  projects <- projects[which(projects$short_name!=DB$short_name),]
  OUT <- NULL
  for(COL in blank_project_cols()){
    OUT <- OUT %>% append(as.character(DB[[COL]]))
  }
  OUT<- OUT %>% as.data.frame() %>% t() %>% as.data.frame()
  colnames(OUT) <- blank_project_cols()
  rownames(OUT)<-NULL
  OUT
  projects <- projects %>% dplyr::bind_rows(OUT)
  saveRDS(projects, file = cache$cache_path_get() %>% normalizePath() %>% file.path("projects.rds"))
}

validate_dir <- function(dir_path,silent=T){
  #param check
  dir_path<-clean_dir_path(dir_path)
  if ( ! file.exists(dir_path)) stop("dir_path does not exist")
  if ( ! is.logical(silent)) stop("silent parameter must be T/F")
  #function
  if( ! silent) message("directory --> '",dir_path,"'")
  stop_mes<-"Did you use `set_dir()`?"
  for(folder in c("R_objects","REDCap","output","scripts","input")){
    if ( ! file.exists(file.path(dir_path,folder))) stop("'",dir_path,"/",folder,"' missing! ",stop_mes)
  }
  # if ( ! file.exists(file.path(dir_path,"ref_tables"))) stop("'",dir_path,"/ref_tables' missing! ",stop_mes)
  if( ! silent) message("Directory is Valid!")
  dir_path
}

clean_dir_path <- function(dir_path){
  if ( ! is.character(dir_path)) stop("dir must be a character string")
  dir_path %>% trimws(whitespace = "[\\h\\v]") %>% normalizePath( winslash = "/",mustWork = F)
}

set_dir <- function(dir_path){
  dir_path<-clean_dir_path(dir_path)
  if( ! file.exists(dir_path)){
    if(utils::menu(choices = c("Yes","No"),title = "No file path found, create?")==1){
      dir.create(file.path(dir_path))
    }
    if ( ! file.exists(dir_path)) {
      stop("Path not found. Use absolute path or choose one within R project working directory.")
    }
  }
  for(folder in c("R_objects","REDCap","output","scripts","input")){
    if ( ! file.exists(file.path(dir_path,folder))) {
      dir.create(file.path(dir_path,folder),showWarnings = F)
    }
  }
  validate_dir(dir_path,silent=F)
}

#' @title get your directory
#' @inheritParams save_DB
#' @export
get_dir <- function(DB){
  dir_path <- DB$dir_path
  stop_mes<-"Did you use `set_dir()`?"
  if ( ! file.exists(dir_path)) {
    warning("Searched for directory --> '",dir_path,"' ...")
    stop(paste0("Does not exist. ", stop_mes))
  }
  # if()
  validate_dir(dir_path,silent=T)
  dir_path
}

#----- tokens ----------

has_redcap_token <- function(DB,silent=T){
  DB <- validate_DB(DB)
  DB$token_name %>% validate_env_name()
  DB$token_name %>% Sys.getenv() %>% is_redcap_token()
}

is_redcap_token <- function(token){
  pattern <- "^([0-9A-Fa-f]{32})(?:\\n)?$"
  token2 <-  sub(pattern,"\\1", token, perl = TRUE) %>% trimws(whitespace = "[\\h\\v]")
  info_message <- paste0("You can set it each session with `Sys.setenv(YOUR_token_name='YoUrNevErShaReToKeN')...` or for higher safety run `edit_r_environ()` from the `usethis` package and add `YOUR_token_name = 'YoUrNevErShaReToKeN'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the code, `Sys.getenv('YOUR_token_name')` or `Sys.getenv(DB$token_name)` and see if it returns your token!...")
  if(is.null(token)){
    message("The token is `NULL`, not a valid 32-character hexademical value.")
    return(F)
  }else if (is.na(token)) {
    message("The token is `NA`, not a valid 32-character hexademical value.")
    return(F)
  }else if (nchar(token) == 0L) {
    message("`Sys.getenv(DB$token_name)` returned no token or is an empty string. ",info_message)
    return(F)
  }else if(token2 != token){
    message("remove whitespace or extra lines from your token.")
    return(F)
  }else if (!grepl(pattern, token, perl = TRUE)) {
    message("The token from `Sys.getenv(DB$token_name)` is not a valid 32-character hexademical value.",info_message)
    return(F)
  }
  return(T)
}

validate_redcap_token <- function(DB,silent=T,return=T){
  while (!has_redcap_token(DB)) {
    set_redcap_token(DB)
    message("Try going to REDCap --> '",DB$links$redcap_API_link,"' run `link_API_token(DB)")
  }
  if(!silent){
    message("You have a valid token set in your session!")
  }
  if(return){
    return(Sys.getenv(DB$token_name))
  }
}

#' @title Sets a valid token for this session
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
set_redcap_token <- function(DB){
  token <- readline("What is your PSDB REDCap API token: ")
  while (!is_redcap_token(token)) {
    warning("You set an invalid token. Try going to REDCap --> '",DB$links$redcap_API_link,"' or run `link_API_token(DB)`",immediate. = T)
    token <- readline("What is your PSDB REDCap API token: ")
  }
  args =list(args =list(token))
  names(args) = DB$token_name
  do.call(Sys.setenv, args)
  message("Token was set for this session only using `Sys.getenv('",DB$token_name,"')` <- 'TheSecretTokenYouJustEntered'")
  message("For higher safety run `edit_r_environ()` from the `usethis` package and add `",DB$token_name,"='YoUrNevErShaReToKeN'` to that file...(then restart R under session tab after saving file)... The way to tell it worked is to run the code, `Sys.getenv('",DB$token_name,"')` or `Sys.getenv(DB$token_name)` or `has_redcap_token(DB)`, and see if it returns your token!...'")
}

#' @title View the REDCap API token currently stored in the session
#' @inheritParams save_DB
#' @return REDCap API token currently stored in the session
#' @export
view_redcap_token <- function(DB){
  DB <-validate_DB(DB)
  validate_redcap_token(DB,silent = F,return = T)
}


#' @title blank DB object
#' @return blank_DB list for reference
blank_DB<- function(){ # can sort this better in version 3.0.0
  list(
    short_name=NULL,
    token_name=NULL,
    dir_path=NULL,
    internals = list(
      last_metadata_update=NULL,
      last_data_update=NULL,
      last_data_transformation = NULL,
      data_extract_labelled = NULL,
      data_extract_clean = NULL
    ),
    redcap = list(
      project_id=NULL,
      project_title= NULL,
      id_col=NULL,
      version=NULL,
      project_info=NULL,
      metadata=NULL,
      instruments=NULL,
      arms=NULL,
      events=NULL,
      event_mapping = NULL,
      missing_codes=NULL,
      log=NULL,
      users=NULL,
      current_user=NULL,
      codebook=NULL,
      choices=NULL,
      raw_structure_cols = NULL,
      is_longitudinal = NULL,
      has_arms = NULL,
      has_multiple_arms = NULL,
      has_repeating_instruments_or_events = NULL,
      has_repeating_instruments = NULL,
      has_repeating_events = NULL
    ),
    auto_jobs = NULL,
    remap = list(
      metadata_map=NULL,
      metadata_new=NULL,
      instruments_map=NULL,
      instruments_new=NULL,
      arms_map=NULL,
      arms_new=NULL,
      events_map=NULL,
      events_new=NULL,
      event_mapping_map=NULL,
      event_mapping_new=NULL
    ),
    data_extract = NULL,
    data_transform = NULL,
    data_upload = NULL,
    all_records = NULL,
    links = list(
      redcap_base_link = NULL,
      redcap_uri = NULL,
      redcap_home_link = NULL,
      redcap_records_link = NULL,
      redcap_API_link = NULL,
      redcap_API_playground_link = NULL,
      github_link = "https://github.com/brandonerose/rosyredcap",
      thecodingdocs_link = "https://www.thecodingdocs.com/"
    )
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
  if(is.null(DB$links$redcap_base_link)){
    stop("`DB$links$redcap_base_link` is NULL!, Did you use `setup_DB()`?")
  }else{
    DB$links$redcap_base_link %>% validate_web_link()
  }
  # for (CHECK in c("title","PID","version","last_metadata_update","last_data_update","home_link","API_playground_link")){
  #   if(is.null(DB[[CHECK]])){
  #     stop("`DB$",CHECK,"` is NULL!, Did you use `setup_DB()`?")
  #   }
  # }
  if(!silent){
    if((length(DB$data_extract)==0)>0||is.null(DB$redcap$project_info)){
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
  missing_dir_path<-missing(dir_path)
  if(missing_dir_path){
    warning("If you don't supply a directory, rosyredcap will only run in R session. Package is best with a directory",immediate. = T)
    DB <- blank_DB()
  }
  if(!missing_dir_path){
    dir_path<-set_dir(dir_path)
    DB<-load_DB(dir_path,blank = force)
    DB$dir_path <- dir_path
  }
  if(
    force |
    is.null(DB$internals$last_metadata_update) |
    is.null(DB$redcap$project_info) |
    is.null(DB$short_name) |
    is.null(DB$token_name) |
    is.null(DB$links$redcap_uri) |
    is.null(DB$redcap$project_title) |
    is.null(DB$redcap$project_id)
  ){
    if(missing(short_name))stop("`short_name` is required for DBs that haven't been validated")
    if(missing(token_name))stop("`token_name` is required for DBs that haven't been validated")
    if(missing(redcap_base_link))stop("`redcap_base_link` is required for DBs that haven't been validated")
    DB$short_name <- short_name %>% validate_env_name()
    DB$token_name <- token_name %>% validate_env_name()
    DB$links$redcap_base_link <- redcap_base_link %>% validate_web_link()
    DB$links$redcap_uri <- DB$links$redcap_base_link  %>% paste0("api/")
    DB <- validate_DB(DB)
  }else{
    if(! missing(short_name)){
      if(DB$short_name != short_name)stop("The `short_name`, ",short_name,", you provided does not match the one the was loaded ",DB$short_name)
    }
    if(! missing(token_name)){
      if(DB$token_name != token_name)stop("The `token_name`, ",token_name,", you provided does not match the one the was loaded ",DB$token_name)
    }
    if(! missing(redcap_base_link)){
      if(DB$links$redcap_base_link != redcap_base_link)stop("The `redcap_base_link`, ",redcap_base_link,", you provided does not match the one the was loaded ",DB$links$redcap_base_link)
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
  for(NAME in names(DB$data_extract)){
    L <- list(DB$data_extract[[NAME]])
    names(L) <- NAME
    data_list <- data_list %>% append(L)
  }
  if(also_metadata){
    for(NAME in c("metadata","instruments","arms","events","event_mapping","log","users","codebook","project_info")){#"unique_events"
      L <- list(DB[[NAME]])
      names(L) <- NAME
      data_list <- data_list %>% append(L)
    }
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
  cols <- DB$redcap$id_col
  if(is.data.frame(DB$arms)){
    if(nrow(DB$arms)>1){
      cols <- DB$redcap$id_col %>% append("arm_num")
    }
  }
  if(length(cols)==1){
    records <- data.frame(
      records =  names(DB$data_extract) %>% lapply(function(IN){DB$data_extract[[IN]][,cols]}) %>% unlist() %>% unique()
    )
    colnames(records) <- cols
  }
  if(length(cols) == 2){
    records <- names(DB$data_extract) %>% lapply(function(IN){DB$data_extract[[IN]][,cols]}) %>% dplyr::bind_rows() %>% unique()
    # records <- records[order(as.integer(records[[DB$redcap$id_col]])),]
  }
  rownames(records) <- NULL
  if(records[[DB$redcap$id_col]]%>% duplicated() %>% any())stop("duplicate ",DB$redcap$id_col, " in all_records() function")
  records
}

#DB ---------

#' @title blank DB object
#' @return blank_DB list for reference
blank_DB<- function(){ # can sort this better in version 3.0.0
  list(
    short_name=NULL,
    token_name=NULL,
    dir_path=NULL,
    internals = list(
      last_metadata_update=NULL,
      last_data_update=NULL,
      last_data_transformation = NULL,
      data_extract_labelled = NULL,
      data_extract_clean = NULL
    ),
    redcap = list(
      project_id=NULL,
      project_title= NULL,
      id_col=NULL,
      version=NULL,
      project_info=NULL,
      metadata=NULL,
      instruments=NULL,
      arms=NULL,
      events=NULL,
      event_mapping = NULL,
      missing_codes=NULL,
      log=NULL,
      users=NULL,
      current_user=NULL,
      codebook=NULL,
      choices=NULL,
      raw_structure_cols = NULL,
      is_longitudinal = NULL,
      has_arms = NULL,
      has_multiple_arms = NULL,
      has_repeating_instruments_or_events = NULL,
      has_repeating_instruments = NULL,
      has_repeating_events = NULL
    ),
    auto_jobs = NULL,
    remap = list(
      metadata_map=NULL,
      metadata_new=NULL,
      instruments_map=NULL,
      instruments_new=NULL,
      arms_map=NULL,
      arms_new=NULL,
      events_map=NULL,
      events_new=NULL,
      event_mapping_map=NULL,
      event_mapping_new=NULL
    ),
    data_extract = NULL,
    data_transform = NULL,
    data_upload = NULL,
    all_records = NULL,
    links = list(
      redcap_base_link = NULL,
      redcap_uri = NULL,
      redcap_home_link = NULL,
      redcap_records_link = NULL,
      redcap_API_link = NULL,
      redcap_API_playground_link = NULL,
      github_link = "https://github.com/brandonerose/rosyredcap",
      thecodingdocs_link = "https://www.thecodingdocs.com/"
    )
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
  if(is.null(DB$links$redcap_base_link)){
    stop("`DB$links$redcap_base_link` is NULL!, Did you use `setup_DB()`?")
  }else{
    DB$links$redcap_base_link %>% validate_web_link()
  }
  # for (CHECK in c("title","PID","version","last_metadata_update","last_data_update","home_link","API_playground_link")){
  #   if(is.null(DB[[CHECK]])){
  #     stop("`DB$",CHECK,"` is NULL!, Did you use `setup_DB()`?")
  #   }
  # }
  if(!silent){
    if((length(DB$data_extract)==0)>0||is.null(DB$redcap$project_info)){
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
  missing_dir_path<-missing(dir_path)
  if(missing_dir_path){
    warning("If you don't supply a directory, rosyredcap will only run in R session. Package is best with a directory",immediate. = T)
    DB <- blank_DB()
  }
  if(!missing_dir_path){
    dir_path<-set_dir(dir_path)
    DB<-load_DB(dir_path,blank = force)
    DB$dir_path <- dir_path
  }
  if(
    force |
    is.null(DB$internals$last_metadata_update) |
    is.null(DB$redcap$project_info) |
    is.null(DB$short_name) |
    is.null(DB$token_name) |
    is.null(DB$links$redcap_uri) |
    is.null(DB$redcap$project_title) |
    is.null(DB$redcap$project_id)
  ){
    if(missing(short_name))stop("`short_name` is required for DBs that haven't been validated")
    if(missing(token_name))stop("`token_name` is required for DBs that haven't been validated")
    if(missing(redcap_base_link))stop("`redcap_base_link` is required for DBs that haven't been validated")
    DB$short_name <- short_name %>% validate_env_name()
    DB$token_name <- token_name %>% validate_env_name()
    DB$links$redcap_base_link <- redcap_base_link %>% validate_web_link()
    DB$links$redcap_uri <- DB$links$redcap_base_link  %>% paste0("api/")
    DB <- validate_DB(DB)
  }else{
    if(! missing(short_name)){
      if(DB$short_name != short_name)stop("The `short_name`, ",short_name,", you provided does not match the one the was loaded ",DB$short_name)
    }
    if(! missing(token_name)){
      if(DB$token_name != token_name)stop("The `token_name`, ",token_name,", you provided does not match the one the was loaded ",DB$token_name)
    }
    if(! missing(redcap_base_link)){
      if(DB$links$redcap_base_link != redcap_base_link)stop("The `redcap_base_link`, ",redcap_base_link,", you provided does not match the one the was loaded ",DB$links$redcap_base_link)
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
  for(NAME in names(DB$data_extract)){
    L <- list(DB$data_extract[[NAME]])
    names(L) <- NAME
    data_list <- data_list %>% append(L)
  }
  if(also_metadata){
    for(NAME in c("metadata","instruments","arms","events","event_mapping","log","users","codebook","project_info")){#"unique_events"
      L <- list(DB[[NAME]])
      names(L) <- NAME
      data_list <- data_list %>% append(L)
    }
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
  cols <- DB$redcap$id_col
  if(is.data.frame(DB$arms)){
    if(nrow(DB$arms)>1){
      cols <- DB$redcap$id_col %>% append("arm_num")
    }
  }
  if(length(cols)==1){
    records <- data.frame(
      records =  names(DB$data_extract) %>% lapply(function(IN){DB$data_extract[[IN]][,cols]}) %>% unlist() %>% unique()
    )
    colnames(records) <- cols
  }
  if(length(cols) == 2){
    records <- names(DB$data_extract) %>% lapply(function(IN){DB$data_extract[[IN]][,cols]}) %>% dplyr::bind_rows() %>% unique()
    # records <- records[order(as.integer(records[[DB$redcap$id_col]])),]
  }
  rownames(records) <- NULL
  if(records[[DB$redcap$id_col]]%>% duplicated() %>% any())stop("duplicate ",DB$redcap$id_col, " in all_records() function")
  records
}

#download -----------

redcap_api_base <- function(url,token,content,additional_args=NULL){
  body <-list(
    "token"= token,
    content=content,
    format='csv',
    returnFormat='json'
  )
  if(!missing(additional_args)||is.null(additional_args)){
    body <- body %>% append(additional_args)
  }
  httr::POST(
    url = url,
    body = body,
    encode = "form"
  )
}

process_response <- function(response,error_action){
  content <- httr::content(response)
  if(httr::http_error(response)){
    if(!missing(error_action)){
      if(!is.null(error_action)){
        if(!error_action%in%c("stop","warn"))stop("error_action must be 'stop' or 'warn'")
        general_error<-response$status_code
        specific_error<-http_errors$Description[which(http_errors$Value==response$status_code)]
        message <- paste0("HTTP error ",general_error, ". ",specific_error,". ",content[["error"]])
        if(error_action=="stop")stop(message)
        warning(message,immediate. = T)
      }
    }
    return(NA)
  }
  return(all_character_cols(content))
}

test_redcap <- function(DB){
  ERROR <-T
  while(ERROR){
    version <- redcap_api_base(DB$links$redcap_uri,validate_redcap_token(DB),"version")
    ERROR <-version %>% httr::http_error()
    if(ERROR){
      warning('Your REDCap API token check failed. Invalid token or API privileges. Contact Admin! Consider rerunnning `setup_DB()`',immediate. = T)
      warning("HTTP error ",version %>% httr::status_code(), ". Check your token, internet connection, and redcap base link.",immediate. = T)
      message("Try going to REDCap --> '",DB$links$redcap_API_link,"' or run `link_API_token(DB)`")
      set_redcap_token(DB)
    }
  }
  message("Connected to REDCap!")
  DB$version <- version %>% httr::content(as="text") %>% as.character()
  DB
}

get_redcap_info <- function(DB,content,error_action=NULL,additional_args=NULL){
  allowed_content <- c("project","arm","event","metadata","instrument","repeatingFormsEvents","user","userRole","userRoleMapping","log","formEventMapping")
  if(!content%in%allowed_content)stop("Must use the following content... ",paste0(allowed_content,collapse = ", "))
  redcap_api_base(url=DB$links$redcap_uri,token = validate_redcap_token(DB),content = content,additional_args=additional_args) %>% process_response(error_action)
}

#' @title Drop redcap files to directory
#' @inheritParams save_DB
#' @param original_file_names logical for using original uploaded filenames vs system defined
#' @param overwrite logical rewriting over the downladed files in your directory. A better alternative is deleting files you want to update.
#' @return message
#' @export
get_redcap_files <- function(DB,original_file_names = F,overwrite = F){
  file_rows <- which(DB$redcap$metadata$field_type=="file")
  out_dir <- file.path(DB$dir_path,"REDCap","files")
  if(length(file_rows)>0){
    dir.create(out_dir,showWarnings = F)
    for(field_name in DB$redcap$metadata$field_name[file_rows]){
      out_dir_folder <- file.path(out_dir,field_name)
      dir.create(out_dir_folder,showWarnings = F)
      form_name <- DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name == field_name)]
      is_repeating <- DB$redcap$instruments$repeating[which(DB$redcap$instruments$instrument_name==form_name)]
      form <- DB$data_extract[[form_name]]
      rows_to_save <- which(!is.na(form[[field_name]]))
      for(i in rows_to_save){
        file_name <-form[[field_name]][i]
        record_id <- form[[DB$redcap$id_col]][i]
        repeat_instrument = form[["redcap_repeat_instrument"]][i]
        repeat_instance = form[["redcap_repeat_instance"]][i]
        if(!original_file_names){
          if(anyDuplicated(file_name)>0){
            warning(paste0("You have duplicate file names in ",form_name,", ",field_name,". Therefore will use system generated names"),immediate. = T)
            original_file_names <- F
          }
        }
        file_name <- ifelse(original_file_names,file_name,paste0(form_name,"_",field_name,"_",ifelse(is_repeating,"inst_",""),repeat_instance,"ID_",record_id,".",tools::file_ext(file_name)))
        if(!file.exists(file.path(out_dir_folder,file_name))||overwrite){
          REDCapR::redcap_download_file_oneshot(
            redcap_uri = DB$links$redcap_uri,
            token = validate_redcap_token(DB),
            field = field_name,
            record = DB$data_extract$results$record_id[i],
            directory = out_dir_folder,
            file_name = file_name,
            repeat_instrument = repeat_instrument,
            repeat_instance = repeat_instance,
            verbose = F
          )
          message("`",file_name,"` saved at --> ",out_dir_folder)
        }
      }
    }
  }
  message("Checked for files!")
}

get_redcap_metadata<-function(DB){
  DB$internals$last_metadata_update=Sys.time()
  DB$redcap$project_info <- get_redcap_info(DB,"project")
  DB$redcap$project_title <-  DB$redcap$project_info$project_title
  DB$redcap$project_id <- DB$redcap$project_info$project_id
  DB$redcap$metadata <- get_redcap_info(DB,"metadata","stop")
  DB$redcap$metadata$section_header <- DB$redcap$metadata$section_header %>% remove_html_tags()
  DB$redcap$metadata$field_label <- DB$redcap$metadata$field_label %>% remove_html_tags()
  DB$redcap$instruments <- get_redcap_info(DB,"instrument","warn")

  DB$arms <- get_redcap_info(DB,"arm")
  DB$events <- get_redcap_info(DB,"event","warn")
  # if(is.data.frame(DB$unique_events)){
  #   DB$events <- data.frame(
  #     event_name = unique(DB$unique_events$event_name),
  #     arms = unique(DB$unique_events$event_name) %>% sapply(function(event_name){
  #       DB$unique_events$arm_num[which(DB$unique_events$event_name==event_name)] %>% unique() %>% paste0(collapse = " | ")
  #     })
  #   )
  # }
  DB$event_mapping  <- get_redcap_info(DB,"formEventMapping","warn")
  DB$has_event_mappings <- is.data.frame(DB$event_mapping)
  if(DB$has_event_mappings){
    DB$event_mapping$unique_event_name
    DB$events$forms = DB$events$unique_event_name %>% sapply(function(events){
      DB$event_mapping$form[which(DB$event_mapping$unique_event_name==events)] %>% unique() %>% paste0(collapse = " | ")
    })
  }
  DB$redcap$id_col<-DB$redcap$metadata[1,1] %>% as.character() #RISKY?

  DB$redcap$metadata<-DB$redcap$metadata %>%dplyr::bind_rows(
    data.frame(
      field_name=paste0(unique(DB$redcap$instruments$instrument_name),"_complete"),form_name=unique(DB$redcap$instruments$instrument_name),field_type="radio",select_choices_or_calculations="0, Incomplete | 1, Unverified | 2, Complete"
    )
  ) %>% unique()
  if(any(DB$redcap$metadata$field_type=="checkbox")){
    for(field_name in DB$redcap$metadata$field_name[which(DB$redcap$metadata$field_type=="checkbox")]){
      x<-DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field_name)] %>% split_choices()
      DB$redcap$metadata<-DB$redcap$metadata %>%dplyr::bind_rows(
        data.frame(
          field_name=paste0(field_name,"___",x$code),
          form_name=DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name==field_name)]  ,
          field_label=x$name,
          # field_label_full=paste0(DB$redcap$metadata$field_label[which(DB$redcap$metadata$field_name==field_name)]," - ",x$name),
          field_type="checkbox_choice",
          select_choices_or_calculations=c("0, Unchecked | 1, Checked")
        )
      ) %>% all_character_cols()
    }
  }
  if(any(DB$redcap$metadata$field_type=="yesno")){
    DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_type=="yesno")] <- c("0, No | 1, Yes")
  }

  DB$redcap$instruments$repeating <- F
  DB$has_repeating <- F
  # if(DB$redcap$project_info$has_repeating_instruments_or_events=="1")
  repeating <- get_redcap_info(DB,"repeatingFormsEvents")
  if(is.data.frame(repeating)){
    DB$redcap$instruments$repeating <- DB$redcap$instruments$instrument_name%in%repeating$form_name
    DB$redcap$metadata<-DB$redcap$metadata %>%dplyr::bind_rows(
      data.frame(
        field_name="redcap_repeat_instance",form_name=DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)] ,field_label="REDCap Repeat Instance",field_type="text",select_choices_or_calculations=NA
      )
    ) %>% unique()
    DB$redcap$metadata<-DB$redcap$metadata %>%dplyr::bind_rows(
      data.frame(
        field_name="redcap_repeat_instrument",form_name=DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)] ,field_label="REDCap Repeat Instrument",field_type="text",select_choices_or_calculations=NA
      )
    ) %>% unique()
  }
  if(DB$has_event_mappings){
    DB$redcap$instruments$repeating[
      which(
        DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
          # instrument_name <- DB$redcap$instruments$instrument_name %>% sample(1)
          anyDuplicated(DB$event_mapping$arm_num[which(DB$event_mapping$form==instrument_name)] %>% unique())>0
        })
      )
    ] <- T
  }
  if(any(DB$redcap$instruments$repeating)){
    DB$has_repeating <- T
  }
  # metadata_remap <- generate_default_remap(DB)
  DB$users <- get_redcap_users(DB)
  DB$codebook <- make_codebook(DB)
  DB$missing_codes <- missing_codes2(DB)
  DB$log<-check_redcap_log(DB,last = 2,units = "mins")
  DB$users$current_user<-DB$users$username==DB$log$username[which(DB$log$details=="Export REDCap version (API)") %>% dplyr::first()]
  DB$home_link <- paste0(DB$links$redcap_base_link,"redcap_v",DB$version,"/index.php?pid=",DB$PID)
  DB$records_link <- paste0(DB$links$redcap_base_link,"redcap_v",DB$version,"/DataEntry/record_status_dashboard.php?pid=",DB$PID)
  DB$links$redcap_API_link <- paste0(DB$links$redcap_base_link,"redcap_v",DB$version,"/API/project_api.php?pid=",DB$PID)
  DB$API_playground_link <- paste0(DB$links$redcap_base_link,"redcap_v",DB$version,"/API/playground.php?pid=",DB$PID)
  DB
}

get_redcap_data<-function(DB,clean=T,records=NULL){
  DB$last_data_update <- Sys.time()
  raw <- get_raw_redcap(
    DB=DB,
    clean=F,
    records=records
  )
  DB <- raw_process_redcap(raw = raw,DB = DB)
  if(is.null(records)){
    DB$all_records <- all_records(DB)
  }
  DB$clean <- F
  if(clean){
    DB <- DB %>% raw_to_clean_redcap()
  }
  DB
}

get_redcap_users<-function(DB){
  userRole <-get_redcap_info(DB,"userRole") %>% dplyr::select("unique_role_name","role_label")
  userRoleMapping<- get_redcap_info(DB,"userRoleMapping")
  user<- get_redcap_info(DB,"user")
  merge(merge(userRole,userRoleMapping,by="unique_role_name"),user, by="username")
}

#' @title Check the REDCap log
#' @inheritParams save_DB
#' @param last numeric paired with units. Default is 24.
#' @param units character paired with last. Options are "mins","hours","days". Default is "hours".
#' @param begin_time character of time where the log should start from. Example 2023-07-11 13:15:06.
#' @return data.frame of log that has been cleaned and has extra summary columns
#' @export
check_redcap_log <- function(DB,last=24,units="hours",begin_time=""){
  if(units=="days"){
    x<-(Sys.time()-lubridate::days(last)) %>% as.character()
  }
  if(units=="hours"){
    x<-(Sys.time()-lubridate::hours(last)) %>% as.character()
  }
  if(units=="mins"){
    x<-(Sys.time()- lubridate::minutes(last)) %>% as.character()
  }
  if(begin_time!=""){
    x<-begin_time
  }
  get_redcap_info(DB,"log",additional_args = list(beginTime=x)) %>% clean_redcap_log()
}

#' @title Check the REDCap log
#' @inheritParams save_DB
#' @param clean T/F for clean vs raw labels
#' @param records optional records
#' @return data.frame of raw_redcap
#' @export
get_raw_redcap <- function(DB,clean=T,records=NULL){
  REDCapR::redcap_read(redcap_uri=DB$links$redcap_uri, token=validate_redcap_token(DB),batch_size = 2000, interbatch_delay = 0.1,records = records, raw_or_label = ifelse(clean,"label","raw"))$data %>% all_character_cols()
}




# file repo -----------------


#' @title Uploads a file to REDCap
#' @inheritParams save_DB
#' @param file file location on your PC
#' @return messages for confirmation
#' @export
upload_file_to_redcap_fileRepository <- function(DB,file){
  DB <- validate_DB(DB)
  file<-normalizePath(file)
  if(!file.exists(file)) stop("File does not exist! --> ",file)
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
      action='import',
      content='fileRepository',
      returnFormat='json',
      file=httr::upload_file(file)
    ),
    encode = "multipart"
  )
  if(httr::http_error(response))stop("File upload failed")
  message("File Uploaded! --> ",file)
}

#' @title Checks REDCap for current files
#' @inheritParams save_DB
#' @return data.frame of files
#' @export
check_redcap_files <- function(DB){
  DB <- validate_DB(DB)
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
      content='fileRepository',
      action='list',
      format='csv',
      folder_id='',
      returnFormat='json'
    ),
    encode = "form"
  )
  if(httr::http_error(response))stop("File check failed")
  message("Files checked!")
  httr::content(response)
}

#' @title Uploads a folder name to REDCap
#' @inheritParams save_DB
#' @param name folder name
#' @return messages for confirmation
#' @export
add_redcap_folder <- function(DB,name){
  DB <- validate_DB(DB)
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
      content='fileRepository',
      action='createFolder',
      format='csv',
      name=name,
      folder_id='',
      returnFormat='json'
    ),
    encode = "form"
  )
  if(httr::http_error(response))stop("Folder add failed")
  message("Folder added!")
  httr::content(response)
}

#' @title Uploads a folder name to REDCap
#' @inheritParams save_DB
#' @param doc_id from the file list `check_redcap_files(DB)`
#' @return messages for confirmation
#' @export
delete_redcap_file <- function(DB,doc_id){
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
      content='fileRepository',
      action='delete',
      doc_id=doc_id,
      returnFormat='json'
    ),
    encode = "form"
  )
  if(httr::http_error(response))stop("File delete failed")
  message("File deleted!")
}

# files -----------------
upload_file_to_redcap <- function(DB,file,record, field,repeat_instance = NULL,event = NULL){
  # DB <- validate_DB(DB)
  file<-normalizePath(file)
  if(!file.exists(file)) stop("File does not exist! --> ",file)
  body <- list(
    "token"=validate_redcap_token(DB),
    action='import',
    content='file',
    record =record,
    field =field,
    returnFormat='csv',
    file=httr::upload_file(file)
  )
  if(!is.null(event)){
    body$event <- event
  }
  if(!is.null(repeat_instance)){
    body$repeat_instance <- repeat_instance
  }
  response <- httr::POST(
    url = DB$links$redcap_uri,
    body = body,
    encode = "multipart"
  )
  if(httr::http_error(response))stop("File upload failed")
  message("File uploaded! --> ",file)
}

delete_file_from_redcap <- function(DB,record, field,repeat_instance = NULL, event = NULL){
  # DB <- validate_DB(DB)
  body <- list(
    "token"=validate_redcap_token(DB),
    action='delete',
    content='file',
    record =record,
    field =field,
    returnFormat='csv'
  )
  if(!is.null(event)){
    body$event <- event
  }
  if(!is.null(repeat_instance)){
    body$repeat_instance <- repeat_instance
  }
  response <- httr::POST(
    url = DB$links$redcap_uri,
    body = body
  )
  if(httr::http_error(response))stop("File Delete failed")
  message("File Deleted!")
}

#process ---------

raw_process_redcap <- function(raw,DB){
  if(nrow(raw)>0){
    raw <-raw %>% all_character_cols()
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")

    if('redcap_event_name'%in%colnames(raw)){
      raw$id_temp <- 1:nrow(raw)
      raw<- merge(raw,DB$events[,c("arm_num","event_name","unique_event_name")],by.x="redcap_event_name",by.y="unique_event_name",sort = F)
      add_ons <-add_ons[which(add_ons%in%colnames(raw))]

      cols <- c(add_ons, colnames(raw)) %>% unique()
      raw <- raw[order(raw$id_temp),cols%>% sapply(function(c){which(colnames(raw)==c)}) %>% as.integer()]
      raw$id_temp <- NULL
    }
    add_ons <-add_ons[which(add_ons%in%colnames(raw))]

    for(instrument_name in DB$redcap$instruments$instrument_name){
      add_ons_x <- add_ons
      #instrument_name<- DB$redcap$instruments$instrument_name %>% sample(1)
      is_repeating_instrument <- instrument_name%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
      rows <-1:nrow(raw)
      if(!DB$has_event_mappings){
        if("redcap_repeat_instrument"%in%colnames(raw)){
          if(is_repeating_instrument){
            rows <- which(raw$redcap_repeat_instrument==instrument_name)
          }
          if(!is_repeating_instrument){
            rows <- which(is.na(raw$redcap_repeat_instrument))
          }
        }
      }
      if(DB$has_event_mappings){
        events_ins <- DB$event_mapping$unique_event_name[which(DB$event_mapping$form==instrument_name)] %>% unique()
        rows <- which(raw$redcap_event_name%in%events_ins)
      }
      if(!is_repeating_instrument){
        add_ons_x <- add_ons_x[which(!add_ons_x%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
      }
      cols <- unique(c(add_ons_x,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(raw))]))
      DB$data_extract[[instrument_name]]<-raw[rows,cols]
    }
  }
  DB
}

#' @title Select REDCap records from DB
#' @inheritParams save_DB
#' @param records character vector of the IDs you want to filter the DB by
#' @return DB object that has been filtered to only include the specified records
#' @export
select_redcap_records<-function(DB, records=NULL){
  DB_selected<-DB
  if(!is.null(records)){
    if (length(records)==0)stop("Must supply records")
    DB_selected$data<-list()
    BAD <-records[which(!records%in%DB$all_records[[DB$redcap$id_col]])]
    GOOD <-records[which(records%in%DB$all_records[[DB$redcap$id_col]])]
    if(length(BAD)>0)stop("Following records are not found in DB: ", BAD %>% paste0(collapse = ", "))
    for(FORM in names(DB$data_extract)){
      DB_selected[["data"]][[FORM]] <-DB$data_extract[[FORM]][which(DB$data_extract[[FORM]][[DB$redcap$id_col]]%in%GOOD),]
    }
  }
  DB_selected
}

clean_to_raw_redcap <- function(DB){
  DB <- validate_DB(DB)
  for(TABLE in names(DB$data_extract)){
    DB$data_extract[[TABLE]] <- clean_to_raw_form(FORM = DB$data_extract[[TABLE]],DB=DB)
  }
  DB$clean<-F
  DB
}

raw_to_clean_redcap <- function(DB){
  if(DB$clean)stop("DB is already clean (not raw coded values)")
  use_missing_codes <- is.data.frame(DB$missing_codes)
  for(instrument_name in DB$redcap$instruments$instrument_name){
    for (field_name in DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_type%in%c("radio","dropdown"))]){
      z<-DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field_name)] %>% split_choices()
      DB$data_extract[[instrument_name]][[field_name]]<-DB$data_extract[[instrument_name]][[field_name]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          coded_redcap<-which(z$code==C)
          if(length(coded_redcap)>0){
            OUT<-z$name[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2<-which(DB$missing_codes$code==C)
              if(length(coded_redcap2)>0){
                OUT<-DB$missing_codes$name[coded_redcap2]
              }else{
                warning("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C,". Also not a missing code.")
              }
            }else{
              warning("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C)
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()

    }
    for (field_name in DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_type=="yesno")]){
      z<-data.frame(
        code=c(0,1),
        name=c("No","Yes")
      )
      DB$data_extract[[instrument_name]][[field_name]]<-DB$data_extract[[instrument_name]][[field_name]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          D<-which(z$code==C)
          if(length(D)>0){
            OUT<-z$name[D]
          }
          if(length(D)==0){
            if(use_missing_codes){
              E<-which(DB$missing_codes$code==C)
              if(length(E)==0){
                warning("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C,". Also not a missing code.")
              }
              if(length(E)>0){
                OUT<-DB$missing_codes$name[E]
              }
            }else{
              warning("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C)
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }
    for (field_name in DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_type=="checkbox_choice")]){
      z<-data.frame(
        code=c(0,1),
        name=c("Unchecked","Checked")
      )
      DB$data_extract[[instrument_name]][[field_name]]<-DB$data_extract[[instrument_name]][[field_name]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          D<-which(z$code==C)
          if(length(D)>0){
            OUT<-z$name[D]
          }
          if(length(D)==0){
            if(use_missing_codes){
              E<-which(DB$missing_codes$code==C)
              if(length(E)==0){
                warning("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C,". Also not a missing code.")
              }
              if(length(E)>0){
                OUT<-DB$missing_codes$name[E]
              }
            }else{
              warning("Mismatch in choices compared to REDCap (above)! Table: ",instrument_name,", Column: ", field_name,", Choice: ",C)
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }
    if(use_missing_codes){
      for(field_name in DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&!DB$redcap$metadata$field_type%in%c("radio","dropdown","yesno","checkbox","checkbox_choice","descriptive"))]){
        z<-DB$missing_codes
        DB$data_extract[[instrument_name]][[field_name]]<-DB$data_extract[[instrument_name]][[field_name]] %>% sapply(function(C){
          OUT<-C
          if(!is.na(C)){
            D<-which(z$code==C)
            if(length(D)>0){
              OUT<-z$name[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  # if(  use_missing_codes) warning("You have missing codes in your redcap. such as UNK for unknown.")
  DB$clean<-T
  DB
}

#' @title Clean to Raw REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of clean REDCap to be converted to raw REDCap (for uploads)
#' @return DB object that has been filtered to only include the specified records
#' @export
clean_to_raw_form <- function(FORM,DB){
  use_missing_codes <- is.data.frame(DB$missing_codes)
  # if(!deparse(substitute(FORM))%in%DB$redcap$instruments$instrument_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  if(any(!colnames(FORM)%in%DB$redcap$metadata$field_name))stop("All column names in your form must match items in your metadata, `DB$redcap$metadata$field_name`")
  instrument <- DB$redcap$metadata$form_name[
    which(
      DB$redcap$metadata$field_name%in%colnames(FORM)&
        !DB$redcap$metadata$field_name%in%c(DB$redcap$id_col,"redcap_repeat_instance","redcap_repeat_instrument")
    )
  ] %>% unique()
  if(length(instrument)>1)stop("All column names in your form must match only one form in your metadata, `DB$redcap$instruments$instrument_name`")
  metadata<-DB$redcap$metadata[which(DB$redcap$metadata$form_name==instrument),]
  for(COL_NAME in FORM %>% colnames()){
    if(COL_NAME%in%metadata$field_name[which(metadata$field_type%in%c("radio","dropdown","checkbox_choice"))]){
      z<-metadata$select_choices_or_calculations[which(metadata$field_name==COL_NAME)] %>% split_choices()
      FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          coded_redcap<-which(z$name==C)
          if(length(coded_redcap)>0){
            OUT<-z$code[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2<-which(DB$missing_codes$name==C)
              if(length(coded_redcap2)>0){
                OUT<-DB$missing_codes$code[coded_redcap2]
              }else{
                stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }
    if(COL_NAME%in%metadata$field_name[which(metadata$field_type=="yesno")]){
      z<-data.frame(
        code=c(0,1),
        name=c("No","Yes")
      )
      FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          D<-which(z$name==C)
          if(length(D)>0){
            OUT<-z$code[D]
          }
          if(length(D)==0){
            if(use_missing_codes){
              E<-which(DB$missing_codes$name==C)
              if(length(E)==0){
                stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
              }
              if(length(E)>0){
                OUT<-DB$missing_codes$code[E]
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C)
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }
    if(use_missing_codes){
      if(COL_NAME%in%metadata$field_name[which(!metadata$field_type%in%c("radio","dropdown","yesno","checkbox_choice"))]){
        FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
          OUT<-C
          if(!is.na(C)){
            D<-which(DB$missing_codes$name==C)
            if(length(D)>0){
              OUT<-DB$missing_codes$code[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  FORM
}

clean_redcap_log <- function(log){
  log$record_id <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),gsub("Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation| |[:):]|[:(:]","",A),NA)})
  log$action_type <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),(A %>% strsplit(" ") %>% unlist())[1],NA)})
  comments <- which(log$action=="Manage/Design"&grepl("Add field comment|Edit field comment|Delete field comment",log$details))
  if(length(comments)>0){
    log$record_id[comments] <- stringr::str_extract(log$details[comments], "(?<=Record: )[^,]+")
    log$action_type[comments] <- "Comment"
  }
  log
}

all_missing_codes <- function(){
  data.frame(
    code = c(
      'NI',
      'INV',
      'UNK',
      'NASK',
      'ASKU',
      'NAV',
      'MSK',
      'NA',
      'NAVU',
      'NP',
      'QS',
      'QI',
      'TRC',
      'UNC',
      'DER',
      'PINF',
      'NINF',
      'OTH'
    ),
    name = c(
      'No information',
      'Invalid',
      'Unknown',
      'Not asked',
      'Asked but unknown',
      'Temporarily unavailable',
      'Masked',
      'Not applicable',
      'Not available',
      'Not present',
      'Sufficient quantity',
      'Insufficient quantity',
      'Trace',
      'Unencoded',
      'Derived',
      'Positive infinity',
      'Negative infinity',
      'Other'
    )
  )
}

missing_codes2 <- function(DB){
  included <- "missing_data_codes"%in%colnames(DB$redcap$project_info)
  if(included){
    is_na <-is.na(DB$redcap$project_info$missing_data_codes)
    if(!is_na){
      return(DB$redcap$project_info$missing_data_codes %>% split_choices())
    }
    if(is_na){
      return(NA)
    }
  }
  if(!included){
    return(NA)
  }
}

#' @title Merge non-repeating, not ready for multi-event projects
#' @inheritParams save_DB
#' @return DB object that has merged all non repeating forms
#' @export
merge_non_repeating_DB <- function(DB){
  if("megrged" %in% names(DB$data_extract))stop("Already merged!")
  instrument_names <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)] %>% as.list()
  if (length(instrument_names)==1) warning('No need to merge you only have one form that is non-repeating')
  merged <- DB$data_extract[[instrument_names[[1]]]]
  merged$redcap_event_name <- NULL
  # merged$arm_num <- NULL
  merged$event_name <- NULL
  merged$redcap_repeat_instrument <- NULL
  merged$redcap_repeat_instance <- NULL
  DB$data_extract[[instrument_names[[1]]]] <- NULL
  instrument_names[[1]]<-NULL
  while (length(instrument_names)>0) {
    dfx <- DB$data_extract[[instrument_names[[1]]]]
    dfx$redcap_event_name <- NULL
    # dfx$arm_num <- NULL
    dfx$event_name <- NULL
    dfx$redcap_repeat_instrument <- NULL
    dfx$redcap_repeat_instance <- NULL
    (in_common <- colnames(merged)[which(colnames(merged)%in%colnames(dfx))])
    merged <- merge(merged,dfx,by=in_common,all = T)
    DB$data_extract[[instrument_names[[1]]]] <- NULL
    instrument_names[[1]]<-NULL
  }
  DB$data_extract$merged <- merged
  DB
}

#' @title Unmerge non-repeating, not ready for multi-event projects
#' @inheritParams save_DB
#' @return DB object that has merged all non repeating forms
#' @export
unmerge_non_repeating_DB <- function(DB){
  if(!"merged" %in% names(DB$data_extract))stop("No DB$data_extract named as 'merged'!")
  instrument_names <- DB$data_extract$merged %>% colnames() %>% sapply(function(COL){DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name==COL)]}) %>% unique() %>% as.list()
  merged <- DB$data_extract$merged
  while (length(instrument_names)>0) {
    instrument_name <-instrument_names[[1]]
    DB$data_extract[[instrument_name]]<-merged[,unique(c(DB$redcap$id_col,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(merged))]))]
    instrument_names[[1]] <- NULL
  }
  DB$data_extract$merged<-NULL
  DB
}

#' @title Deidentify the REDCap DB according to REDCap or your choices
#' @inheritParams save_DB
#' @param identifiers optional character vector of column names that should be excluded from DB. Otherwise `DB$redcap$metadata$identifier =="y` will be used.
#' @return DB object that has deidentified forms
#' @export
deidentify_DB <- function(DB,identifiers){
  DB <- validate_DB(DB)
  missing_identifiers <- missing(identifiers)
  if(!missing_identifiers){
    identifiers <- identifiers %>% unique()
    bad_identifiers<-identifiers[which(!identifiers%in%DB$redcap$metadata$field_name)]
    if(length(bad_identifiers)>0)stop("You have an identifier that is not included in the set of `DB$redcap$metadata$field_name` --> ",bad_identifiers %>% paste0(collapse = ", "))
    if(DB$redcap$id_col%in%identifiers)stop("Your REDCap ID, ",DB$redcap$id_col,", should not be deidentified.") #If you want to pass a new set of random IDs to make this data use `scramble_ID_DB(DB)`.")
  }
  if(missing_identifiers){
    identifiers<- DB$redcap$metadata$field_name[which(DB$redcap$metadata$identifier=="y")]
    if(length(identifiers)==0)warning("You have no identifiers marked in `DB$redcap$metadata$identifier`. You can set it in REDCap Project Setup and update DB OR define your idenitifiers in this functions `identifiers` argument." ,immediate. = T)
  }
  drop_list <- Map(function(NAME, COLS) {identifiers[which(identifiers %in% COLS)]},names(DB$data_extract), lapply(DB$data_extract, colnames))
  drop_list <- drop_list[sapply(drop_list, length) > 0]
  if(length(drop_list)==0)message("Nothing to deidentify from --> ",identifiers %>% paste0(collapse = ", "))
  for (FORM in names(drop_list)) {
    for(DROP in drop_list[[FORM]]){
      DB$data_extract[[FORM]][[DROP]] <- NULL
      message("Dropped ",DROP," from ", FORM)
    }
  }
  DB
}

#' @title clean DB columns for plotting using the metadata
#' @description
#'  Turns choices into factors and integers to integer for table processing such as with table1 and plots
#' @inheritParams save_DB
#' @param drop_blanks logical for dropping n=0 choices
#' @param drop_unknowns logical for dropping missing codes
#' @param units_df data.frame with two columns: `field_name` in the metadata and `units` to set units
#' @return DB object cleaned for table or plots
#' @export
clean_DB <- function(DB,drop_blanks=T,drop_unknowns=T,units_df){
  metadata <- DB$redcap$metadata
  metadata$field_label[which(is.na(metadata$field_label))] <- metadata$field_name[which(is.na(metadata$field_label))]
  metadata <-unique(metadata$form_name) %>%
    lapply(function(IN){
      metadata[which(metadata$form_name==IN),]
    }) %>% dplyr::bind_rows()
  metadata$field_type_R <- NA
  metadata$field_type_R[which(metadata$field_type %in% c("radio","yesno","dropdown"))] <- "factor"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "integer")] <- "integer"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "date_mdy")] <- "date"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "date_ymd")] <- "date"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "datetime_dmy")] <- "datetime"
  DB$redcap$metadata <- metadata
  DB <- annotate_codebook(DB)
  here_is_units_df <- NULL
  if(!missing(units_df)){
    if(!is.data.frame(units_df))stop("units_df must be a dataframe")
    if(nrow(units_df)>0){
      here_is_units_df <- units_df
    }
  }
  for(FORM in names(DB$data_extract)){
    for(COLUMN in colnames(DB$data_extract[[FORM]])){
      if(COLUMN %in% metadata$field_name){
        units <- NULL
        if(!is.null(here_is_units_df)){
          if(COLUMN%in%units_df$field_name){
            units <- units_df$units[which(units_df$field_name==COLUMN)]
            if(length(units)>1)stop("only provide one unit per field name")
          }
        }
        class <- metadata$field_type_R[which(metadata$field_name==COLUMN)][[1]]
        label <- ifelse(is.na(metadata$field_label[which(metadata$field_name==COLUMN)]),COLUMN,metadata$field_label[which(metadata$field_name==COLUMN)])[[1]]
        levels <- NULL
        if(!is.na(class)){
          if(class == "factor"){
            levels <- (metadata$select_choices_or_calculations[which(metadata$field_name==COLUMN)] %>% split_choices())[[2]]
            if(any(duplicated(levels))){
              DUPS <- levels %>% duplicated() %>% which()
              warning("You have a variable (",COLUMN,") with dupplicate names (",levels[DUPS] %>% paste0(collapse = ", "),"). This is not great but for this proccess they will be merged and treated as identical responses.")
              levels <- levels %>% unique()
            }
            if(drop_blanks){
              levels <- levels[which(levels%in%unique(DB$data_extract[[FORM]][[COLUMN]]))]
            }
            if(!drop_unknowns){
              levels <- levels %>% append(unique(DB$data_extract[[FORM]][[COLUMN]])) %>% unique() %>% drop_nas()
            }
          }
          if(class == "integer"){

          }
          DB$data_extract[[FORM]]
        }
      }
      DB$data_extract[[FORM]][[COLUMN]]<-DB$data_extract[[FORM]][[COLUMN]] %>% clean_column_for_table(
        class = class,
        label = label,
        units = units,
        levels = levels
      )
    }
  }
  return(DB)
}

#' @title clean column for plotting; manual addition of clean_DB
#' @description
#'  Turns choices into factors and integers to integer for table processing such as with table1 and plots
#' @param col the column vector
#' @param class character for column type: integer, factor, numeric
#' @param label character for label
#' @param units character for units
#' @param levels character vector of levels for factor
#' @return cleaned column
#' @export
clean_column_for_table<-function(col,class,label,units,levels){
  if(!missing(class)){
    if(!is.null(class)){
      if(!is.na(class)){
        if(class=="integer"){
          col <-   col %>% as.integer()
        }
        if(class=="factor"){
          col <-   col %>% factor(levels = levels,ordered = T)
        }
        if(class=="numeric"){
          col <-   col %>% as.numeric()
        }
      }
    }
  }
  if(!missing(label)){
    attr(col, "label") <- label
  }
  if(!missing(units)){
    attr(col, "units") <- units
  }
  col
}

#' @title add REDCap ID to any dataframe using a ref_id
#' @description
#'  add REDCap ID to any dataframe using a ref_id
#' @param DF dataframe
#' @inheritParams save_DB
#' @param ref_id column name that matches a REDCap variable name that could be an ALT id such as MRN
#' @return original dataframe with REDCap id_col added as the first column
#' @export
add_ID_to_DF<-function(DF,DB,ref_id){
  if(!ref_id%in%DB$redcap$metadata$field_name)stop("The ref_id not valid. Must be a REDCap raw colname")
  form<-DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name==ref_id)]
  DF[[ref_id]] %>% sapply(function(ID){
    DB$data_extract[[form]][[DB$redcap$id_col]][which(DB$data_extract[[form]][[ref_id]]==ID)]
  }) %>% as.data.frame()->y
  colnames(y)<-"record_id"
  DF<-cbind(y,DF)
  DF
}

#' @title grab data table for an individual(s)
#' @description
#' grab data table for an individual(s)
#' @inheritParams select_redcap_records
#' @return list of data tables
#' @export
grab_record_tables<-function(DB, records){
  OUT <-list()
  for(TABLE in names(DB$data_extract)){
    OUT[[TABLE]] <-   DB$data_extract[[TABLE]][which(DB$data_extract[[TABLE]][[DB$redcap$id_col]]%in%records),]
  }
  OUT
}

split_choices<-function(x){
  oops <- x
  x<-gsub("\n", " | ",x)  #added this to account for redcap metadata output if not a number
  x<-x %>% strsplit(" [:|:] ") %>% unlist()
  check_length <- length(x)
  # code <- x %>% stringr::str_extract("^[^,]+(?=, )")
  # name <- x %>% stringr::str_extract("(?<=, ).*$")
  result <- x %>% stringr::str_match("([^,]+), (.*)")
  # x<-data.frame(
  #   code=x %>% strsplit(", ") %>% sapply(`[`, 1),
  #   name=x %>% strsplit(", ")%>% sapply(`[`, -1) %>% sapply(function(y){paste0(y,collapse = ", ")})
  # )
  x<-data.frame(
    code=result[,2],
    name=result[,3]
  )
  rownames(x)<-NULL
  if(nrow(x)!=check_length)stop("split choice error: ",oops)
  x
}

make_codebook<-function(DB){
  choices<-which(DB$redcap$metadata$field_type%in%c("radio","dropdown","checkbox_choice","yesno"))
  if(length(choices)>0){
    for(field in DB$redcap$metadata$field_name[choices]){
      DB[["choices"]][[field]]<-DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field)] %>% split_choices()
    }
  }
  OUT <- NULL
  for (CHOICE in names(DB[["choices"]])){
    x<-DB[["choices"]][[CHOICE]]
    x$field_name <- CHOICE
    OUT <- OUT %>% dplyr::bind_rows(x)
  }
  OUT<-OUT %>% dplyr::select("field_name","code","name")
  rownames(OUT) <- NULL
  OUT
}



# summarize ---------------


#toa and from dir -----------

#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param records character vector of records you want dropped to your directory
#' @param allow_mod logical for whether non-instrument names are allowed
#' @param deidentify logical for deidentification
#' @param dir_other optional character string of another file path where the files should be saved
#' @param only_redcap logical for whether to only include redcap and not metadata
#' @param append_name optional character string for adding to the front of file names
#' @param str_trunc_length optional integer for truncation
#' @param annotated_codebook optional logical for adding annoations of n and percent to codebook file
#' @param with_links optional logical for including links in excel sheets
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
    if(!file.exists(dir_other)){
      choice <- utils::menu(c("Yes","No"),title = "dir_other doesn't exist. Should I create?")
      if(choice==1){
        dir.create(dir_other)
      }
    }
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
    to_save <- names(DB$data_extract)
  }else{
    to_save <- DB$redcap$instruments$instrument_name
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
#' @param allow_all logical TF for allowing DB$data_extract names that are not also instrument names
#' @return messages for confirmation
#' @export
read_redcap_dir<-function(DB,allow_all=T){
  DB <- validate_DB(DB)
  path<-file.path(get_dir(DB),"REDCap","upload")
  if(!file.exists(path))stop("No REDCap files found at path --> ",path)
  x<-list.files.real(path)
  if(!allow_all){
    x<-x[which(gsub("\\.xlsx|\\.xls","",x)%in%DB$redcap$instruments$instrument_name)]
  }
  DB_import<-DB
  DB_import[["data"]]<-list()
  for(y in x){#not done yet
    DB_import[["data"]][[gsub("\\.xlsx","",y)]] <- readxl::read_xlsx(file.path(path,y)) %>% all_character_cols()
  }
  DB_import
}




# upload -----


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
  if(is.null(DB$data_extract))stop("`DB$data_extract` is empty")
  for(TABLE in names(DB$data_extract)){
    to_be_uploaded_raw <- DB$data_extract[[TABLE]]
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
  if(any(!names(DB_import[["data"]])%in%names(DB$data_extract)))stop("All file names and data.table names from your directory a.k.a. `names(DB_import$data)` must match the DB instrument names, `DB$redcap$instruments$instrument_name`")
  if(is.null(DB_import[["data"]]))stop("`DB_import$data` is empty")
  for(TABLE in names(DB_import[["data"]])){
    ref_cols <- DB$redcap$id_col
    if(TABLE%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]){
      ref_cols <- c(DB$redcap$id_col,"redcap_repeat_instrument","redcap_repeat_instance")
    }
    DB_import[["data"]][[TABLE]] <- find_df_diff(new= DB_import[["data"]][[TABLE]] , old =  DB$data_extract[[TABLE]], ref_cols = ref_cols)
  }
  DB_import
}



#link --------

#' @title Link to get a new API token for your project (if you access)
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
link_API_token<- function(DB){
  DB$links$redcap_API_link %>% utils::browseURL()
}

#' @title Link view the API playground for your project (if you access)
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
link_API_playground <- function(DB){
  DB$API_playground_link %>% utils::browseURL()
}

#' @title Link view the REDCap project home page
#' @inheritParams save_DB
#' @return opens browser link
#' @export
link_REDCap_home <- function(DB){
  DB$links$redcap_base_link %>% utils::browseURL()
}

#' @title Shows DB in the env
#' @inheritParams save_DB
#' @return opens browser link
#' @export
link_REDCap_project <- function(DB){
  DB$home_link %>% utils::browseURL()
}

#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param record REDCap record id or study id etc, any column names that match `DB$redcap$id_col`
#' @param page REDCap page for the record. Must be one of `DB$instruments$instrument_name`
#' @param instance REDCap instance if it's a repeating instrument
#' @return opens browser link
#' @export
link_REDCap_record <- function(DB,record,page,instance){
  link <- paste0(DB$links$redcap_base_link,"redcap_v",DB$version,"/DataEntry/record_home.php?pid=",DB$PID)
  if(!missing(record)){
    if(!record%in%DB$all_records[[DB$redcap$id_col]])stop(record," is not one of the records inside DB")
    if("arm_num"%in%colnames(DB$all_records)){
      link <- link %>% paste0("&arm=", DB$all_records$arm_num[which(DB$all_records$participant_id==record)])
    }
    link <- link %>% paste0("&id=",record)
  }
  if(!missing(page)){
    link <- gsub("record_home","index",link)
    if(!page%in%DB$instruments$instrument_name)stop(page," has to be one of the instrument names: ",paste0(DB$instruments$instrument_name,collapse = ", "))
    link <- link %>% paste0("&page=",page)
    if(!missing(instance)){
      if(!page%in%DB$instruments$instrument_name[which(DB$instruments$repeating)])stop("If you provide an instance, it has to be one of the repeating instrument names: ",paste0(DB$instruments$instrument_name[which(DB$instruments$repeating)],collapse = ", "))
      link <- link %>% paste0("&instance=",instance)
    }
  }
  link %>% utils::browseURL()
}



#upload --------

#' @title Shows DB in the env
#' @param DB DB from load_DB or setup_DB
#' @param force logical for force a fresh update
#' @param day_of_log numbers of days to be checked in the log
#' @param labelled logical for whether or not to return raw or labelled REDCap. Default is TRUE.
#' @param get_files logical for whether or not to get files from redcap.
#' @param original_file_names logical for whether or not to use original file names.
#' @return messages for confirmation
#' @export
update_DB<-function(DB,force=F,day_of_log = 10,labelled = T,get_files = F,original_file_names=F){
  IDs<-NULL
  DB <- validate_DB(DB)
  if(!is.null(DB$internals$data_extract_labelled)){
    if(DB$internals$data_extract_labelled!=labelled){
      if(!force){
        force <- T
        warning("The DB that was loaded was ",ifelse(DB$internals$data_extract_labelled,"labelled","RAW"), " and you chose ",ifelse(labelled,"labelled","RAW"),". Therefore, we will set force to TRUE for a full update of data to avoid data conflicts",immediate. = T)
      }
    }
  }
  DB <- test_redcap(DB)
  if(!force){
    if(is.null(DB$internals$last_metadata_update)||is.null(DB$internals$last_data_update)){
      force<-T
    }else{
      time<-c(DB$internals$last_metadata_update,DB$internals$last_data_update)
      time<-time %>% min() %>% as.character()
      ilog<-check_redcap_log(DB,begin_time = time)

      ilog3<-ilog[which(is.na(ilog$record_id)),]
      ilog3$timestamp<-NULL
      ilog3<-ilog3 %>% unique()
      ilog3<-ilog3[grep("export|download |edit report|Switch DAG|Copy report|Multi-Language|File Repository |custom record dashboard|User regenerate own API token|Create report",ilog3$details,ignore.case = T,invert = T) %>% unique(),]
      if(nrow(ilog3)>0){
        force<-T
        message(paste0("Update because: " ,ilog3$action, " - ", ilog3$details))
      }else{
        ilog2<-ilog[which(!is.na(ilog$record_id)),]
        ilog2$timestamp<-NULL
        ilog2<-ilog2 %>% unique()
        if(any(ilog2$action_type%in%c("Create","Delete"))){
          force<-T
        }else{
          IDs<-ilog2$record_id %>% unique()
          if(length(IDs)==0){IDs<-NULL}
        }
        # if(Sys.time()>=(DB$internals$last_metadata_update+lubridate::days(2))){
        #   force<-T
        # }#not needed anymore because of log check?
      }
    }
  }
  if(force){
    DB<-DB %>% get_redcap_metadata()
    DB<-DB %>% get_redcap_data(labelled = labelled)
    DB$log<-DB %>% check_redcap_log(last = day_of_log,units = "days") %>% unique()
    message("Full update!")
    DB %>% save_DB()
  }else{
    if(!is.null(IDs)){
      time<-c(DB$internals$last_metadata_update,DB$internals$last_data_update)
      time<-time %>% min() %>% magrittr::subtract(lubridate::minutes(3)) %>% as.character()
      DB2<-DB %>% get_redcap_data(labelled = labelled,records = IDs)
      DB$internals$last_metadata_update<-DB$internals$last_data_update<-DB2$last_data_update
      DB$log<-DB$log %>% dplyr::bind_rows(check_redcap_log(DB,begin_time = time)) %>% unique() %>% all_character_cols()
      for(TABLE  in names(DB$data_extract)){
        DB$data_extract[[TABLE]]<-DB$data_extract[[TABLE]][which(!DB$data_extract[[TABLE]][[DB$redcap$id_col]]%in%IDs),] %>% dplyr::bind_rows(DB2$data_extract[[TABLE]][which(DB2$data_extract[[TABLE]][[DB2$id_col]]%in%IDs),])
      }
      message("updated: ",paste0(IDs,collapse = ", "))
      DB %>% save_DB()
    }else{
      message("Up to date already!")
    }
  }
  if(get_files){
    get_redcap_files(DB,original_file_names=original_file_names)
  }
  DB
}

rmarkdown_DB <- function (DB,dir_other){

  if(missing(dir_other)){
    dir <- get_dir(DB) %>% file.path("output")
  }else{
    dir  <- dir_other
  }
  filename <- paste0(DB$short_name,"_full_summary_",gsub("-","_",Sys.Date()),".pdf")
  rmarkdown::render(
    input = system.file("rmarkdown","pdf.Rmd",package = pkg_name),
    output_format = "pdf_document",
    output_file = dir %>% file.path(filename),
    output_dir = dir,
    quiet = F
  )
}



# transfrom --------


generate_default_remap <- function(DB,merge_non_repeating_name='merged'){
  DB <- validate_DB(DB)
  if(is.data.frame(DB$redcap$metadata)){
    metadata_remap<-  DB$redcap$metadata
    metadata_remap$field_name_remap <- metadata_remap$field_name %>% replace_between_tails()
    metadata_remap$form_name_remap <- metadata_remap$form_name
    non_reps <- DB$instruments$instrument_name[which(!DB$instruments$repeating)]
    if(length(non_reps)>0){
      metadata_remap$form_name_remap[which( metadata_remap$form_name%in%non_reps)] <- merge_non_repeating_name
    }
    if(DB$has_event_mappings){
      # DB$events->x
      events <- DB$events
      event_mapping <- DB$event_mapping
      event_mapping <- merge(event_mapping, events[,c("event_name","unique_event_name")], by="unique_event_name")
      event_mapping$unique_event_name_remap <- tolower(gsub(" ","_",event_mapping$event_name))
      metadata_remap$unique_event_name_remap <-  metadata_remap$form_name%>% sapply(function(form_name){
        event_mapping$unique_event_name_remap[which(event_mapping$form==form_name)] %>% unique() %>% paste0(collapse = " | ")
      }) %>% unlist()
      # events %>% rio::export(file = DB$dir_path %>% file.path("input","events_remap_default.xlsx"))
      event_mapping %>% rio::export(file = DB$dir_path %>% file.path("input","event_mapping_remap_default.xlsx"))
    }
    metadata_remap %>% rio::export(file = DB$dir_path %>% file.path("input","metadata_remap_default.xlsx"))
  }
}
transform_DB<-function(DB,merge_non_repeating=T,merge_non_repeating_name = "merged",metadata_remap,event_mapping_remap){
  DB <-validate_DB(DB)
  transform <- list()
  if(missing(metadata_remap)){
    path<-DB$dir_path %>% file.path("input","metadata_remap.xlsx")
    if(!file.exists(path)){
      warning(paste0("No remap file at the following path: ",path), immediate. = T)
      path <- DB$dir_path %>% file.path("input","metadata_remap_default.xlsx")
    }
    transform$metadata_remap <- rio::import(path)
    # check it
    remappings <- transform$metadata_remap$field_name[which(transform$metadata_remap$field_name!=transform$metadata_remap$field_name_remap)]
    field_name <- remappings %>% sample(1)
    for(field_name in remappings){
      field_name_remap<-transform$metadata_remap$field_name_remap[which(transform$metadata_remap$field_name==field_name)]
      scc1 <- DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field_name_remap)]
      scc2 <- DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field_name)]
      mes <- paste0("unequal choices: ", field_name_remap," and ",field_name)
      if(!is.na(scc1)&is.na(scc2))stop(mes)
      if(is.na(scc1)&!is.na(scc2))stop(mes)
      if(!is.na(scc1)&!is.na(scc2)){
        if(scc1!=scc2)stop(mes)
      }
    }
    transform$metadata_final <- DB$redcap$metadata[which(DB$redcap$metadata$field_name%in%transform$metadata_remap$field_name_remap),]
    transform$metadata_final$form_name <- transform$metadata_final$field_name %>% sapply(function(field_name){
      x <-  transform$metadata_remap$form_name_remap[which(transform$metadata_remap$field_name_remap==field_name)] %>% unique()
      if(length(x)>1)stop("your remap cannot have variables that map to more than one form... --> ",field_name)
      x
    })

  }
  if(missing(event_mapping_remap)){
    path<-DB$dir_path %>% file.path("input","event_mapping_remap.xlsx")
    if(!file.exists(path)){
      warning(paste0("No remap file at the following path: ",path), immediate. = T)
      path <- DB$dir_path %>% file.path("input","event_mapping_remap_default.xlsx")
    }
    if(file.exists(path)){transform$event_mapping_remap <- rio::import(path)}
    #check
    transform$event_mapping_final <- transform$event_mapping_remap[,c("arm_num","form","event_name","unique_event_name_remap","unique_event_name")]
    transform$event_mapping_final$unique_event_name <- transform$event_mapping_final$unique_event_name_remap
    transform$event_mapping_final$unique_event_name_remap <- NULL
    form <- transform$event_mapping_final$form %>% sample(1)
    transform$event_mapping_final$form <-transform$event_mapping_final$form %>% sapply(function(form){
      x <-  transform$metadata_remap$form_name_remap[which(transform$metadata_remap$form_name==form)] %>% unique()
      if(length(x)>1)stop("your remap cannot have forms that map to more than one form... --> ",field_name)
      x
    })
    transform$event_mapping_final <- unique(transform$event_mapping_final)
    if(is_something(DB$arms,1)){
      test_df <- transform$event_mapping_final
      for(arm in DB$arms$arm_num){
        if(nrow(test_df)>0){
          check_these <-transform$event_mapping_final[which(transform$event_mapping_final$arm_num==arm),c("form","event_name","unique_event_name")]
          for(i in 1:nrow(check_these)){
            test_df <- test_df[which(!(test_df$form==check_these$form[i]&test_df$event_name==check_these$event_name[i]&test_df$unique_event_name==check_these$unique_event_name[i])),]
          }
        }
      }
      if(nrow(test_df)==0){
        transform$event_mapping_final$arm_num <- NULL
        transform$event_mapping_final <-   transform$event_mapping_final %>% unique()
      }
    }
    events <- unique(transform$event_mapping_final$unique_event_name)
    transform$events_final <- data.frame(
      unique_event_name = events,
      forms = events %>% sapply(function(event){
        transform$event_mapping_final$form[which(transform$event_mapping_final$unique_event_name==event)] %>% unique() %>% paste0(collapse = " | ")
      })
    )
  }

  transform$instruments <- data.frame(
    instrument_name =  transform$metadata_remap$form_name_remap %>% unique(),
    instrument_label = NA,
    repeating = NA
  )
  transform$instruments$instrument_label <-  transform$instruments$instrument_name %>% sapply(function(instrument_name){
    row <- which(DB$instruments$instrument_name==instrument_name)
    if(length(row)==0)return(instrument_name)
    return(DB$instruments$instrument_label[row])
  })
  if("unique_event_name_remap"%in%colnames(transform$metadata_remap)){
    transform$instruments$repeating[
      which(
        transform$instruments$instrument_name %>% sapply(function(instrument_name){
          # instrument_name <- DB$instruments$instrument_name %>% sample(1)
          events <- transform$metadata_remap$unique_event_name_remap[which(transform$metadata_remap$form_name_remap== instrument_name)] %>% unique() %>% strsplit(" [:|:] ") %>% unlist() %>% unique()
          length(events)>1
        })
      )
    ] <- T

    transform$instruments$repeating[
      which(
        transform$instruments$instrument_name %>% sapply(function(instrument_name){
          # instrument_name <- DB$instruments$instrument_name %>% sample(1)
          anyDuplicated(transform$event_mapping_remap$arm_num[which(transform$event_mapping_remap$unique_event_name_remap==instrument_name)])>0
        })
      )
    ] <- T

  }else{
    transform$instruments$repeating <-  transform$instruments$instrument_name %>% sapply(function(instrument_name){
      old_forms <- transform$metadata_remap$form_name[which(transform$metadata_remap$form_name_remap==instrument_name)] %>% unique()
      were_repeating <- old_forms %>% sapply(function(instrument_name){DB$instruments$repeating[which(DB$instruments$instrument_name==instrument_name)]})
      # if(any(were_repeating)){
      #
      # }
      if(length(row)==0)return(instrument_name)
      return(DB$instruments$instrument_label[row])
    })
  }

  # merge-------

  instrument_names <- DB$instruments$instrument_name[which(!DB$instruments$repeating)] %>% as.list()
  if (length(instrument_names)==1) warning('No need to merge you only have one form that is non-repeating')
  merged <- DB$data_extract[[instrument_names[[1]]]]
  merged$redcap_event_name <- NULL
  # merged$arm_num <- NULL
  merged$event_name <- NULL
  merged$redcap_repeat_instrument <- NULL
  merged$redcap_repeat_instance <- NULL
  DB$data_extract[[instrument_names[[1]]]] <- NULL
  instrument_names[[1]]<-NULL
  while (length(instrument_names)>0) {
    dfx <- DB$data_extract[[instrument_names[[1]]]]
    dfx$redcap_event_name <- NULL
    # dfx$arm_num <- NULL
    dfx$event_name <- NULL
    dfx$redcap_repeat_instrument <- NULL
    dfx$redcap_repeat_instance <- NULL
    (in_common <- colnames(merged)[which(colnames(merged)%in%colnames(dfx))])
    merged <- merge(merged,dfx,by=in_common,all = T)
    DB$data_extract[[instrument_names[[1]]]] <- NULL
    instrument_names[[1]]<-NULL
  }
  DB$data_extract$merged <- merged
  DB




  #project --------
  #records belong to arms 1 to 1 ----------
  transform$records_n <- 0
  if(!is.null(DB$all_records)){
    transform$records_n <- DB$all_records %>% length()
  }
  #arms----------------
  transform$arms_n <- 0
  if(is.data.frame(DB$arms)){
    transform$arms_n <- DB$arms %>% nrow()
    id_pairs <- DB$instruments$instrument_name %>%  lapply(function(IN){DB$data_extract[[IN]][,c(DB$redcap$id_col,"arm_num")]}) %>% dplyr::bind_rows() %>% unique()
    DB$arms$arm_records_n <- DB$arms$arm_num %>% sapply(function(arm){
      which(id_pairs$arm_num==arm)%>% length()
    })
  }
  #events belong to arms many to 1 ----------------
  # transform$events_n <- DB$events %>% nrow()
  transform$events_n <- 0
  if(is.data.frame(DB$events)){
    transform$events_n <- DB$events %>% nrow()
    transform$event_names_n <- DB$events$event_name %>% unique() %>% length()
    # 1:nrow(DB$event_mapping) %>% lapply(function(i){
    #   (DB$data_extract[[DB$event_mapping$form[i]]][['redcap_event_name']]==DB$event_mapping$unique_event_name[i]) %>% which() %>% length()
    # })
    # for(event in ){
    #   transform[[paste0(event,"_records_n")]] <- DB$data_extract[[]][which(DB$arms$arm_num==arm)]
    # }
  }

  #instruments/forms belong to events many to 1 (if no events/arms) ----------------
  transform$instruments_n <- 0
  if(is.data.frame(DB$instruments)){ # can add expected later
    transform$instruments_n <- DB$instruments %>% nrow()
    DB$instruments$incomplete <- DB$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Incomplete") %>% which() %>% length()
    })
    DB$instruments$unverified <- DB$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Unverified") %>% which() %>% length()
    })
    DB$instruments$complete <- DB$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Complete") %>% which() %>% length()
    })
  }

  #fields belong to instruments/forms 1 to 1 ----------------
  transform$metadata_n <- 0

  transform$metadata_n <- DB$redcap$metadata[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive")),] %>% nrow()
  # DB$redcap$metadata$field_type[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive"))] %>% table()

  #metadata/codebook =============
  metadata <- DB$redcap$metadata
  codebook <- DB$codebook %>% dplyr::select("field_name", "code", "name")

  codebook <- unique(metadata$field_name) %>%
    lapply(function(IN){
      codebook[which(codebook$field_name==IN),]
    }) %>% dplyr::bind_rows()

  codebook<-codebook %>% merge(
    metadata %>% dplyr::select(
      "form_name","field_name","field_label","field_type","field_type_R","text_validation_type_or_show_slider_number"),by="field_name",sort=F)
  codebook$form_name <- 1:nrow(codebook) %>% lapply(function(i){
    form_name <- codebook$form_name[i]
    field_name <- codebook$field_name[i]

    if(!form_name %in% names(DB$data_extract)){
      if("merged" %in% names(DB$data_extract)){
        if(field_name%in%colnames(DB$data_extract$merged))return("merged")
      }
      if("patient" %in% names(DB$data_extract)){
        if(field_name%in%colnames(DB$data_extract$patient))return("patient")
      }
      for(other in names(DB$data_extract)[which(!names(DB$data_extract)%in%DB$instruments$instrument_name)]){
        if(field_name%in%colnames(DB$data_extract[[other]]))return(other)
      }
    }
    return(form_name)
  }) %>% unlist()

  codebook$field_name_raw <- codebook$field_name
  codebook$field_name_raw[which(codebook$field_type=="checkbox_choice")]<-codebook$field_name[which(codebook$field_type=="checkbox_choice")] %>%
    strsplit("___") %>%
    sapply(function(X){X[[1]]})

  codebook$field_label_raw <- codebook$field_label
  codebook$field_label_raw[which(codebook$field_type=="checkbox_choice")]<-codebook$field_name_raw[which(codebook$field_type=="checkbox_choice")] %>%
    sapply(function(X){
      metadata$field_label[which(metadata$field_name==X)] %>% unique()
    })

  codebook$n <- 1:nrow(codebook) %>% lapply(function(i){
    sum(DB$data_extract[[codebook$form_name[i]]][,codebook$field_name[i]]==codebook$name[i],na.rm = T)
  }) %>% unlist()
  codebook$n_total <- 1:nrow(codebook) %>% lapply(function(i){
    sum(!is.na(DB$data_extract[[codebook$form_name[i]]][,codebook$field_name[i]]),na.rm = T)
  }) %>% unlist()
  codebook$perc <-  (codebook$n/codebook$n_total) %>% round(4)
  codebook$perc_text <- codebook$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
  DB$codebook <- codebook
  return(DB)
}

























find_in_DB <- function(DB,text, exact = F){
  DB <- validate_DB(DB)
  out <- data.frame(
    record_id = character(0),
    col = character(0),
    row = character(0)
  )
  if (!exact){
    text <- tolower(text)
  }
  for(form in names(DB$data_extract)){
    DF <- DB$data_extract[[form]]
    for(col in colnames(DF)){
      if (!exact){
        DF[[col]] <- tolower(DF[[col]])
      }
      rows <- which(grepl(text,DF[[col]]))
      if(length(rows)>0){
        out <- out %>%dplyr::bind_rows(
          data.frame(
            record_id = DF[[DB$redcap$id_col]][rows],
            col = col,
            row = as.character(rows)
          )
        )
      }
    }
  }
  return(out)
}


extract_instrument_from_merged <- function(DB,instrument_name){
  merged <- DB$data_extract$merged
  if(nrow(merged)>0){
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
    add_ons <-add_ons[which(add_ons%in%colnames(merged))]
    if(!instrument_name%in%DB$instruments$instrument_name)stop("instrument_name must be included in set of DB$instruments$instrument_name")
    #instrument_name<- DB$instruments$instrument_name %>% sample(1)
    is_repeating_instrument <- instrument_name%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]
    rows <-1:nrow(merged)
    if(is_repeating_instrument){
      # rows <- which(merged$redcap_repeat_instrument==instrument_name)
    }
    if(!is_repeating_instrument){
      rows <- which(!is.na(merged[[paste0(instrument_name,"_complete")]]))
    }
    #
    # if(!DB$has_event_mappings){
    #   if("redcap_repeat_instrument"%in%colnames(merged)){
    #     if(is_repeating_instrument){
    #       rows <- which(merged$redcap_repeat_instrument==instrument_name)
    #     }
    #     if(!is_repeating_instrument){
    #       rows <- which(is.na(merged$redcap_repeat_instrument))
    #     }
    #   }
    # }
    # if(DB$has_event_mappings){
    #   events_ins <- DB$event_mapping$unique_event_name[which(DB$event_mapping$form==instrument_name)] %>% unique()
    #   rows <- which(merged$redcap_event_name%in%events_ins)
    # }
    # if(!is_repeating_instrument){
    #   add_ons <- add_ons[which(!add_ons%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
    # }
    cols <- unique(c(add_ons,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(merged))]))
    return(merged[rows,cols])
  }
}



# utils ----------

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL



write_xl<-function(DF,DB,path,str_trunc_length=32000,with_links = T){# add instance links
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "sheet")
  COL<-which(colnames(DF)==DB$redcap$id_col)
  DF <-  DF %>% lapply(stringr::str_trunc, str_trunc_length, ellipsis = "") %>% as.data.frame()
  if(nrow(DF)>0&&length(COL)>0&&with_links){
    DF$redcap_link<-paste0("https://redcap.miami.edu/redcap_v",DB$version,"/DataEntry/record_home.php?pid=",DB$PID,"&id=",DF[[DB$redcap$id_col]])
    if("arm_num"%in%colnames(DF)){
      DF$redcap_link <- DF$redcap_link %>% paste0("&arm=", DF[["arm_num"]])
    }
    class(DF$redcap_link) <- "hyperlink"
    openxlsx::writeData(wb, sheet = 1, x = DF$redcap_link,startRow = 2,startCol = COL)
    DF$redcap_link<-NULL
  }
  openxlsx::writeData(wb, sheet = 1, x = DF)
  openxlsx::saveWorkbook(
    wb = wb,
    file = path, overwrite = TRUE)
  message("Saved at -> ","'",path,"'")
}

list.files.real<-function(path){
  grep('~$', list.files(path), fixed = TRUE, value = TRUE, invert = TRUE)
}

validate_env_name <- function(env_name) {
  # Check if the name is empty
  if(is.null(env_name)) stop("env_name is NULL")
  if (nchar(env_name) == 0) {
    stop("Short name cannot be empty.")
  }
  # Check if the name starts with a number
  if (grepl("^\\d", env_name)) {
    stop("Short name cannot start with a number.")
  }
  # Check if the name contains any invalid characters
  if (grepl("[^A-Za-z0-9_]", env_name)) {
    stop("Short name can only contain letters, numbers, and underscores.")
  }
  return(env_name)
}

validate_web_link <- function(link) {
  if(is.null(link)) stop("link is NULL")

  # Check if the link starts with "https://" or "http://"
  if (!grepl("^https?://", link)) {
    stop("Invalid web link. It must start with 'http://' or 'https://'.")
  }
  # Remove trailing slash if present
  link <- gsub("/$", "", link)
  # Check if the link ends with one of the specified web endings
  if (!grepl("\\.(edu|com|org|net|gov|io|xyz|info|co|uk)$", link)) {
    stop("Invalid web link. It must end with a valid web ending (.edu, .com, etc.).")
  }
  # Add a trailing slash
  link <- paste0(link, "/")
  return(link)
}

#' @title find the difference between two data.frames
#' @description
#' This function will compare two data.frames: new and old.
#' You define the reference columns with ref_cols.
#' Reference columns are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @param new a new data.frame to compare to old. All new cols must be included in the set of the old ones.
#' @param old a reference data.frame to be compared to
#' @param ref_cols character vector of reference columns. They are always included in the return data.frame and their combination should always lead to a unique key for each row.
#' @return messages and data.frame of only changes and reference cols
#' @export
find_df_diff <- function (new, old,ref_cols=NULL){
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new df columns must be included in old df")
  }
  if (!all(ref_cols %in% colnames(new))| !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols)>1){
    new$key <- apply( new[ , ref_cols] , 1 , paste , collapse = "_" )
    old$key <- apply( old[ , ref_cols ] , 1 , paste , collapse = "_" )
  }else{
    new$key <- new[ , ref_cols]
    old$key <- old[ , ref_cols]
  }
  if (anyDuplicated(old$key) > 0) {
    stop("Keys must lead to unique rows! (old df)")
  }
  if (anyDuplicated(new$key) > 0) {
    stop("Keys must lead to unique rows! (new df)")
  }
  new_keys <- integer(0)
  if(any(!new$key %in% old$key)){
    warning("You have at least one new key compared to old df therefore all columns will be included by default",immediate. = T)
    new_keys <- which(!new$key %in% old$key)
  }
  indices <- data.frame(
    row = integer(0),
    col = integer(0)
  )
  if(length(new_keys)>0){
    indices <- indices %>% dplyr::bind_rows(
      data.frame(
        row = new_keys,
        col = which(!colnames(new)%in%c(ref_cols,"key"))
      )
    )
  }
  for (KEY in new$key[which(new$key%in%old$key)]){
    row <- which(new$key == KEY)
    row_old <- which(old$key == KEY)
    for (COL in colnames(new)[which(!colnames(new)%in%c(ref_cols,"key"))]){
      col <- which(colnames(new) == COL)
      if(!identical(new[row,COL],old[row_old,COL])){
        indices <- indices %>% dplyr::bind_rows(
          data.frame(
            row = row,
            col = col
          )
        )
      }
    }
  }

  if(nrow(indices)>0){
    rows <- indices$row %>% unique() %>% sort()
    cols <- which(colnames(new)%in%ref_cols) %>% append(indices$col %>% unique() %>% sort())
    OUT <- new[rows,cols]
    message(nrow(OUT), " rows have updates")
  }else{
    OUT <- NULL
    message("No changes!")
  }
  OUT
}

count_DB_cells <- function(DB){
  DB$data_extract %>% lapply(function(x){nrow(x)*ncol(x)}) %>% unlist() %>% sum()
}

all_character_cols <- function(df){
  as.data.frame(lapply(df,as.character))
}

addSlashIfNeeded <- function(input_string) {
  if (!endsWith(input_string, "/")) {
    output_string <- gsub("$", "/", input_string)
  } else {
    output_string <- input_string
  }
  return(output_string)
}

remove_html_tags <- function(text_vector) {
  # Regular expression to match HTML tags
  html_pattern <- "<[^>]+>"
  # Use gsub to remove the HTML tags from each element in the vector
  cleaned_vector <- gsub(html_pattern, "", text_vector)
  return(cleaned_vector)
}

drop_nas <- function(x) {
  x[!sapply(x, is.na)]
}

full.file.info <- function(path,showWarnings = T) {
  if(showWarnings){
    if(! file.exists(path))warning("path does not exist: ",path,immediate. = T)
  }
  file_info <- data.frame(
    file = list.files(path) ,
    path = list.files(path, full.names = T)
  )
  file_info <- cbind(
    file_info,
    file.info(file_info$path)
  )
  rownames(file_info) <- NULL
  return(file_info)
}

sync_dir <- function(from,to,top_level=T){
  if(top_level){
    if(!file.exists(from))stop("from path '",from, "' doesn't exist")
    if(!file.info(from)[["isdir"]])stop("from path '",from, "' must be a folder")
    if(!file.exists(to)){
      dir.create(to,showWarnings = F)
    }#stop("to path '",to, "' doesn't exist")
    if(!file.info(to)[["isdir"]])stop("to path '",to, "' must be a folder")
  }
  file_info_from <- full.file.info(from)
  file_info_to <- full.file.info(to,showWarnings=F)
  if(nrow(file_info_from)>0){
    for(i in 1:nrow(file_info_from)){
      file_from <- file_info_from$file[i]
      isdir_from <- file_info_from$isdir[i]
      path_from <- file_info_from$path[i]
      mtime_from <- file_info_from$mtime[i]
      COPY_TF <- T
      add_or_update <- "Adding"
      MATCHING_FILE_ROW <- which(file_info_to$file==file_from)
      if(length(MATCHING_FILE_ROW)>0){
        if(length(MATCHING_FILE_ROW)>1){stop("Strange case of from and to file names seen more than once")}
        isdir_to <- file_info_to$isdir[MATCHING_FILE_ROW]
        if(isdir_to!=isdir_from){stop("Strange case of from and to paths not being both file-file or folder-folder")}
        add_or_update <- "Updating"
        file_to <- file_info_to$file[MATCHING_FILE_ROW]
        path_to <- file_info_to$path[MATCHING_FILE_ROW]
        mtime_to <- file_info_to$mtime[MATCHING_FILE_ROW]
        if(isdir_from){
          COPY_TF <- F # no need to copy folders that exist
        }
        if(!isdir_from){#if it's a file... check mtimes
          COPY_TF <- mtime_from > mtime_to
        }
      }
      if(COPY_TF){
        file.copy(
          from = path_from,
          to = to,
          overwrite = T,
          recursive = T
        )
        message(add_or_update," file: ",file_from, " to '", to, "'")
      }
      if(!COPY_TF&&isdir_from){
        sync_dir( #recursive dive down if it's a folder
          from = path_from,
          to = path_to,
          top_level = F
        )
      }
    }
  }else{
    warning(from, " is empty!",immediate. = T)
  }
  if(top_level){message("Up to date!")}
}

is_something <- function(thing,row=0){
  out <- F
  if(!is.null(thing)){
    if(is.data.frame(thing)){
      if(nrow(thing)>row){
        out <- T
      }
    }else{
      if(!is.na(thing)){
        if(length(thing)>0){
          out <- T
        }
      }
    }
  }
  return(out)
}
