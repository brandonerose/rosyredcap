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

#' @export
cache_clear<- function(){
  cache$delete_all()
}

cache_projects_exists<- function(){
  if(cache_exists()){
    cache_path() %>% file.path("projects.rds") %>% file.exists()
  }else{
    warning("Cache doesn't exist",immediate. = T)
    FALSE
  }
}

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
  for(folder in c("R_objects","REDCap","output")){
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
#' @title set your  directory
#' @param dir_path your absolute path to the  directory
#' @export
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
  for(folder in c("R_objects","REDCap","output")){
    if ( ! file.exists(file.path(dir_path,folder))) {
      dir.create(file.path(dir_path,folder),showWarnings = F)
    }
  }
  validate_dir(dir_path,silent=F)
}

#' @title get your directory
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
