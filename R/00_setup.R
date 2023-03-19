cache<-NULL
.onLoad<-function(libname, pkgname){
  x<-hoardr::hoard()
  x$cache_path_set(.packageName,type="user_cache_dir")
  cache<<-x
}

validate_dir <- function(dir_path,silent=T){
  #param check
  if ( ! is.character(dir_path)) stop("dir_path parameter must be character")
  if ( ! file.exists(dir_path)) stop("dir_path does not exist")
  if ( ! is.logical(silent)) stop("silent parameter must be T/F")
  #function
  if( ! silent) message("directory --> '",dir_path,"'")
  dir_path<-normalizePath(dir_path)
  stop_mes<-"Did you use `set_dir()`?"
  for(folder in c("R_objects","REDCap","output")){
    if ( ! file.exists(file.path(dir_path,folder))) stop("'",dir_path,"/",folder,"' missing! ",stop_mes)
  }
  # if ( ! file.exists(file.path(dir_path,"ref_tables"))) stop("'",dir_path,"/ref_tables' missing! ",stop_mes)
  if( ! silent) message("Directory is Valid!")
}

#' @title set your  directory
#' @param dir_path your absolute path to the  directory
#' @export
set_dir <- function(dir_path){
  #param check
  if ( ! is.character(dir_path)) stop("dir must be a character string")
  #function
  dir_path <- trimws(dir_path)

  dir_path <- normalizePath(dir_path, winslash = "/",mustWork = F)
  if(!file.exists(dir_path)){
    if(utils::menu(choices = c("Yes","No"),title = "No file path found, create?")==1){
      dir.create(file.path(dir_path))
    }
  }
  if ( ! file.exists(dir_path)) {
    stop("Path not found. Use absolute path or choose one within R project working directory.")
  }
  cache$mkdir()
  cache_path <- suppressWarnings(
    normalizePath(
      file.path(cache$cache_path_get())
    )
  )
  if (file.exists(file.path(cache_path,"dir_path.rds"))){
    if (file.exists(readRDS(file.path(cache_path,"dir_path.rds")))){
      if (dir_path!=readRDS(file.path(cache_path,"dir_path.rds"))){
        answer<-utils::menu(c("Yes","No"),title = paste0("There is already a cached path at '",readRDS(file.path(cache_path,"dir_path.rds")),"'. Would you like to continue?"))
        if (answer == 2){
          validate_dir(readRDS(file.path(cache_path,"dir_path.rds")),T)
          stop("You chose to stop because there is already an exsisting directory.")
        }
      }
    }
  }
  for(folder in c("R_objects","REDCap","output")){
    dir.create(file.path(dir_path,folder),showWarnings = F)
  }

  validate_dir(dir_path,silent=F)
  saveRDS(dir_path, file = file.path(cache_path,"dir_path.rds"))
  message("saved to cache")
}

#' @title get your directory
#' @export
get_dir <- function(){
  cache$mkdir()
  cache_path <- suppressWarnings(
    normalizePath(
      file.path(cache$cache_path_get())
    )
  )
  stop_mes<-"Did you use `set_dir()`?"
  if ( ! file.exists(cache_path)) stop(paste0("No cache. ", stop_mes))
  if ( ! file.exists(file.path(cache_path,"dir_path.rds"))) stop(paste0("No directory stored in cache. ", stop_mes))
  dir_path<-readRDS(file.path(cache_path,"dir_path.rds"))
  if ( ! file.exists(dir_path)) {
    warning("Searched for cached directory --> '",dir_path,"' ...")
    stop(paste0("Does not exist. ", stop_mes))
  }
  validate_dir(dir_path,silent=T)
  dir_path
}


