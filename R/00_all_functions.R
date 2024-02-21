# setup-----

cache <- NULL

.onLoad <- function(libname, pkgname){
  x <- hoardr::hoard()
  x$cache_path_set(path=.packageName,type="user_cache_dir")
  x$mkdir()
  cache <<-x
}

cache_path <- function(){
  cache$cache_path_get() %>% normalizePath()
}

cache_exists <-  function(){
  cache_path() %>% file.exists()
}

#' @title Clear your cached projects
#' @return message confirmation
#' @export
cache_clear <-  function(){
  cache$delete_all()
  message("Cache cleared!")
}

cache_projects_exists <-  function(){
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
    projects <- cache_path() %>% file.path("projects.rds") %>% readRDS()
    return(projects)
  }else{
    "You have no projects cached. Try `add_project`" %>% message()
    return(
      blank_projects()
    )
  }
}

blank_project_cols <- function(){
  c(
    "short_name",
    "dir_path",
    "token_name",
    "project_id",
    "redcap_version",
    "project_title",
    "last_metadata_update",
    "last_data_update",
    "redcap_home_link",
    "redcap_API_playground_link"
  )
}

blank_projects <- function(){
  x <- matrix(data = character(0),ncol = length(blank_project_cols())) %>% as.data.frame()
  colnames(x) <- blank_project_cols()
  x
}

add_project <- function(DB){
  projects <- get_projects()
  projects <- projects[which(projects$short_name!=DB$short_name),]
  OUT <- data.frame(
    short_name = DB$short_name,
    dir_path = DB$dir_path %>% is.null() %>% ifelse(NA,DB$dir_path),
    token_name = DB$token_name,
    project_id = DB$redcap$project_id,
    redcap_version = DB$redcap$version,
    project_title = DB$redcap$project_title,
    last_metadata_update = DB$internals$last_metadata_update,
    last_data_update = DB$internals$last_data_update,
    redcap_home_link = DB$links$redcap_home,
    redcap_API_playground_link =  DB$links$redcap_API_playground
  ) %>% all_character_cols()
  colnames(OUT) <- blank_project_cols()
  rownames(OUT) <- NULL
  OUT
  projects <- projects %>% dplyr::bind_rows(OUT)
  saveRDS(projects, file = cache$cache_path_get() %>% normalizePath() %>% file.path("projects.rds"))
}

validate_dir <- function(dir_path,silent=T){
  #param check
  dir_path <- clean_dir_path(dir_path)
  if ( ! file.exists(dir_path)) stop("dir_path does not exist")
  if ( ! is.logical(silent)) stop("silent parameter must be T/F")
  #function
  if( ! silent) message("directory --> '",dir_path,"'")
  stop_mes <- "Did you use `set_dir()`?"
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
  dir_path <- clean_dir_path(dir_path)
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
  stop_mes <- "Did you use `set_dir()`?"
  if ( ! file.exists(dir_path)) {
    warning("Searched for directory --> '",dir_path,"' ...")
    stop(paste0("Does not exist. ", stop_mes))
  }
  # if()
  validate_dir(dir_path,silent=T)
  dir_path
}

# tokens ----------

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
    message("Try going to REDCap --> '",DB$links$redcap_API,"' run `link_API_token(DB)")
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
    warning("You set an invalid token. Try going to REDCap --> '",DB$links$redcap_API,"' or run `link_API_token(DB)`",immediate. = T)
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
  DB  <- validate_DB(DB)
  validate_redcap_token(DB,silent = F,return = T)
}

# DB ---------

#' @title blank DB object
#' @return blank_DB list for reference
blank_DB <-  function(){ # can sort this better in version 3.0.0
  list(
    short_name=NULL,
    token_name=NULL,
    dir_path=NULL,
    internals = list(
      last_metadata_check=NULL,
      last_metadata_update=NULL,
      last_metadata_dir_save=NULL,
      last_data_check=NULL,
      last_data_update=NULL,
      last_data_dir_save = NULL,
      last_data_transformation = NULL,
      last_directory_save=NULL,
      data_extract_labelled = NULL,
      data_extract_merged = NULL,
      merge_form_name = "merged",
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
      has_arms_that_matter = NULL,
      has_repeating_instruments_or_events = NULL,
      has_repeating_instruments = NULL,
      has_repeating_events = NULL
    ),
    auto_jobs = NULL,
    remap = list(
      metadata_remap=NULL,
      metadata_new=NULL,
      instruments_remap=NULL,
      instruments_new=NULL,
      arms_map=NULL,
      arms_new=NULL,
      events_remap=NULL,
      events_new=NULL,
      event_mapping_remap=NULL,
      event_mapping_new=NULL
    ),
    data_extract = NULL,
    data_transform = NULL,
    data_upload = NULL,
    all_records = NULL,
    links = list(
      redcap_base = NULL,
      redcap_uri = NULL,
      redcap_home = NULL,
      redcap_records = NULL,
      redcap_API = NULL,
      redcap_API_playground = NULL,
      github = "https://github.com/brandonerose/rosyredcap",
      thecodingdocs = "https://www.thecodingdocs.com/"
    )
  )
}

validate_DB <- function(DB,silent = T){
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
  missing_dir_path <- missing(dir_path)
  if(missing_dir_path){
    warning("If you don't supply a directory, rosyredcap will only run in R session. Package is best with a directory",immediate. = T)
    DB <- blank_DB()
  }
  if(!missing_dir_path){
    dir_path <- set_dir(dir_path)
    DB <- load_DB(dir_path,blank = force)
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
load_DB <- function(dir_path,blank=F){
  if(blank){
    DB <- blank_DB()
  }else{
    DB_path <- file.path(dir_path,"R_objects","DB.rdata")
    if(file.exists(DB_path)){
      DB  <- readRDS(file=DB_path)
      validate_DB(DB)
    }else{
      DB <- blank_DB()
    }
  }
  DB
}

#' @title Saves DB in the directory
#' @param DB object generated using `load_DB()` or `setup_DB()`
#' @return Message
#' @export
save_DB <- function(DB){
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
#' @param transform show the data_transform section instead of data_extract
#' @export
show_DB <- function(DB,also_metadata=T,transform = T){
  DB <- validate_DB(DB)
  data_list <- list()
  data_choice <- "data_extract"
  if(transform)data_choice <- "data_transform"
  for(NAME in names(DB[[data_choice]])){
    L <- list(DB[[data_choice]][[NAME]])
    names(L) <- NAME
    data_list <- data_list %>% append(L)
  }
  if(also_metadata){
    for(NAME in c("metadata","instruments","arms","events","event_mapping","log","users","codebook","project_info")){#"unique_events"
      L <- list(DB$redcap[[NAME]])
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
  DIR  <- validate_dir(DIR,silent = F)
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
  if(is.data.frame(DB$redcap$arms)){
    if(nrow(DB$redcap$arms)>1){
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

# redcap API-----------

redcap_api_base <- function(url,token,content,additional_args=NULL){
  body  <- list(
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
        general_error <- response$status_code
        specific_error <- http_errors$Description[which(http_errors$Value==response$status_code)]
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
  ERROR  <- T
  while(ERROR){
    version <- redcap_api_base(DB$links$redcap_uri,validate_redcap_token(DB),"version")
    ERROR  <- version %>% httr::http_error()
    if(ERROR){
      warning('Your REDCap API token check failed. Invalid token or API privileges. Contact Admin! Consider rerunnning `setup_DB()`',immediate. = T)
      warning("HTTP error ",version %>% httr::status_code(), ". Check your token, internet connection, and redcap base link.",immediate. = T)
      message("Try going to REDCap --> '",DB$links$redcap_API,"' or run `link_API_token(DB)`")
      set_redcap_token(DB)
    }
  }
  message("Connected to REDCap!")
  DB$redcap$version <- version %>% httr::content(as="text") %>% as.character()
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
        file_name  <- form[[field_name]][i]
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

get_redcap_metadata <- function(DB){
  DB$internals$last_metadata_update <- Sys.time()
  # info ----------
  DB$redcap$project_info <- get_redcap_info(DB,"project")
  DB$redcap$project_title <-  DB$redcap$project_info$project_title
  DB$redcap$project_id <- DB$redcap$project_info$project_id
  DB$redcap$is_longitudinal <- DB$redcap$project_info$is_longitudinal == "1"
  DB$redcap$missing_codes <- missing_codes2(DB)

  #instruments --------
  DB$redcap$instruments <- get_redcap_info(DB,"instrument","warn")
  DB$redcap$instruments$repeating <- F
  DB$redcap$has_repeating_instruments <- F
  DB$redcap$has_repeating_events <- F
  DB$redcap$has_repeating_instruments_or_events <- DB$redcap$project_info$has_repeating_instruments_or_events=="1"
  # if(DB$redcap$project_info$has_repeating_instruments_or_events=="1")
  repeating <- get_redcap_info(DB,"repeatingFormsEvents")
  if(is.data.frame(repeating)){
    DB$redcap$instruments$repeating <- DB$redcap$instruments$instrument_name%in%repeating$form_name
    #   DB$redcap$metadata <- DB$redcap$metadata %>%dplyr::bind_rows(
    #     data.frame(
    #       field_name="redcap_repeat_instance",form_name=DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)] ,field_label="REDCap Repeat Instance",field_type="text",select_choices_or_calculations=NA
    #     )
    #   ) %>% unique()
    #   DB$redcap$metadata <- DB$redcap$metadata %>%dplyr::bind_rows(
    #     data.frame(
    #       field_name="redcap_repeat_instrument",form_name=DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)] ,field_label="REDCap Repeat Instrument",field_type="text",select_choices_or_calculations=NA
    #     )
    #   ) %>% unique()
  }
  if(any(DB$redcap$instruments$repeating)){
    DB$redcap$has_repeating_instruments <- T
  }
  #metadata ----------
  DB$redcap$metadata <- get_redcap_info(DB,"metadata","stop")
  DB$redcap$metadata$section_header <- DB$redcap$metadata$section_header %>% remove_html_tags()
  DB$redcap$metadata$field_label <- DB$redcap$metadata$field_label %>% remove_html_tags()
  DB$redcap$id_col <- DB$redcap$metadata[1,1] %>% as.character() #RISKY?

  DB$redcap$metadata <- DB$redcap$metadata %>%
    dplyr::bind_rows(
      data.frame(
        field_name = paste0(unique(DB$redcap$instruments$instrument_name),"_complete"),
        form_name = unique(DB$redcap$instruments$instrument_name),
        field_type = "radio",
        select_choices_or_calculations = "0, Incomplete | 1, Unverified | 2, Complete"
      )
    ) %>% unique()
  if(any(DB$redcap$metadata$field_type=="checkbox")){
    for(field_name in DB$redcap$metadata$field_name[which(DB$redcap$metadata$field_type=="checkbox")]){
      x <- DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field_name)] %>% split_choices()
      DB$redcap$metadata <- DB$redcap$metadata %>%dplyr::bind_rows(
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

  #other-------
  # is longitudinal ------
  if(DB$redcap$is_longitudinal){
    DB$redcap$arms <- get_redcap_info(DB,"arm")
    DB$redcap$has_arms <- T
    DB$redcap$has_multiple_arms <- nrow(DB$redcap$arms)>1
    DB$redcap$has_arms_that_matter <- DB$redcap$has_multiple_arms
    if(DB$redcap$has_arms_that_matter){
      DB$redcap$has_arms_that_matter<- DB$redcap$arms$arm_num %>% lapply(function(arm){
        DB$redcap$event_mapping$form[which(DB$redcap$event_mapping$arm_num==arm)]
      }) %>% check_match() %>% magrittr::not()
    }
    DB$redcap$event_mapping  <- get_redcap_info(DB,"formEventMapping","warn")
    DB$redcap$events <- get_redcap_info(DB,"event","warn")
    DB$redcap$events$forms <- DB$redcap$events$unique_event_name %>% sapply(function(events){
      DB$redcap$event_mapping$form[which(DB$redcap$event_mapping$unique_event_name==events)] %>% unique() %>% paste0(collapse = " | ")
    })
    # if(is.data.frame(DB$unique_events)){
    #   DB$redcap$events <- data.frame(
    #     event_name = unique(DB$unique_events$event_name),
    #     arms = unique(DB$unique_events$event_name) %>% sapply(function(event_name){
    #       DB$unique_events$arm_num[which(DB$unique_events$event_name==event_name)] %>% unique() %>% paste0(collapse = " | ")
    #     })
    #   )
    # }
    DB$redcap$instruments$repeating_via_events <- F
    DB$redcap$instruments$repeating_via_events[
      which(
        DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
          # instrument_name <- DB$redcap$instruments$instrument_name %>% sample(1)
          anyDuplicated(DB$redcap$event_mapping$arm_num[which(DB$redcap$event_mapping$form==instrument_name)])>0
        })
      )
    ] <- T
  }else{
    DB$redcap$has_arms <- F
    DB$redcap$has_multiple_arms <- NA
    DB$redcap$has_arms_that_matter <- NA
    DB$redcap$event_mapping  <- NA
    DB$redcap$events <- NA
  }
  DB$redcap$users <- get_redcap_users(DB)
  DB$redcap$codebook <- make_codebook(DB)
  DB$redcap$log <- check_redcap_log(DB,last = 2,units = "mins")
  DB$redcap$users$current_user <- DB$redcap$users$username==DB$redcap$log$username[which(DB$redcap$log$details=="Export REDCap version (API)") %>% dplyr::first()]
  DB$links$redcap_home <- paste0(DB$links$redcap_base_link,"redcap_v",DB$redcap$version,"/index.php?pid=",DB$redcap$project_id)
  DB$links$redcap_records <- paste0(DB$links$redcap_base_link,"redcap_v",DB$redcap$version,"/DataEntry/record_status_dashboard.php?pid=",DB$redcap$project_id)
  DB$links$redcap_API <- paste0(DB$links$redcap_base_link,"redcap_v",DB$redcap$version,"/API/project_api.php?pid=",DB$redcap$project_id)
  DB$links$redcap_API_playground <- paste0(DB$links$redcap_base_link,"redcap_v",DB$redcap$version,"/API/playground.php?pid=",DB$redcap$project_id)
  DB
}
check_match <- function(vec_list) {
  sorted_vecs <- lapply(vec_list, sort)
  all(sapply(sorted_vecs[-1], function(x) identical(sorted_vecs[[1]], x)))
}

get_redcap_data <- function(DB,labelled=T,records=NULL){
  DB$internals$last_data_update <- Sys.time()
  raw <- get_raw_redcap(
    DB = DB,
    labelled = F,
    records = records
  )
  all_raw_structure_cols <- c(DB$redcap$id_col,"redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
  DB$redcap$raw_structure_cols <- all_raw_structure_cols[which(all_raw_structure_cols%in%colnames(raw))]
  DB <- raw_process_redcap(raw = raw,DB = DB)
  if(is.null(records)){
    DB$all_records <- all_records(DB)
  }
  DB$internals$data_extract_labelled <- F
  if(labelled){
    DB <- DB %>% raw_to_labelled_DB()
  }
  DB
}

get_redcap_users <- function(DB){
  userRole  <- get_redcap_info(DB,"userRole") %>% dplyr::select("unique_role_name","role_label")
  userRoleMapping <-  get_redcap_info(DB,"userRoleMapping")
  user <-  get_redcap_info(DB,"user")
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
    x <- (Sys.time()-lubridate::days(last)) %>% as.character()
  }
  if(units=="hours"){
    x <- (Sys.time()-lubridate::hours(last)) %>% as.character()
  }
  if(units=="mins"){
    x <- (Sys.time()- lubridate::minutes(last)) %>% as.character()
  }
  if(begin_time!=""){
    x <- begin_time
  }
  get_redcap_info(DB,"log",additional_args = list(beginTime=x)) %>% clean_redcap_log()
}

#' @title Check the REDCap log
#' @inheritParams save_DB
#' @param labelled T/F for clean vs raw labels
#' @param records optional records
#' @return data.frame of raw_redcap
#' @export
get_raw_redcap <- function(DB,labelled=T,records=NULL){
  if(missing(records)) records <- NULL
  raw <- REDCapR::redcap_read(redcap_uri=DB$links$redcap_uri, token=validate_redcap_token(DB),batch_size = 2000, interbatch_delay = 0.1,records = records, raw_or_label = ifelse(labelled,"label","raw"))$data %>% all_character_cols()
  return(raw)
}

# file repo -----------------

#' @title Uploads a file to REDCap
#' @inheritParams save_DB
#' @param file file location on your PC
#' @return messages for confirmation
#' @export
upload_file_to_redcap_fileRepository <- function(DB,file){
  DB <- validate_DB(DB)
  file <- normalizePath(file)
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
  file <- normalizePath(file)
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

# process ---------

raw_process_redcap <- function(raw,DB){
  if(nrow(raw)>0){
    if(is.null(DB$internals$data_extract_merged)){
      DB$internals$data_extract_merged <- F
    }
    was_merged <- F
    if(DB$internals$data_extract_merged){
      was_merged <- T
      DB <- unmerge_non_repeating_DB(DB)
    }
    raw  <- raw %>% all_character_cols()
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")

    if(DB$redcap$is_longitudinal){
      raw$id_temp <- 1:nrow(raw)
      raw <-  merge(raw,DB$redcap$events[,c("arm_num","event_name","unique_event_name")],by.x="redcap_event_name",by.y="unique_event_name",sort = F)
      add_ons  <- add_ons[which(add_ons%in%colnames(raw))]

      cols <- c(add_ons, colnames(raw)) %>% unique()
      raw <- raw[order(raw$id_temp),cols%>% sapply(function(c){which(colnames(raw)==c)}) %>% as.integer()]
      raw$id_temp <- NULL
    }
    add_ons  <- add_ons[which(add_ons%in%colnames(raw))]
    if(any(!DB$redcap$raw_structure_cols %in% colnames(raw)))stop("raw is missing one of the following... and that's weird: ", DB$redcap$raw_structure_cols %>% paste0(collapse = ", "))


    for(instrument_name in DB$redcap$instruments$instrument_name){
      add_ons_x <- add_ons
      #instrument_name <-  DB$redcap$instruments$instrument_name %>% sample(1)
      is_repeating_instrument <- instrument_name%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
      rows  <- 1:nrow(raw)
      if(!DB$redcap$is_longitudinal){
        if("redcap_repeat_instrument"%in%colnames(raw)){
          if(is_repeating_instrument){
            rows <- which(raw$redcap_repeat_instrument==instrument_name)
          }
          if(!is_repeating_instrument){
            rows <- which(is.na(raw$redcap_repeat_instrument))
          }
        }
      }
      if(DB$redcap$is_longitudinal){
        events_ins <- DB$redcap$event_mapping$unique_event_name[which(DB$redcap$event_mapping$form==instrument_name)] %>% unique()
        rows <- which(raw$redcap_event_name%in%events_ins)
      }
      if(!is_repeating_instrument){
        add_ons_x <- add_ons_x[which(!add_ons_x%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
      }
      cols <- unique(c(add_ons_x,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(raw))]))
      DB$data_extract[[instrument_name]] <- raw[rows,cols]
    }
    if(was_merged){
      DB <- merge_non_repeating_DB()
    }
  }
  DB
}

#' @title Select REDCap records from DB
#' @inheritParams save_DB
#' @param records character vector of the IDs you want to filter the DB by
#' @return DB object that has been filtered to only include the specified records
#' @export
select_redcap_records <- function(DB, records=NULL){
  DB_selected <- DB
  if(!is.null(records)){
    if (length(records)==0)stop("Must supply records")
    DB_selected$data_extract <- list()
    BAD  <- records[which(!records%in%DB$all_records[[DB$redcap$id_col]])]
    GOOD  <- records[which(records%in%DB$all_records[[DB$redcap$id_col]])]
    if(length(BAD)>0)stop("Following records are not found in DB: ", BAD %>% paste0(collapse = ", "))
    for(FORM in names(DB$data_extract)){
      DB_selected[["data_extract"]][[FORM]]  <- DB$data_extract[[FORM]][which(DB$data_extract[[FORM]][[DB$redcap$id_col]]%in%GOOD),]
    }
    for(FORM in names(DB$data_transform)){
      DB_selected[["data_transform"]][[FORM]]  <- DB$data_transform[[FORM]][which(DB$data_transform[[FORM]][[DB$redcap$id_col]]%in%GOOD),]
    }
  }
  DB_selected
}

filter_metadata_from_form <- function(FORM,DB){
  # if(!deparse(substitute(FORM))%in%DB$redcap$instruments$instrument_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  if(any(!colnames(FORM)%in%c(DB$redcap$metadata$field_name,DB$redcap$raw_structure_cols,"arm_num","event_name")))stop("All column names in your form must match items in your metadata, `DB$redcap$metadata$field_name`")
  instruments <- DB$redcap$metadata$form_name[
    which(
      DB$redcap$metadata$field_name%in%colnames(FORM)&
        !DB$redcap$metadata$field_name%in%c(DB$redcap$id_col,"redcap_repeat_instance","redcap_repeat_instrument")
    )
  ] %>% unique()
  if(any(instruments%in%DB$redcap$instruments$repeating))stop("All column names in your form must match only one form in your metadata, `DB$redcap$instruments$instrument_name`, unless they are all non-repeating")
  metadata <- DB$redcap$metadata[which(DB$redcap$metadata$form_name%in%instruments),]
  metadata <- metadata[which(metadata$field_type!="descriptive"),]
  metadata <- metadata[which(metadata$field_name%in%colnames(FORM)),]
  metadata$has_choices <- !is.na(metadata$select_choices_or_calculations)
  return(metadata)
}


#' @title Clean to Raw REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of labelled REDCap to be converted to raw REDCap (for uploads)
#' @return DB object that has been filtered to only include the specified records
#' @export
labelled_to_raw_form <- function(FORM,DB){
  use_missing_codes <- is.data.frame(DB$redcap$missing_codes)
  metadata <- filter_metadata_from_form(FORM = FORM,DB = DB)
  for(i in 1:nrow(metadata)){ # i <-  1:nrow(metadata) %>% sample(1)
    COL_NAME <- metadata$field_name[i]
    has_choices <- metadata$has_choices[i]
    if(has_choices){
      z <- metadata$select_choices_or_calculations[i] %>% split_choices()
      FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
        OUT <- NA
        if(!is.na(C)){
          coded_redcap <- which(z$name==C)
          if(length(coded_redcap)>0){
            OUT <- z$code[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2 <- which(DB$redcap$missing_codes$name==C)
              if(length(coded_redcap2)>0){
                OUT <- DB$redcap$missing_codes$code[coded_redcap2]
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }else{
      if(use_missing_codes){
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- C
          if(!is.na(C)){
            D <- which(DB$redcap$missing_codes$name==C)
            if(length(D)>0){
              OUT <- DB$redcap$missing_codes$code[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  FORM
}


#' @title Raw to Labelled REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of raw REDCap to be converted to labelled REDCap
#' @return DB object
#' @export
raw_to_labelled_form <- function(FORM,DB){
  if(nrow(FORM)>0){
    use_missing_codes <- is.data.frame(DB$redcap$missing_codes)
    metadata <- filter_metadata_from_form(FORM = FORM,DB = DB)
    for(i in 1:nrow(metadata)){ # i <-  1:nrow(metadata) %>% sample(1)
      COL_NAME <- metadata$field_name[i]
      has_choices <- metadata$has_choices[i]
      if(has_choices){
        z <- metadata$select_choices_or_calculations[i] %>% split_choices()
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- NA
          if(!is.na(C)){
            coded_redcap <- which(z$code==C)
            if(length(coded_redcap)>0){
              OUT <- z$name[coded_redcap]
            }else{
              if(use_missing_codes){
                coded_redcap2 <- which(DB$redcap$missing_codes$code==C)
                if(length(coded_redcap2)>0){
                  OUT <- DB$redcap$missing_codes$name[coded_redcap2]
                }else{
                  warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
                }
              }else{
                warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }else{
        if(use_missing_codes){
          z <- DB$redcap$missing_codes
          FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
            OUT <- C
            if(!is.na(C)){
              D <- which(z$code==C)
              if(length(D)>0){
                OUT <- z$name[D]
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }
      }
    }
  }
  FORM
}

labelled_to_raw_DB <- function(DB){
  DB <- validate_DB(DB)
  if(!DB$internals$data_extract_labelled)stop("DB is already raw/coded (not labelled values)")
  for(TABLE in names(DB$data_extract)){
    DB$data_extract[[TABLE]] <- labelled_to_raw_form(FORM = DB$data_extract[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- F
  DB
}

raw_to_labelled_DB <- function(DB){
  DB <- validate_DB(DB)
  if(DB$internals$data_extract_labelled)stop("DB is already labelled (not raw coded values)")
  for(TABLE in names(DB$data_extract)){
    DB$data_extract[[TABLE]] <- raw_to_labelled_form(FORM = DB$data_extract[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- T
  DB
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
    is_na  <- is.na(DB$redcap$project_info$missing_data_codes)
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
merge_non_repeating_DB <- function(DB){ # need to adjust for events, currently destructive
  if(DB$internals$data_extract_merged)stop("Already merged!")
  data_choice <- "data_extract"
  all_instrument_names <- DB$redcap$instruments$instrument_name
  keep_instruments <- NULL
  instrument_names <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]
  if(DB$redcap$is_longitudinal){
    instrument_names <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating&!DB$redcap$instruments$repeating_via_events)]
    keep_instruments <- all_instrument_names[which(!all_instrument_names%in% instrument_names)]
    data_choice <- "data_transform"
  }

  DB[[data_choice]][[DB$internals$merge_form_name]] <- merge_from_extact(DB,instrument_names)
  if(data_choice=="data_extract") {
    for(instrument_name in instrument_names){
      DB[["data_extract"]][[instrument_name]] <- NULL
    }
    DB$internals$data_extract_merged <- T

  }else{
    for(keep in keep_instruments){
      DB[["data_transform"]][[keep]] <- DB[["data_extract"]][[keep]]
    }
  }
  DB
}
merge_from_extact <- function(DB,instrument_names){
  instrument_names <- instrument_names %>% as.list()
  if (length(instrument_names)==1) warning('No need to merge you only have one form that is non-repeating')
  merged <- DB[["data_extract"]][[instrument_names[[1]]]]
  merged$redcap_event_name <- NULL
  # merged$arm_num <- NULL
  merged$event_name <- NULL
  merged$redcap_repeat_instrument <- NULL
  merged$redcap_repeat_instance <- NULL
  instrument_names[[1]] <- NULL
  while (length(instrument_names)>0) {
    dfx <- DB[["data_extract"]][[instrument_names[[1]]]]
    dfx$redcap_event_name <- NULL
    # dfx$arm_num <- NULL
    dfx$event_name <- NULL
    dfx$redcap_repeat_instrument <- NULL
    dfx$redcap_repeat_instance <- NULL
    (in_common <- colnames(merged)[which(colnames(merged)%in%colnames(dfx))])
    merged <- merge(merged,dfx,by=in_common,all = T)
    instrument_names[[1]] <- NULL
  }
  merged
}

#' @title Unmerge non-repeating, not ready for multi-event projects
#' @inheritParams save_DB
#' @return DB object that has merged all non repeating forms
#' @export
unmerge_non_repeating_DB <- function(DB){
  if(!DB$internals$data_extract_merged)stop("No DB$data_extract named as 'merged'!")
  instrument_names <- DB$data_extract[[DB$internals$merge_form_name]] %>% colnames() %>% sapply(function(COL){DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name==COL)]}) %>% unique() %>% as.list()
  merged <- DB$data_extract[[DB$internals$merge_form_name]]
  while (length(instrument_names)>0) {
    instrument_name  <- instrument_names[[1]]
    DB$data_extract[[instrument_name]] <- merged[,unique(c(DB$redcap$id_col,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(merged))]))]
    instrument_names[[1]] <- NULL
  }
  DB$data_extract[[DB$internals$merge_form_name]] <- NULL
  DB$internals$data_extract_merged <- F
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
    bad_identifiers <- identifiers[which(!identifiers%in%DB$redcap$metadata$field_name)]
    if(length(bad_identifiers)>0)stop("You have an identifier that is not included in the set of `DB$redcap$metadata$field_name` --> ",bad_identifiers %>% paste0(collapse = ", "))
    if(DB$redcap$id_col%in%identifiers)stop("Your REDCap ID, ",DB$redcap$id_col,", should not be deidentified.") #If you want to pass a new set of random IDs to make this data use `scramble_ID_DB(DB)`.")
  }
  if(missing_identifiers){
    identifiers <-  DB$redcap$metadata$field_name[which(DB$redcap$metadata$identifier=="y")]
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
  metadata  <- unique(metadata$form_name) %>%
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
      DB$data_extract[[FORM]][[COLUMN]] <- DB$data_extract[[FORM]][[COLUMN]] %>% clean_column_for_table(
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
clean_column_for_table <- function(col,class,label,units,levels){
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
add_ID_to_DF <- function(DF,DB,ref_id){
  if(!ref_id%in%DB$redcap$metadata$field_name)stop("The ref_id not valid. Must be a REDCap raw colname")
  form <- DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name==ref_id)]
  if(DB$internals$data_extract_merged){
    if(form %in% DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]){
      form <- DB$internals$merge_form_name
    }
  }
  id_col <- DF[[ref_id]] %>% sapply(function(ID){
    DB$data_extract[[form]][[DB$redcap$id_col]][which(DB$data_extract[[form]][[ref_id]]==ID)]
  }) %>% as.data.frame()
  colnames(id_col) <- DB$redcap$id_col
  DF <- cbind(id_col,DF)
  DF
}

#' @title grab data table for an individual(s)
#' @description
#' grab data table for an individual(s)
#' @inheritParams select_redcap_records
#' @return list of data tables
#' @export
grab_record_tables <- function(DB, records){
  OUT  <- list()
  for(TABLE in names(DB$data_extract)){
    OUT[[TABLE]] <-   DB$data_extract[[TABLE]][which(DB$data_extract[[TABLE]][[DB$redcap$id_col]]%in%records),]
  }
  OUT
}

split_choices <- function(x){
  oops <- x
  x <- gsub("\n", " | ",x)  #added this to account for redcap metadata output if not a number
  x <- x %>% strsplit(" [:|:] ") %>% unlist()
  check_length <- length(x)
  # code <- x %>% stringr::str_extract("^[^,]+(?=, )")
  # name <- x %>% stringr::str_extract("(?<=, ).*$")
  result <- x %>% stringr::str_match("([^,]+), (.*)")
  # x <- data.frame(
  #   code=x %>% strsplit(", ") %>% sapply(`[`, 1),
  #   name=x %>% strsplit(", ")%>% sapply(`[`, -1) %>% sapply(function(y){paste0(y,collapse = ", ")})
  # )
  x <- data.frame(
    code=result[,2],
    name=result[,3]
  )
  rownames(x) <- NULL
  if(nrow(x)!=check_length)stop("split choice error: ",oops)
  x
}

make_codebook <- function(DB){
  choices <- which(DB$redcap$metadata$field_type%in%c("radio","dropdown","checkbox_choice","yesno"))
  if(length(choices)>0){
    for(field in DB$redcap$metadata$field_name[choices]){
      DB[["choices"]][[field]] <- DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field)] %>% split_choices()
    }
  }
  OUT <- NULL
  for (CHOICE in names(DB[["choices"]])){
    x <- DB[["choices"]][[CHOICE]]
    x$field_name <- CHOICE
    OUT <- OUT %>% dplyr::bind_rows(x)
  }
  OUT <- OUT %>% dplyr::select("field_name","code","name")
  rownames(OUT) <- NULL
  OUT
}

# to and from dir -----------

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

# upload -----

#' @title Upload to REDCap
#' @description
#' This will only overwrite and new data. It will not directly delete and data.
#' Because this is the only function that can mess up your data, use it at your own risk.
#' Remember all changes are saved in the redcap log if there's an issue. Missing rows and columns are fine!
#' @param to_be_uploaded data.frame in raw coded form. If you worked with clean data pass your data to `labelled_to_raw_form(FORM,DB)` first.
#' @inheritParams save_DB
#' @param batch_size numeric of how big the REDCap batch upload is. Default 500.
#' @return messages
#' @export
upload_form_to_redcap <- function(to_be_uploaded,DB,batch_size=500){
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
upload_DB_to_redcap <- function(DB,batch_size=500,ask=T){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB <- validate_DB(DB)
  if(ask){
    if(count_DB_cells(DB)>5000){
      stop  <- utils::menu(choices = c("YES - I want to stop and double check what I'm about to upload","NO - Move forward with larger upload"),title = "This is a large upload. Do you want to stop and double check it first?")
      if(stop==1)stop("Double check DB object prior to upload")
    }
  }
  warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(is.null(DB$data_extract))stop("`DB$data_extract` is empty")
  for(TABLE in names(DB$data_extract)){
    to_be_uploaded_raw <- DB$data_extract[[TABLE]]
    if(nrow(to_be_uploaded_raw)>0){
      if(DB$internals$data_extract_labelled){
        to_be_uploaded_clean <- to_be_uploaded_raw
        to_be_uploaded_raw <- to_be_uploaded_clean %>% labelled_to_raw_form(DB)
      }
      do_it <- 1
      if(ask){
        if(DB$internals$data_extract_labelled){
          print("Clean Data")
          print.data.frame(to_be_uploaded_clean%>% utils::head(n=40))
        }
        print("Raw Data")
        print.data.frame(to_be_uploaded_raw %>% utils::head(n=40))
        do_it  <- utils::menu(choices = c("Yes upload","No and go to next"),title = "Do you want to upload this?")
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
#' @inheritParams upload_form_to_redcap
#' @param DB2 The comparison DB object
#' @param ignore_instruments character vector of instruments to be ignored if you know they are unchanged.
#' @return DB_import but only the differences
#' @export
find_DB_diff <- function(DB2,DB,ignore_instruments){
  warning("This function is not ready for primetime yet! Use at your own risk!",immediate. = T)
  DB <- validate_DB(DB)
  DB2 <- validate_DB(DB2)
  DB <- select_redcap_records(DB, records = DB2$all_records[[DB$redcap$id_col]])
  if(!missing(ignore_instruments)){
    if(any(!ignore_instruments%in%names(DB2[["data_extract"]])))stop("ignore_instruments must be included in the set of instrument names, `names(DB2$data_extract)`")
    for(DROP in ignore_instruments){
      DB2$data_extract[[DROP]] <- NULL
    }
  }
  # if(DB2$internals$data_extract_labelled&!DB$internals$data_extract_labelled){
  #   DB2 <- labelled_to_raw_DB(DB2)
  # }
  # warning("Right now this function only updates repeating instruments. It WILL NOT clear repeating instrument instances past number 1. SO, you will have to delete manually on REDCap.",immediate. = T)
  if(any(!names(DB2[["data_extract"]])%in%names(DB$data_extract)))stop("All file names and data.table names from your directory a.k.a. `names(DB2$data_extract)` must match the DB instrument names, `DB$redcap$instruments$instrument_name`")
  if(is.null(DB2[["data_extract"]]))stop("`DB2$data_extract` is empty")
  for(TABLE in names(DB2[["data_extract"]])){#TABLE <- names(DB2[["data_extract"]]) %>% sample(1)
    new <-  DB2[["data_extract"]][[TABLE]]
    old <-  DB$data_extract[[TABLE]]
    ref_cols <- DB$redcap$raw_structure_cols
    ref_cols <- ref_cols[which(ref_cols%in%c(colnames(old),colnames(new)))]
    DB2[["data_extract"]][[TABLE]] <- find_df_diff(new= new , old =  old, ref_cols = ref_cols)
  }
  DB2
}

# link --------

#' @title Link to get a new API token for your project (if you access)
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
link_API_token <-  function(DB){
  DB$links$redcap_API %>% utils::browseURL()
}

#' @title Link view the API playground for your project (if you access)
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
link_API_playground <- function(DB){
  DB$links$redcap_API_playground %>% utils::browseURL()
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
  DB$links$redcap_home %>% utils::browseURL()
}

#' @title Shows DB in the env
#' @inheritParams save_DB
#' @param record REDCap record id or study id etc, any column names that match `DB$redcap$id_col`
#' @param page REDCap page for the record. Must be one of `DB$redcap$instruments$instrument_name`
#' @param instance REDCap instance if it's a repeating instrument
#' @return opens browser link
#' @export
link_REDCap_record <- function(DB,record,page,instance){
  link <- paste0(DB$links$redcap_base_link,"redcap_v",DB$redcap$version,"/DataEntry/record_home.php?pid=",DB$redcap$project_id)
  if(!missing(record)){
    if(!record%in%DB$all_records[[DB$redcap$id_col]])stop(record," is not one of the records inside DB")
    if("arm_num"%in%colnames(DB$all_records)){
      link <- link %>% paste0("&arm=", DB$all_records$arm_num[which(DB$all_records$participant_id==record)])
    }
    link <- link %>% paste0("&id=",record)
  }
  if(!missing(page)){
    link <- gsub("record_home","index",link)
    if(!page%in%DB$redcap$instruments$instrument_name)stop(page," has to be one of the instrument names: ",paste0(DB$redcap$instruments$instrument_name,collapse = ", "))
    link <- link %>% paste0("&page=",page)
    if(!missing(instance)){
      if(!page%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)])stop("If you provide an instance, it has to be one of the repeating instrument names: ",paste0(DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)],collapse = ", "))
      link <- link %>% paste0("&instance=",instance)
    }
  }
  link %>% utils::browseURL()
}

# upload --------

#' @title Shows DB in the env
#' @param DB DB from load_DB or setup_DB
#' @param force logical for force a fresh update
#' @param day_of_log numbers of days to be checked in the log
#' @param labelled logical for whether or not to return raw or labelled REDCap. Default is TRUE.
#' @param drop_dir logical for whether or not save files to directory.
#' @param get_files logical for whether or not to get files from redcap.
#' @param original_file_names logical for whether or not to use original file names.
#' @return messages for confirmation
#' @export
update_DB <- function(
    DB,
    force = F,
    day_of_log = 10,
    labelled = T,
    get_files = F,
    original_file_names = F,
    drop_dir = T
) {
  IDs <- NULL
  will_update <- T
  was_updated <- F
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
  DB$internals$last_metadata_check <- Sys.time()
  DB$internals$last_data_check <- Sys.time()
  # DB$internals$last_metadata_update <- Sys.time()-lubridate::days(1)
  # DB$internals$last_data_update <- Sys.time()-lubridate::days(1)
  if(!force){ # check log interim
    if(is.null(DB$internals$last_metadata_update)||is.null(DB$internals$last_data_update)){
      force <- T
    }else{
      time <- c(DB$internals$last_metadata_update,DB$internals$last_data_update)
      time <- time %>% min() %>% as.character()
      ilog <- check_redcap_log(DB,begin_time = time)

      ilog3 <- ilog[which(is.na(ilog$record_id)),]
      ilog3$timestamp <- NULL
      ilog3 <- ilog3 %>% unique()
      ilog3 <- ilog3[grep("export|download |edit report|Switch DAG|Copy report|Multi-Language|File Repository |custom record dashboard|User regenerate own API token|Create report",ilog3$details,ignore.case = T,invert = T) %>% unique(),]
      if(nrow(ilog3)>0){
        force <- T
        message(paste0("Update because: " ,ilog3$action, " - ", ilog3$details))
      }else{
        ilog2 <- ilog[which(!is.na(ilog$record_id)),]
        ilog2$timestamp <- NULL
        ilog2 <- ilog2 %>% unique()
        if(any(ilog2$action_type%in%c("Create","Delete"))){
          force <- T
        }else{
          IDs <- ilog2$record_id %>% unique()
          if(length(IDs)==0){
            IDs <- NULL
            will_update <- F
          }
        }
        # if(Sys.time()>=(DB$internals$last_metadata_update+lubridate::days(2))){
        #   force <- T
        # }#not needed anymore because of log check?
      }
    }
  }
  if(will_update||force){
    if(is.null( DB$internals$data_extract_merged)){
      DB$internals$data_extract_merged <- F
    }
    was_merged <- DB$internals$data_extract_merged
    if(was_merged){
      warning(" I would prefer if you run updates on unmerged data sets. Consider saving the merge step as a final save.", immediate. = T)
      DB <- unmerge_non_repeating_DB(DB)
    }
  }
  if(force){
    DB <- DB %>% get_redcap_metadata()
    DB <- DB %>% get_redcap_data(labelled = labelled)
    forms <- DB$redcap$instruments$instrument_name
    DB$redcap$log <- DB %>% check_redcap_log(last = day_of_log,units = "days") %>% unique()
    message("Full update!")
    was_updated <- T
  }else{
    if(will_update){
      time <- c(DB$internals$last_metadata_update,DB$internals$last_data_update)
      time <- time %>% min() %>% magrittr::subtract(lubridate::minutes(3)) %>% as.character()
      DB2 <- DB %>% get_redcap_data(labelled = labelled,records = IDs)
      DB2$all_records <-  all_records(DB2)
      DB2 <-DB2 %>% find_DB_diff(DB)
      DB$internals$last_data_update <- DB2$internals$last_data_update
      DB$redcap$log <- DB$redcap$log %>% dplyr::bind_rows(check_redcap_log(DB,begin_time = time)) %>% unique() %>% all_character_cols()
      forms <- names(DB2$data_extract)[
        which(
          names(DB2$data_extract) %>%
            sapply(function(form){
              nrow(DB2$data_extract[[form]])>0
            })
        )]
      for(TABLE in forms){
        DB$data_extract[[TABLE]] <- DB$data_extract[[TABLE]][which(!DB$data_extract[[TABLE]][[DB$redcap$id_col]]%in%IDs),] %>% dplyr::bind_rows(DB2$data_extract[[TABLE]][which(DB2$data_extract[[TABLE]][[DB$redcap$id_col]]%in%IDs),])
      }
      message("updated: ",paste0(IDs,collapse = ", "))
      was_updated <- T
    }else{
      message("Up to date already!")
    }
  }
  if(get_files){
    get_redcap_files(DB,original_file_names=original_file_names)
  }
  if(was_updated){
    if(was_merged){
      DB <- merge_non_repeating_DB(DB)
    }
    if(drop_dir){
      DB <- drop_redcap_dir(DB,forms = forms)
    }
  }
  if(!is.null(DB$dir_path))  save_DB(DB)
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

# transform --------

remap_process <- function(DB){
  DB <- validate_DB(DB)
  if(is.data.frame(DB$redcap$metadata)){
    metadata_remap <-   DB$remap$metadata_remap
    instruments_remap <- DB$redcap$instruments
    instruments_remap$instrument_name_remap <- instruments_remap$instrument_name %>% sapply(function(instrument_name){
      x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
      if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
      return(x)
    })
    if(DB$redcap$is_longitudinal){
      if(DB$redcap$has_arms_that_matter){
        #can add remapping of arms but not smart if they matter
      }else{
        # DB$redcap$events->x
        event_mapping_remap <- DB$remap$event_mapping_remap
        events_remap <- DB$redcap$events
        events_remap$unique_event_name_remap <- events_remap$unique_event_name %>% sapply(function(unique_event_name){
          x<-event_mapping_remap$unique_event_name_remap[which(event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()
          if(length(x)>1)stop("unique_event_names cannot match multiple unique_event_name_remaps: ",unique_event_name)
          return(x)
        })
        events_new <- events_remap[,c("unique_event_name_remap","event_name")] %>% unique()
        colnames(events_new)[1] <- "unique_event_name"
        event_mapping_new <- event_mapping_remap[,c("unique_event_name_remap","event_name","form")] %>% unique()
        event_mapping_new$form <- event_mapping_new$form %>% sapply(function(instrument_name){
          x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
          if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
          return(x)
        })
        event_mapping_new <- event_mapping_new %>% unique()
        colnames(event_mapping_new)[1] <- "unique_event_name"
        events_new$former_unique_event_names <- events_new$unique_event_name %>% sapply(function(unique_event_name){
          events_remap$unique_event_name[which(events_remap$unique_event_name_remap==unique_event_name)] %>% unique() %>% paste0(collapse = " | ")
        })
        event_mapping_new$former_unique_event_names <- event_mapping_new$unique_event_name %>% sapply(function(unique_event_name){
          event_mapping_remap$unique_event_name[which(event_mapping_remap$unique_event_name_remap==unique_event_name)] %>% unique() %>% paste0(collapse = " | ")
        })
        x<- event_mapping_new[which(event_mapping_new$form%in%instruments_remap$instrument_name_remap),]
        instruments_remap$repeating_via_events[
          which(
            instruments_remap$instrument_name_remap %>% sapply(function(instrument_name_remap){
              # instrument_name <- DB$redcap$instruments$instrument_name %>% sample(1)
              if(DB$internals$merge_form_name==instrument_name_remap){
                F
              }else{
                anyDuplicated(event_mapping_new$form[which(event_mapping_new$form==instrument_name_remap)])>0
              }
            })
          )
        ] <- T
      }
      non_reps <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]
      ins_new_cols <- c("instrument_name_remap","repeating")
      if(DB$redcap$is_longitudinal){
        non_reps <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating&!DB$redcap$instruments$repeating_via_events)]
        ins_new_cols <- c("instrument_name_remap","repeating","repeating_via_events")
      }
      if(length(non_reps)>0){
        metadata_remap$form_name_remap[which( metadata_remap$form_name%in%non_reps)] <- DB$internals$merge_form_name
      }
      instruments_new <- instruments_remap[,ins_new_cols] %>% unique()
      instruments_new$former_instrument_names <- instruments_new$instrument_name_remap %>% sapply(function(instrument_name_remap){
        instruments_remap$instrument_name[which(instruments_remap$instrument_name_remap==instrument_name_remap)] %>% unique() %>% paste0(collapse = " | ")
      })
      colnames(instruments_new)[1] <- "instrument_name"
      DB$remap$events_new <- events_new
      DB$remap$event_mapping_new <- event_mapping_new
    }
    metadata_new <- metadata_remap
    metadata_new$field_name <- metadata_new$field_name_remap
    metadata_new$field_name_remap <- NULL
    metadata_new$form_name <- metadata_new$form_name_remap
    metadata_new$form_name_remap <- NULL
    metadata_new <- metadata_new[,colnames(metadata_new)%in%c(
      "field_name",
      "form_name",
      "field_type",
      "select_choices_or_calculations"
    )] %>% unique()
    if(metadata_new$field_name %>% anyDuplicated() %>% magrittr::is_greater_than(0))stop("metadata_new has duplicate field names")

    DB$remap$metadata_new <- metadata_new
    DB$remap$instruments_new <- instruments_new
    DB$remap$instruments_remap <- instruments_remap
    # if(save_file) metadata_new %>% rio::export(file = DB$dir_path %>% file.path("input","metadata_new_default.xlsx"))
  }
  return( DB)
}

generate_default_remap <- function(DB,save_file=!is.null(DB$dir_path)){
  DB <- validate_DB(DB)
  if(is.data.frame(DB$redcap$metadata)){
    metadata_remap <-   DB$redcap$metadata
    metadata_remap$field_name_remap <- metadata_remap$field_name
    metadata_remap$form_name_remap <- metadata_remap$form_name

    if(DB$redcap$is_longitudinal){
      if(DB$redcap$has_arms_that_matter){
        #can add remapping of arms but not smart if they matter
      }else{
        # DB$redcap$events->x
        event_mapping_remap <- DB$redcap$event_mapping
        event_mapping_remap <- merge(event_mapping_remap, DB$redcap$events[,c("event_name","unique_event_name")], by="unique_event_name")
        event_mapping_remap$unique_event_name_remap <- tolower(gsub(" ","_",event_mapping_remap$event_name))
        event_mapping_remap$form_remap <-  event_mapping_remap$form %>% sapply(function(instrument_name){
          x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
          if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
          return(x)
        })
      }
      if(save_file) event_mapping_remap %>% rio::export(file = DB$dir_path %>% file.path("input","event_mapping_remap_default.xlsx"))
      DB$remap$event_mapping_remap <- event_mapping_remap
    }
    if(save_file) metadata_remap %>% rio::export(file = DB$dir_path %>% file.path("input","metadata_remap_default.xlsx"))
    DB$remap$metadata_remap <- metadata_remap
  }
  remap_process(DB)
}

#' @title Generate custom remap files from input
#' @inheritParams save_DB
#' @return DB object that has DB$remap populated from input folder
#' @export
generate_custom_remap_from_dir <- function(DB){
  DB <- validate_DB(DB)
  input_folder <- DB$dir_path %>% file.path("input")
  input_folder %>% file.path(c("metadata_remap.xlsx","event_mapping_remap.xlsx"))
  input_folder %>% list.files(full.names = T)
  for(file in c("metadata_remap","event_mapping_remap")){
    path <- input_folder %>% file.path(paste0(file,".xlsx"))
    if(file.exists(path)){
      DB$remap[[file]] <- rio::import(file = path)
    }
  }
  DB <- remap_process(DB)
  return(DB)
}

#' @title Transform DB
#' @inheritParams save_DB
#' @return DB object that has DB$data_transform, can be based on a remap file from input folder or default
#' @export
transform_DB <- function(DB){
  DB  <- validate_DB(DB)
  transform <- list()
  if(DB$remap %>% is_something()){
    instrument_names <- DB$remap$instruments_new$instrument_name
    instrument_name <- instrument_names %>%  sample (1)
    for (instrument_name in instrument_names) {
      if(instrument_name == DB$internals$merge_form_name){
        old_instruments <- DB$remap$instruments_remap$instrument_name[which(DB$remap$instruments_remap$instrument_name_remap == instrument_name)]
        DB$data_transform[[instrument_name]] <- merge_from_extact(DB, old_instruments)
      }else{
        old_instruments <- DB$remap$instruments_remap$instrument_name[which(DB$remap$instruments_remap$instrument_name_remap == instrument_name)]
        old_instrument <- old_instruments %>%  sample (1)
        final_out <- NULL
        for(old_instrument in old_instruments){
          keep <- DB$data_extract[[old_instrument]]
          colnames(keep) <- colnames(keep) %>% sapply(function(col){
            out <- col
            x<-DB$remap$metadata_remap$field_name_remap[which(DB$remap$metadata_remap$field_name == col)]
            if(length(x)>0)out <- x
            out
          })
          if("redcap_event_name"%in%colnames(keep)){
            keep$redcap_event_name <- keep$redcap_event_name %>% sapply(function(unique_event_name){DB$remap$event_mapping_remap$unique_event_name_remap[which(DB$remap$event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()})
            # keep$event_name <- keep$event_name %>% sapply(function(event_name){DB$remap$event_mapping_remap$[which(DB$remap$event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()})
          }
          final_out <- final_out %>% dplyr::bind_rows(keep)
        }
        DB$data_transform[[instrument_name]] <- final_out
      }
    }
  }
  return(DB)
}

summarize_DB <- function(DB){
  #project --------
  summary <- list()
  was_remapped <-DB$remap %>% is_something()
  pull_from <- "redcap"
  df_names1 <- c("metadata","instruments","event_mapping","events")
  if(was_remapped){
    pull_from <- "remap"
    df_names2 <- paste0(df_names1,"_new")
  }

  for(i in 1:length(df_names1)){
    summary[[df_names1[[i]]]] <- DB[[pull_from]][[df_names2[[i]]]]
  }
  DB$redcap$project_info

  metadata <- DB$redcap$metadata
  instruments <- DB$redcap$instruments
  arms <- DB$redcap$arms
  users <- DB$redcap$instruments
  events <- DB$redcap$events
  event_mapping <- DB$redcap$event_mapping

  list(
    short_name=NULL,
    token_name=NULL,
    dir_path=NULL,
    internals = list(
      last_metadata_check=NULL,
      last_metadata_update=NULL,
      last_metadata_dir_save=NULL,
      last_data_check=NULL,
      last_data_update=NULL,
      last_data_dir_save = NULL,
      last_data_transformation = NULL,
      last_directory_save=NULL,
      data_extract_labelled = NULL,
      data_extract_merged = NULL,
      merge_form_name = "merged",
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
      has_arms_that_matter = NULL,
      has_repeating_instruments_or_events = NULL,
      has_repeating_instruments = NULL,
      has_repeating_events = NULL
    ),
    auto_jobs = NULL,
    remap = list(
      metadata_remap=NULL,
      metadata_new=NULL,
      instruments_remap=NULL,
      instruments_new=NULL,
      arms_map=NULL,
      arms_new=NULL,
      events_remap=NULL,
      events_new=NULL,
      event_mapping_remap=NULL,
      event_mapping_new=NULL
    ),
    data_extract = NULL,
    data_transform = NULL,
    data_upload = NULL,
    all_records = NULL,
    links = list(
      redcap_base = NULL,
      redcap_uri = NULL,
      redcap_home = NULL,
      redcap_records = NULL,
      redcap_API = NULL,
      redcap_API_playground = NULL,
      github = "https://github.com/brandonerose/rosyredcap",
      thecodingdocs = "https://www.thecodingdocs.com/"
    )
  )


  if(was_remapped){

  }

  #records belong to arms 1 to 1 ----------
  summary$records_n <- 0
  if(!is.null(DB$all_records)){
    summary$records_n <- DB$all_records %>% length()
  }
  #arms----------------
  summary$arms_n <- 0
  if(is.data.frame(DB$redcap$arms)){
    summary$arms_n <- DB$redcap$arms %>% nrow()
    id_pairs <- DB$redcap$instruments$instrument_name %>%  lapply(function(IN){DB$data_extract[[IN]][,c(DB$redcap$id_col,"arm_num")]}) %>% dplyr::bind_rows() %>% unique()
    DB$redcap$arms$arm_records_n <- DB$redcap$arms$arm_num %>% sapply(function(arm){
      which(id_pairs$arm_num==arm)%>% length()
    })
  }
  #events belong to arms many to 1 ----------------
  # summary$events_n <- DB$redcap$events %>% nrow()
  summary$events_n <- 0
  if(is.data.frame(DB$redcap$events)){
    summary$events_n <- DB$redcap$events %>% nrow()
    summary$event_names_n <- DB$redcap$events$event_name %>% unique() %>% length()
    # 1:nrow(DB$redcap$event_mapping) %>% lapply(function(i){
    #   (DB$data_extract[[DB$redcap$event_mapping$form[i]]][['redcap_event_name']]==DB$redcap$event_mapping$unique_event_name[i]) %>% which() %>% length()
    # })
    # for(event in ){
    #   summary[[paste0(event,"_records_n")]] <- DB$data_extract[[]][which(DB$redcap$arms$arm_num==arm)]
    # }
  }

  #instruments/forms belong to events many to 1 (if no events/arms) ----------------
  summary$instruments_n <- 0
  if(is.data.frame(DB$redcap$instruments)){ # can add expected later
    summary$instruments_n <- DB$redcap$instruments %>% nrow()
    DB$redcap$instruments$incomplete <- DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Incomplete") %>% which() %>% length()
    })
    DB$redcap$instruments$unverified <- DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Unverified") %>% which() %>% length()
    })
    DB$redcap$instruments$complete <- DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Complete") %>% which() %>% length()
    })
  }

  #fields belong to instruments/forms 1 to 1 ----------------
  summary$metadata_n <- 0

  summary$metadata_n <- DB$redcap$metadata[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive")),] %>% nrow()
  # DB$redcap$metadata$field_type[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive"))] %>% table()

  #metadata/codebook =============
  metadata <- DB$redcap$metadata
  codebook <- DB$redcap$codebook %>% dplyr::select("field_name", "code", "name")

  codebook <- unique(metadata$field_name) %>%
    lapply(function(IN){
      codebook[which(codebook$field_name==IN),]
    }) %>% dplyr::bind_rows()

  codebook <- codebook %>% merge(
    metadata %>% dplyr::select(
      "form_name","field_name","field_label","field_type","field_type_R","text_validation_type_or_show_slider_number"),by="field_name",sort=F)
  codebook$form_name <- 1:nrow(codebook) %>% lapply(function(i){
    form_name <- codebook$form_name[i]
    field_name <- codebook$field_name[i]

    if(!form_name %in% names(DB$data_extract)){
      if(DB$internals$merge_form_name %in% names(DB$data_extract)){
        if(field_name%in%colnames(DB$data_extract$merged))return(DB$internals$merge_form_name)
      }
      for(other in names(DB$data_extract)[which(!names(DB$data_extract)%in%DB$redcap$instruments$instrument_name)]){
        if(field_name%in%colnames(DB$data_extract[[other]]))return(other)
      }
    }
    return(form_name)
  }) %>% unlist()

  codebook$field_name_raw <- codebook$field_name
  codebook$field_name_raw[which(codebook$field_type=="checkbox_choice")] <- codebook$field_name[which(codebook$field_type=="checkbox_choice")] %>%
    strsplit("___") %>%
    sapply(function(X){X[[1]]})

  codebook$field_label_raw <- codebook$field_label
  codebook$field_label_raw[which(codebook$field_type=="checkbox_choice")] <- codebook$field_name_raw[which(codebook$field_type=="checkbox_choice")] %>%
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
  DB$redcap$codebook <- codebook
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
  merged <- DB$data_extract[[DB$internals$merge_form_name]]
  if(nrow(merged)>0){
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
    add_ons  <- add_ons[which(add_ons%in%colnames(merged))]
    if(!instrument_name%in%DB$redcap$instruments$instrument_name)stop("instrument_name must be included in set of DB$redcap$instruments$instrument_name")
    #instrument_name <-  DB$redcap$instruments$instrument_name %>% sample(1)
    is_repeating_instrument <- instrument_name%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
    rows  <- 1:nrow(merged)
    if(is_repeating_instrument){
      # rows <- which(merged$redcap_repeat_instrument==instrument_name)
    }
    if(!is_repeating_instrument){
      rows <- which(!is.na(merged[[paste0(instrument_name,"_complete")]]))
    }
    #
    # if(!DB$redcap$is_longitudinal){
    #   if("redcap_repeat_instrument"%in%colnames(merged)){
    #     if(is_repeating_instrument){
    #       rows <- which(merged$redcap_repeat_instrument==instrument_name)
    #     }
    #     if(!is_repeating_instrument){
    #       rows <- which(is.na(merged$redcap_repeat_instrument))
    #     }
    #   }
    # }
    # if(DB$redcap$is_longitudinal){
    #   events_ins <- DB$redcap$event_mapping$unique_event_name[which(DB$redcap$event_mapping$form==instrument_name)] %>% unique()
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

write_xl <- function(DF,DB,path,str_trunc_length=32000,with_links = T){# add instance links
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "sheet")
  COL <- which(colnames(DF)==DB$redcap$id_col)
  DF <-  DF %>% lapply(stringr::str_trunc, str_trunc_length, ellipsis = "") %>% as.data.frame()
  if(nrow(DF)>0&&length(COL)>0&&with_links){
    DF_structure_cols <- DB$redcap$raw_structure_cols[which(DB$redcap$raw_structure_cols%in%colnames(DF))]
    DF_structure_cols <- DB$redcap$raw_structure_cols[which(DB$redcap$raw_structure_cols%in%colnames(DF)&DB$redcap$raw_structure_cols!=DB$redcap$id_col)]
    link_tail <- "&id=" %>% paste0(DF[[DB$redcap$id_col]])
    if("redcap_repeat_instrument"%in%DF_structure_cols) link_tail <- link_tail %>% paste0("&page=",DF[["redcap_repeat_instrument"]])
    if("redcap_repeat_instance"%in%DF_structure_cols) link_tail <- link_tail %>% paste0("&instance=",DF[["redcap_repeat_instance"]])
    DF$redcap_link <- paste0("https://redcap.miami.edu/redcap_v",DB$redcap$version,"/DataEntry/record_home.php?pid=",DB$redcap$project_id,link_tail)
    if("arm_num"%in%colnames(DF)){
      DF$redcap_link <- DF$redcap_link %>% paste0("&arm=", DF[["arm_num"]])
    }
    class(DF$redcap_link) <- "hyperlink"
    openxlsx::writeData(wb, sheet = 1, x = DF$redcap_link,startRow = 2,startCol = COL)
    DF$redcap_link <- NULL
  }
  openxlsx::writeData(wb, sheet = 1, x = DF)
  openxlsx::saveWorkbook(
    wb = wb,
    file = path, overwrite = TRUE)
  message("Saved at -> ","'",path,"'")
}

list.files.real <- function(path){
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
      if(length(thing)>0){
        if(is.list(thing)){
          out <- T
        }else{
          if(!is.na(thing)){
            out <- T
          }
        }
      }
    }
  }
  return(out)
}
