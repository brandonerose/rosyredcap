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
          if(any(unlist(lapply(thing,function(x){length(x)>0})))){
            out <- T
          }
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
