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


split_choices<-function(x){
  x<-gsub("\n", " | ",x)  #added this to account for redcap metadata output if not a number
  x<-x %>% strsplit(" [:|:] ") %>% unlist()
  data.frame(
    code=x %>% strsplit(", ") %>% sapply(`[`, 1),
    name=x %>% strsplit(", ")%>% sapply(`[`, -1) %>% sapply(function(y){paste0(y,collapse = ", ")})
  )
}

make_codebook<-function(DB){
  OUT <- NULL
  for (CHOICE in names(DB[["choices"]])){
    x<-DB[["choices"]][[CHOICE]]
    x$field_name <- CHOICE
    OUT <- OUT %>% dplyr::bind_rows(x)
  }
  OUT<-OUT %>% dplyr::select("field_name","code","name")
  OUT
}

write_xl<-function(DF,DB,path){# add instance links
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "sheet")
  COL<-which(colnames(DF)==DB$id_col)
  if(nrow(DF)>0&&length(COL)>0){
    DF$redcap_link<-paste0("https://redcap.miami.edu/redcap_v",DB$version,"/DataEntry/record_home.php?pid=",DB$PID,"&id=",DF[[DB$id_col]],"&arm=1")
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

find_the_diff2 <- function (new, old,ref_cols=NULL){
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
