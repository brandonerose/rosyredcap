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
  x<-x %>% strsplit(" [:|:] ") %>% unlist() %>% strsplit(", ")
  data.frame(code=x %>% purrr::map(1) %>% unlist(),name=x %>% purrr::map(2) %>% unlist())
}

make_codebook<-function(DB){
  OUT <- NULL
  for (CHOICE in names(DB[["choices"]])){
    x<-DB[["choices"]][[CHOICE]]
    x$field_name <- CHOICE
    OUT <- OUT %>% dplyr::bind_rows(x)
  }
  OUT<-OUT %>% dplyr::select(field_name,code,name)
  OUT
}

write_xl<-function(DF,DB,path){
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
