upload_file_to_redcap <- function(file,record, field,repeat_instance = NULL,event=NULL){
  # DB <- validate_DB(DB)
  file<-normalizePath(file)
  if(!file.exists(file)) stop("File does not exist! --> ",file)
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
      action='import',
      content='file',
      record =record,
      field =field,
      event =event,
      repeat_instance =repeat_instance,
      returnFormat='csv',
      file=httr::upload_file(file)
    ),
    encode = "multipart"
  )
  if(httr::http_error(response))stop("File upload failed")
  message("File deleted! --> ",file)
}

delete_file_from_redcap <- function(DB,record, field,repeat_instance = NULL, event = NULL){
  # DB <- validate_DB(DB)
  response <- httr::POST(
    url = DB$redcap_uri,
    body = list(
      "token"=validate_redcap_token(DB),
      action='delete',
      content='file',
      record =record,
      field =field,
      event =event,
      repeat_instance = repeat_instance,
      returnFormat='csv'
    )
  )
  if(httr::http_error(response))stop("File Delete failed")
  message("File Deleted!")
}
