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
  # if(add_last_update){
  #   rows <- which(!is.na(log$record))
  #   records$last_update <- records[[DB$redcap$id_col]] %>% sapply(function(record_id){
  #     for(i in rows){
  #       if(log$record[i]==record_id) return(log$timestamp[i])
  #     }
  #     return(NA)
  #   })
  #   DB$redcap$log -> c
  # }
  records
}
#' @title Summarize DB
#' @inheritParams save_DB
#' @param records character vector of records to be summarized
#' @param drop_blanks optional logical for dropping blanks
#' @export
summarize_DB <- function(DB,records,drop_blanks = T){
  #project --------
  # DB$summary$users <- DB$redcap$users
  df_names0 <- df_names1 <- df_names2 <- c("metadata","instruments","event_mapping","events","arms")
  data_choice <- "data_extract"
  if(DB$internals$was_remapped){
    df_names2 <- c(paste0(df_names1,"_new"),paste0(df_names1,"_remap"))
    df_names1 <- c(df_names1,paste0(df_names1,"_remap"))
    data_choice <- "data_transform"
  }
  if(!missing(records)) DB[[data_choice]] <- DB %>% filter_DB(records = records,data_choice = data_choice)
  for(i in 1:length(df_names1)){
    x <- DB[[DB$internals$reference_metadata]][[df_names2[i]]]
    if(!is.null(x)) DB$summary[[df_names1[i]]] <- x
  }
  #records belong to arms 1 to 1 ----------
  DB$summary$records_n <- 0
  if(!is.null(DB$summary$all_records)){
    if(!missing(records)) {
      if(!is.null(records)) DB$summary$all_records <- DB$summary$all_records[which( DB$summary$all_records[[DB$redcap$id_col]]%in% records),]
    }
    DB$summary$records_n <- DB$summary$all_records %>% nrow()
  }
  #arms----------------
  DB$summary$arms_n <- 0
  if(is.data.frame(DB$redcap$arms)){
    DB$summary$arms_n <- DB$redcap$arms %>% nrow()
    id_pairs <- DB$redcap$instruments$instrument_name %>%  lapply(function(IN){DB$data_extract[[IN]][,c(DB$redcap$id_col,"arm_num")]}) %>% dplyr::bind_rows() %>% unique()
    DB$redcap$arms$arm_records_n <- DB$redcap$arms$arm_num %>% sapply(function(arm){
      which(id_pairs$arm_num==arm)%>% length()
    })
  }
  #events belong to arms many to 1 ----------------
  # DB$summary$events_n <- DB$redcap$events %>% nrow()
  DB$summary$events_n <- 0
  if(is.data.frame(DB$redcap$events)){
    DB$summary$events_n <- DB$redcap$events %>% nrow()
    DB$summary$event_names_n <- DB$redcap$events$event_name %>% unique() %>% length()
    # 1:nrow(DB$redcap$event_mapping) %>% lapply(function(i){
    #   (DB$data_extract[[DB$redcap$event_mapping$form[i]]][['redcap_event_name']]==DB$redcap$event_mapping$unique_event_name[i]) %>% which() %>% length()
    # })
    # for(event in ){
    #   DB$summary[[paste0(event,"_records_n")]] <- DB$data_extract[[]][which(DB$redcap$arms$arm_num==arm)]
    # }
  }
  #instruments/forms belong to events many to 1 (if no events/arms) ----------------
  DB$summary$instruments_n <- 0
  if(is.data.frame(DB$summary$instruments)){ # can add expected later
    DB$summary$instruments_n <- DB$summary$instruments %>% nrow()
    DB$summary$instruments <- DB  %>% annotate_instruments(DB$summary$instruments)
    if(is_something(DB$summary$instruments_remap)){
      DB$summary$instruments_remap <- DB %>% annotate_instruments(DB$summary$instruments_remap)
    }
  }
  #fields belong to instruments/forms 1 to 1 ----------------
  DB$summary$metadata_n <- 0
  DB$summary$metadata_n <- DB$redcap$metadata[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive")),] %>% nrow()
  # DB$redcap$metadata$field_type[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive"))] %>% table()
  DB$summary$metadata <- DB %>%  annotate_metadata(metadata = DB$summary$metadata)
  #metadata/codebook =============
  codebook <- metadata_to_codebook(DB$summary$metadata) %>% annotate_codebook(DB$summary$metadata,data_choice = "data_transform",DB = DB)
  if(drop_blanks) codebook <- codebook[which(codebook$n>0),]
  DB$summary$codebook <- codebook
  #cross_codebook ------
  return(DB)
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
