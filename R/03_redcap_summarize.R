annotate_codebook <- function (DB){


  #metadata/codebook =============
  metadata <- DB$metadata
  codebook <- DB$codebook %>% dplyr::select("field_name", "code", "name")

  codebook <- unique(metadata$field_name) %>%
    lapply(function(IN){
      codebook[which(codebook$field_name==IN),]
    }) %>% dplyr::bind_rows()

  codebook<-codebook %>% merge(
    metadata %>% dplyr::select(
      "form_name","field_name","field_label","field_type","text_validation_type_or_show_slider_number"),by="field_name",sort=F)
  codebook$form_name <- 1:nrow(codebook) %>% lapply(function(i){
    form_name <- codebook$form_name[i]
    field_name <- codebook$field_name[i]

    if(!form_name %in% names(DB$data)){
      if("merged" %in% names(DB$data)){
        if(field_name%in%colnames(DB$data$merged))return("merged")
      }
      if("patient" %in% names(DB$data)){
        if(field_name%in%colnames(DB$data$patient))return("patient")
      }
      for(other in names(DB$data)[which(!names(DB$data)%in%DB$instruments$instrument_name)]){
        if(field_name%in%colnames(DB$data[[other]]))return(other)
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
    sum(DB$data[[codebook$form_name[i]]][,codebook$field_name[i]]==codebook$name[i],na.rm = T)
  }) %>% unlist()
  codebook$n_total <- 1:nrow(codebook) %>% lapply(function(i){
    sum(!is.na(DB$data[[codebook$form_name[i]]][,codebook$field_name[i]]),na.rm = T)
  }) %>% unlist()
  codebook$perc <-  codebook$n/codebook$n_total
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
  for(form in names(DB$data)){
    DF <- DB$data[[form]]
    for(col in colnames(DF)){
      if (!exact){
        DF[[col]] <- tolower(DF[[col]])
      }
      rows <- which(grepl(text,DF[[col]]))
      if(length(rows)>0){
        out <- out %>%dplyr::bind_rows(
          data.frame(
            record_id = DF[[DB$id_col]][rows],
            col = col,
            row = as.character(rows)
          )
        )
      }
    }
  }
  return(out)
}

# summarize_DB <- function(DB){
#
#   #project
#   #records belong to arms 1 to 1
#   DB$summary$records_n <- 0
#   if(!is.null(DB$all_records)){
#     DB$summary$records_n <- DB$all_records %>% length()
#   }
#   #arms
#   DB$summary$arms_n <- 0
#   if(is.data.frame(DB$arms)){
#     DB$summary$arms_n <- DB$arms %>% nrow()
#     id_pairs <- DB$instruments$instrument_name %>%  lapply(function(IN){DB$data[[IN]][,c(DB$id_col,"arm_num")]}) %>% dplyr::bind_rows() %>% unique()
#     DB$arms$arm_records_n <- DB$arms$arm_num %>% sapply(function(arm){
#       which(id_pairs$arm_num==arm)%>% length()
#     })
#     for(arm in DB$arms$arm_num){
#       DB$summary[[paste0("arm_",arm,"_records_n")]] <- DB$arms$arm_records_n[which(DB$arms$arm_num==arm)]
#     }
#   }
#   #events belong to arms many to 1
#   # DB$summary$events_n <- DB$events %>% nrow()
#   DB$summary$events_n <- 0
#   if(is.data.frame(DB$events)){
#     DB$summary$events_n <- DB$events %>% nrow()
#     DB$summary$event_names_n <- DB$events$event_name %>% unique() %>% length()
#     # DB$events$unique_event_name %>% lapply(function(event){
#     #   DB$instruments$instrument_name %>%  sapply(function(IN){
#     #     which(DB$data[[IN]][["redcap_event_name"]]==event) %>% length()
#     #   }) %>% sum()
#     # })
#     # for(event in ){
#     #   DB$summary[[paste0(event,"_records_n")]] <- DB$data[[]][which(DB$arms$arm_num==arm)]
#     # }
#   }
#
#   #instruments/forms belong to events many to 1 (if no events/arms)
#   DB$summary$instruments_n <- 0
#   if(is.data.frame(DB$instruments)){
#     DB$summary$instruments_n <- DB$instruments %>% nrow()
#   }
#
#   #fields belong to instruments/forms 1 to 1
#   DB$summary$metadata_n <- 0
#   if(is.data.frame(DB$metadata)){
#     DB$summary$metadata_n <- DB$metadata[which(!DB$metadata$field_type%in%c("checkbox_choice","descriptive")),] %>% nrow()
#     DB$metadata$field_type[which(!DB$metadata$field_type%in%c("checkbox_choice","descriptive"))] %>% table()
#   }
#   DB$summary <- DB$summary
#   DB
# }
