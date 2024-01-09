summarize_DB <- function (DB,drop_dir=T){
  # dropdir ----
  if(!"merged"%in%names(DB$data)){
    DB$data$merged <- merge_non_repeating_DB(DB)[["data"]][["merged"]]
  }
  if(drop_dir){DB %>% drop_redcap_dir()}
  #metadata/codebook =============
  metadata <- DB$metadata
  codebook <- DB$codebook
  merged <- DB$data$merged

  codebook<-codebook %>% merge(
    metadata %>% dplyr::select(
      "form_name","field_name","field_label","field_type","text_validation_type_or_show_slider_number"),by="field_name")


  metadata$field_label[which(is.na(metadata$field_label))] <- metadata$field_name[which(is.na(metadata$field_label))]

  codebook$field_name_raw <- codebook$field_name
  codebook$field_name_raw[which(codebook$field_type=="checkbox_choice")]<-codebook$field_name[which(codebook$field_type=="checkbox_choice")] %>%
    strsplit("___") %>%
    sapply(function(X){X[[1]]})

  codebook$field_label_raw <- codebook$field_label
  codebook$field_label_raw[which(codebook$field_type=="checkbox_choice")]<-codebook$field_name_raw[which(codebook$field_type=="checkbox_choice")] %>%
    sapply(function(X){
      metadata$field_label[which(metadata$field_name==X)] %>% unique()
    })

  metadata <- metadata[which(!metadata$field_type=="checkbox_choice"),]
  # i <- 1:nrow(codebook) %>% sample(1)
  codebook$n <- 1:nrow(codebook) %>% lapply(function(i){
    sum(merged[,codebook$field_name[i]]==codebook$name[i],na.rm = T)
  }) %>% unlist()
  codebook$n_total <- 1:nrow(codebook) %>% lapply(function(i){
    sum(!is.na(merged[,codebook$field_name[i]]),na.rm = T)
  }) %>% unlist()
  codebook$perc <-  codebook$n/codebook$n_total
  codebook$perc_text <- codebook$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
  path <- file.path(get_dir(DB), "output", "annotated_codebook.xlsx")
  codebook %>% rio::export(path)
  message("Saved at -> ","'",path,"'")
  metadata <-unique(metadata$form_name) %>%
    lapply(function(IN){
      metadata[which(metadata$form_name==IN),]
    }) %>% dplyr::bind_rows()
  codebook <-unique(metadata$form_name) %>%
    lapply(function(IN){
      codebook[which(codebook$form_name==IN),]
    }) %>% dplyr::bind_rows()
  list(
    metadata = metadata,
    codebook = codebook
  ) %>% return()
}
