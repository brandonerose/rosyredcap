annotate_codebook <- function (DB){

  codebook$form_name <- 1:nrow(codebook) %>% lapply(function(i){
    form_name <- codebook$form_name[i]
    field_name <- codebook$field_name[i]

    if(!form_name %in% names(DB$data)){
      if("merged" %in% names(DB$data)){
        if(field_name%in%colnames(DB$data$merged))return("merged")
      }
      if("patient" %in% names(DB$data)){
        if(field_name%in%colnames(DB$data$merged))return("patient")
      }
      for(other in names(DB$data)[which(!names(DB$data)%in%DB$instruments$instrument_name)]){
        if(field_name%in%colnames(DB$data$other))return(other)
      }
    }
    return(form_name)
  }) %>% unlist()

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
