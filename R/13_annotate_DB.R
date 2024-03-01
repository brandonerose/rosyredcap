annotate_metadata <- function(metadata){
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
  metadata$in_original_redcap <- metadata$field_name %in% DB$redcap$metadata$field_name
  if(!"units" %in% colnames(metadata))metadata$units <- NA
  if(!"field_label_short" %in% colnames(metadata)) metadata$field_label_short <- metadata$field_label
  # if(!"field_label_short" %in% colnames(metadata))metadata$ <- metadata$field_label
  return(metadata)
}
metadata_to_codebook <- function(metadata){
  rows_with_choices <- which(metadata$field_type%in%c("radio","dropdown","checkbox_choice","yesno"))
  codebook <- NULL
  if(length(rows_with_choices)>0){
    for(field_name in metadata$field_name[rows_with_choices]){
      choices <- metadata$select_choices_or_calculations[which(metadata$field_name==field_name)] %>% split_choices()
      codebook <- codebook %>% dplyr::bind_rows(
        data.frame(
          field_name = field_name,
          code = choices$code,
          name =choices$name
        )
      )
    }
  }
  rownames(codebook) <- NULL
  return(codebook)
}
annotate_instruments <- function(instruments){
  choice <- "instrument_name"
  if("former_instrument_names" %in% colnames(instruments)){
    choice <- "former_instrument_names"
  }
  for(status in c("Incomplete","Unverified","Complete")){
    instruments[[tolower(status)]] <- instruments[[choice]] %>% sapply(function(former_instrument_names){
      former_instrument_names %>% strsplit(" [:|:] ") %>% unlist() %>%  sapply(function(instrument_name){
        (DB[["data_extract"]][[instrument_name]][[paste0(instrument_name,"_complete")]]==status) %>% which() %>% length()
      }) %>% paste0(collapse = " | ")
    })
  }
  return(instruments)
}

annotate_codebook <- function(codebook,metadata,data_choice="data_extract"){

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
    if(!form_name %in% names(DB[[data_choice]])){
      if(DB$internals$merge_form_name %in% names(DB[[data_choice]])){
        if(field_name%in%colnames(DB[[data_choice]]$merged))return(DB$internals$merge_form_name)
      }
      for(other in names(DB[[data_choice]])[which(!names(DB[[data_choice]])%in%DB$redcap$instruments$instrument_name)]){
        if(field_name%in%colnames(DB[[data_choice]][[other]]))return(other)
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
    sum(DB[[data_choice]][[codebook$form_name[i]]][,codebook$field_name[i]]==codebook$name[i],na.rm = T)
  }) %>% unlist()
  codebook$n_total <- 1:nrow(codebook) %>% lapply(function(i){
    sum(!is.na(DB[[data_choice]][[codebook$form_name[i]]][,codebook$field_name[i]]),na.rm = T)
  }) %>% unlist()
  codebook$perc <-  (codebook$n/codebook$n_total) %>% round(4)
  codebook$perc_text <- codebook$perc %>% magrittr::multiply_by(100) %>% round(1) %>% paste0("%")
  DB$redcap$codebook <- codebook
  return(codebook)
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
clean_DB <- function(DB,drop_blanks=T,drop_unknowns=T){
  for (data_choice in c("data_extract","data_transform")) {
    if(data_choice=="data_extract"){
      metadata <- DB$redcap$metadata %>% annotate_metadata()
    }else{
      metadata <- DB$remap$metadata_new %>% annotate_metadata()
    }
    for(FORM in names(DB[[data_choice]])){
      DB[[data_choice]][[FORM]] <- DB[[data_choice]][[FORM]] %>% clean_DF(metadata=metadata,drop_blanks= drop_blanks,drop_unknowns=drop_unknowns)
    }
  }
  return(DB)
}
clean_DF <- function(DF,metadata,drop_blanks= T,drop_unknowns=T){
  for(COLUMN in colnames(DF)){
    if(COLUMN %in% metadata$field_name){
      units <- NULL
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
            levels <- levels[which(levels%in%unique(DF[[COLUMN]]))]
          }
          if(!drop_unknowns){
            levels <- levels %>% append(unique(DF[[COLUMN]])) %>% unique() %>% drop_nas()
          }
        }
        if(class == "integer"){
        }
        DF
      }
    }
    DF[[COLUMN]] <- DF[[COLUMN]] %>% clean_column_for_table(
      class = class,
      label = label,
      units = units,
      levels = levels
    )
  }
  return(DF)
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
