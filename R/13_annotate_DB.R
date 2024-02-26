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

annotate_codebook <- function(metadata){
  codebook <- DB$redcap$codebook
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
clean_DB <- function(DB,drop_blanks=T,drop_unknowns=T,units_df){
  here_is_units_df <- NULL
  if(!missing(units_df)){
    if(!is.data.frame(units_df))stop("units_df must be a dataframe")
    if(nrow(units_df)>0){
      here_is_units_df <- units_df
    }
  }
  for (data_choice in c("data_extract","data_transform")) {
    for(FORM in names(DB[[data_choice]])){
      for(COLUMN in colnames(DB[[data_choice]][[FORM]])){
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
                levels <- levels[which(levels%in%unique(DB[[data_choice]][[FORM]][[COLUMN]]))]
              }
              if(!drop_unknowns){
                levels <- levels %>% append(unique(DB[[data_choice]][[FORM]][[COLUMN]])) %>% unique() %>% drop_nas()
              }
            }
            if(class == "integer"){
            }
            DB[[data_choice]][[FORM]]
          }
        }
        DB[[data_choice]][[FORM]][[COLUMN]] <- DB[[data_choice]][[FORM]][[COLUMN]] %>% clean_column_for_table(
          class = class,
          label = label,
          units = units,
          levels = levels
        )
      }
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
