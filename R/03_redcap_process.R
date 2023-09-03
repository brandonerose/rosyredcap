raw_process_redcap <- function(raw,DB,clean=T){
  has_event_mappings <- "redcap_event_name" %in% colnames(raw)
  has_repeating <- "redcap_repeat_instrument" %in% colnames(raw)
  if(nrow(raw)>0){
    if(clean){
      if("redcap_repeat_instrument" %in% colnames(raw)){
        raw$redcap_repeat_instrument <- raw$redcap_repeat_instrument %>% sapply(function(redcap_repeat_instrument){
          OUT <- NA
          if(!is.na(redcap_repeat_instrument)){
            OUT <-DB$instruments$instrument_name[which(DB$instruments$instrument_label==redcap_repeat_instrument)]
          }
          OUT
        })
      }
      if("redcap_event_name" %in% colnames(raw)){
        raw$redcap_event_name <- raw$redcap_event_name %>% sapply(function(redcap_event_name){
          OUT <- NA
          if(!is.na(redcap_event_name)){
            OUT <-DB$events$unique_event_name[which(DB$events$event_name==redcap_event_name)]
          }
          OUT
        })
      }
    }
    add_ons <- NULL
    if(has_event_mappings){
      add_ons <- add_ons %>% append("redcap_event_name")
    }
    if(has_repeating){
      add_ons <- add_ons %>% append("redcap_repeat_instrument")
      add_ons <- add_ons %>% append("redcap_repeat_instance")
    }
    for(instrument_name in DB$instruments$instrument_name){
      DB[["data"]][[instrument_name]]<-raw[,unique(c(DB$id_col,add_ons,DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&DB$metadata$field_name%in%colnames(raw))]))]
      if(has_event_mappings){
        events_ins <- DB$event_mapping$unique_event_name[which(DB$event_mapping$form==instrument_name)] %>% unique()
        DB[["data"]][[instrument_name]] <-DB[["data"]][[instrument_name]][which(DB[["data"]][[instrument_name]]$redcap_event_name%in%events_ins),]
      }
      if(has_repeating){
        is_repeating_instrument <- instrument_name%in%DB$instruments$instrument_name[which(DB$instruments$repeating)]
        if(!is_repeating_instrument){
          DB[["data"]][[instrument_name]] <- DB[["data"]][[instrument_name]][which(is.na(DB[["data"]][[instrument_name]]$redcap_repeat_instrument)),]
          DB[["data"]][[instrument_name]]$redcap_repeat_instrument <- NULL
          DB[["data"]][[instrument_name]]$redcap_repeat_instance <- NULL
        }
        if(is_repeating_instrument){
          DB[["data"]][[instrument_name]] <-DB[["data"]][[instrument_name]][which(DB[["data"]][[instrument_name]]$redcap_repeat_instrument==instrument_name),]
        }
      }
      DB[["data"]][[instrument_name]] <- DB[["data"]][[instrument_name]] %>% all_character_cols()
    }
  }
  DB$clean<-clean
  DB$has_event_mappings <- has_event_mappings
  DB$has_repeating <- has_repeating
  DB
}

#' @title Select REDCap records from DB
#' @inheritParams save_DB
#' @param records character vector of the IDs you want to filter the DB by
#' @return DB object that has been filtered to only include the specified records
#' @export
select_redcap_records<-function(DB, records=NULL){
  DB_selected<-DB
  if(!is.null(records)){
    if (length(records)==0)stop("Must supply records")
    DB_selected$data<-list()
    BAD <-records[which(!records%in%DB$all_records)]
    GOOD <-records[which(records%in%DB$all_records)]
    if(length(BAD)>0)stop("Following records are not found in DB: ", BAD %>% paste0(collapse = ", "))
    if (length(GOOD)==0)stop("Must supply valid records")
    for(FORM in names(DB$data)){
      DB_selected[["data"]][[FORM]] <-DB[["data"]][[FORM]][which(DB[["data"]][[FORM]][[DB$id_col]]%in%GOOD),]
    }
  }
  DB_selected
}

clean_to_raw_redcap <- function(DB){
  DB <- validate_DB(DB)
  for(TABLE in names(DB[["data"]])){
    DB[["data"]][[TABLE]] <- clean_to_raw_form(FORM = DB[["data"]][[TABLE]],DB=DB)
  }
  DB$clean<-F
  DB
}

#' @title Clean to Raw REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of clean REDCap to be converted to raw REDCap (for uploads)
#' @return DB object that has been filtered to only include the specified records
#' @export
clean_to_raw_form <- function(FORM,DB){
  use_missing_codes <- is.data.frame(DB$missing_codes)
  # if(!deparse(substitute(FORM))%in%DB$instruments$instrument_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  if(any(!colnames(FORM)%in%DB$metadata$field_name))stop("All column names in your form must match items in your metadata, `DB$metadata$field_name`")
  instrument <- DB$metadata$form_name[
    which(
      DB$metadata$field_name%in%colnames(FORM)&
        !DB$metadata$field_name%in%c(DB$id_col,"redcap_repeat_instance","redcap_repeat_instrument")
    )
  ] %>% unique()
  if(length(instrument)>1)stop("All column names in your form must match only one form in your metadata, `DB$instruments$instrument_name`")
  metadata<-DB$metadata[which(DB$metadata$form_name==instrument),]
  for(COL_NAME in FORM %>% colnames()){
    if(COL_NAME%in%metadata$field_name[which(metadata$field_type%in%c("radio","dropdown","checkbox_choice"))]){
      z<-metadata$select_choices_or_calculations[which(metadata$field_name==COL_NAME)] %>% split_choices()
      FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          coded_redcap<-which(z$name==C)
          if(length(coded_redcap)>0){
            OUT<-z$code[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2<-which(DB$missing_codes$name==C)
              if(length(coded_redcap2)>0){
                OUT<-DB$missing_codes$code[coded_redcap2]
              }else{
                stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }
    if(COL_NAME%in%metadata$field_name[which(metadata$field_type=="yesno")]){
      z<-data.frame(
        code=c(0,1),
        name=c("No","Yes")
      )
      FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
        OUT<-NA
        if(!is.na(C)){
          D<-which(z$name==C)
          if(length(D)>0){
            OUT<-z$code[D]
          }
          if(length(D)==0){
            if(use_missing_codes){
              E<-which(DB$missing_codes$name==C)
              if(length(E)==0){
                stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
              }
              if(length(E)>0){
                OUT<-DB$missing_codes$code[E]
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)!, Column: ", COL_NAME,", Choice: ",C)
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }
    if(use_missing_codes){
      if(COL_NAME%in%metadata$field_name[which(!metadata$field_type%in%c("radio","dropdown","yesno","checkbox_choice"))]){
        FORM[[COL_NAME]]<-FORM[[COL_NAME]] %>% sapply(function(C){
          OUT<-C
          if(!is.na(C)){
            D<-which(DB$missing_codes$name==C)
            if(length(D)>0){
              OUT<-DB$missing_codes$code[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  FORM
}

clean_redcap_log <- function(log){
  log$record_id <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),gsub("Update record|Delete record|Create record|[:(:]API[:):]|Auto|calculation| |[:):]|[:(:]","",A),NA)})
  log$action_type <- log$action %>% sapply(function(A){ifelse(grepl("Update record |Delete record |Create record ",A),(A %>% strsplit(" ") %>% unlist())[1],NA)})
  comments <- which(log$action=="Manage/Design"&grepl("Add field comment|Edit field comment|Delete field comment",log$details))
  if(length(comments)>0){
    log$record_id[comments] <- stringr::str_extract(log$details[comments], "(?<=Record: )[^,]+")
    log$action_type[comments] <- "Comment"
  }
  log
}

all_missing_codes <- function(){
  data.frame(
    code = c(
      'NI',
      'INV',
      'UNK',
      'NASK',
      'ASKU',
      'NAV',
      'MSK',
      'NA',
      'NAVU',
      'NP',
      'QS',
      'QI',
      'TRC',
      'UNC',
      'DER',
      'PINF',
      'NINF',
      'OTH'
    ),
    name = c(
      'No information',
      'Invalid',
      'Unknown',
      'Not asked',
      'Asked but unknown',
      'Temporarily unavailable',
      'Masked',
      'Not applicable',
      'Not available',
      'Not present',
      'Sufficient quantity',
      'Insufficient quantity',
      'Trace',
      'Unencoded',
      'Derived',
      'Positive infinity',
      'Negative infinity',
      'Other'
    )
  )
}

missing_codes2 <- function(DB){
  included <- "missing_data_codes"%in%colnames(DB$project_info)
  if(included){
    is_na <-is.na(DB$project_info$missing_data_codes)
    if(!is_na){
      return(DB$project_info$missing_data_codes %>% split_choices())
    }
    if(is_na){
      return(NA)
    }
  }
  if(!included){
    return(NA)
  }
}

#' @title Merge non-repeating, not ready for multi-event projects
#' @inheritParams save_DB
#' @return DB object that has merged all non repeating forms
#' @export
merge_non_repeating_DB <- function(DB){
  if("megrged" %in% names(DB$data))stop("Already merged!")
  instrument_names <- DB$instruments$instrument_name[which(!DB$instruments$repeating)] %>% as.list()
  if (length(instrument_names)==1) stop('No need to merge you only have one form that is non-repeating')
  merged <- merge(DB$data[[instrument_names[[1]]]],DB$data[[instrument_names[[2]]]],by=DB$id_col,all.x = T)
  DB$data[[instrument_names[[1]]]] <- NULL
  DB$data[[instrument_names[[2]]]] <- NULL
  instrument_names[1:2]<-NULL
  while (length(instrument_names)>0) {
    merged <- merge(merged,DB$data[[instrument_names[[1]]]],by=DB$id_col,all.x = T)
    DB$data[[instrument_names[[1]]]] <- NULL
    instrument_names[[1]]<-NULL
  }
  DB$data$merged <- merged
  DB
}

#' @title Unmerge non-repeating, not ready for multi-event projects
#' @inheritParams save_DB
#' @return DB object that has merged all non repeating forms
unmerge_non_repeating_DB <- function(DB){
  if(!"megrged" %in% names(DB$data))stop("No DB$data named as 'merged'!")
  instrument_names <- DB$data$merged %>% colnames() %>% sapply(function(COL){DB$metadata$form_name[which(DB$metadata$field_name==COL)]}) %>% unique() %>% as.list()
  merged <- DB$data$merged
  while (length(instrument_names)>0) {
    instrument_name <-instrument_names[[1]]
    DB$data[[instrument_name]]<-raw[,unique(c(DB$id_col,DB$metadata$field_name[which(DB$metadata$form_name==instrument_name&DB$metadata$field_name%in%colnames(raw))]))]
    instrument_names[[1]] <- NULL
  }
  DB
}

#' @title Deidentify the REDCap DB according to REDCap or your choices
#' @inheritParams save_DB
#' @param identifiers optional character vector of column names that should be excluded from DB. Otherwise `DB$metadata$identifier =="y` will be used.
#' @return DB object that has deidentified forms
#' @export
deidentify_DB <- function(DB,identifiers){
  DB <- validate_DB(DB)
  missing_identifiers <- missing(identifiers)
  if(!missing_identifiers){
    identifiers <- identifiers %>% unique()
    bad_identifiers<-identifiers[which(!identifiers%in%DB$metadata$field_name)]
    if(length(bad_identifiers)>0)stop("You have an identifier that is not included in the set of `DB$metadata$field_name` --> ",bad_identifiers %>% paste0(collapse = ", "))
    if(DB$id_col%in%identifiers)stop("Your REDCap ID, ",DB$id_col,", should not be deidentified.") #If you want to pass a new set of random IDs to make this data use `scramble_ID_DB(DB)`.")
  }
  if(missing_identifiers){
    identifiers<- DB$metadata$field_name[which(DB$metadata$identifier=="y")]
    if(length(identifiers)==0)warning("You have no identifiers marked in `DB$metadata$identifier`. You can set it in REDCap Project Setup and update DB OR define your idenitifiers in this functions `identifiers` argument." ,immediate. = T)
  }
  drop_list <- Map(function(NAME, COLS) {identifiers[which(identifiers %in% COLS)]},names(DB$data), lapply(DB$data, colnames))
  drop_list <- drop_list[sapply(drop_list, length) > 0]
  if(length(drop_list)==0)message("Nothing to deidentify from --> ",identifiers %>% paste0(collapse = ", "))
  for (FORM in names(drop_list)) {
    for(DROP in drop_list[[FORM]]){
      DB$data[[FORM]][[DROP]] <- NULL
      message("Dropped ",DROP," from ", FORM)
    }
  }
  DB
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
  metadata <- DB$metadata
  metadata$field_type_R <- NA
  metadata$field_type_R[which(metadata$field_type %in% c("radio","yesno","dropdown"))] <- "factor"
  metadata$field_type_R[which(metadata$text_validation_type_or_show_slider_number == "integer")] <- "integer"
  here_is_units_df <- NULL
  if(!missing(units_df)){
    if(!is.data.frame(units_df))stop("units_df must be a dataframe")
    if(nrow(units_df)>0){
      here_is_units_df <- units_df
    }
  }
  for(FORM in names(DB$data)){
    for(COLUMN in colnames(DB$data[[FORM]])){
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
            if(drop_blanks){
              levels <- levels[which(levels%in%unique(DB$data[[FORM]][[COLUMN]]))]
            }
            if(!drop_unknowns){
              levels <- levels %>% append(unique(DB$data[[FORM]][[COLUMN]])) %>% unique() %>% drop_nas()
            }
          }
          if(class == "integer"){

          }
          DB$data[[FORM]]
        }
      }
      DB$data[[FORM]][[COLUMN]]<-DB$data[[FORM]][[COLUMN]] %>% clean_column_for_table(
        class = class,
        label = label,
        units = units,
        levels = levels
      )
    }
  }
  DB
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
clean_column_for_table<-function(col,class,label,units,levels){
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

#' @title add REDCap ID to any dataframe using a ref_id
#' @description
#'  add REDCap ID to any dataframe using a ref_id
#' @param DF dataframe
#' @inheritParams save_DB
#' @param ref_id column name that matches a REDCap variable name that could be an ALT id such as MRN
#' @return original dataframe with REDCap id_col added as the first column
#' @export
add_ID_to_DF<-function(DF,DB,ref_id){
  if(!ref_id%in%DB$metadata$field_name)stop("The ref_id not valid. Must be a REDCap raw colname")
  form<-DB$metadata$form_name[which(DB$metadata$field_name==ref_id)]
  DF[[ref_id]] %>% sapply(function(ID){
    DB$data[[form]][[DB$id_col]][which(DB$data[[form]][[ref_id]]==ID)]
  }) %>% as.data.frame()->y
  colnames(y)<-"record_id"
  DF<-cbind(y,DF)
  DF
}

#' @title grab data table for an individual(s)
#' @description
#' grab data table for an individual(s)
#' @inheritParams select_redcap_records
#' @return list of data tables
#' @export
grab_record_tables<-function(DB, records){
  OUT <-list()
  for(TABLE in names(DB$data)){
    OUT[[TABLE]] <-   DB[["data"]][[TABLE]][which(DB[["data"]][[TABLE]][[DB$id_col]]%in%records),]
  }
  OUT
}
