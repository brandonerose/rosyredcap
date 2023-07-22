raw_process_redcap <- function(raw,DB,clean=T){
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
    has_event_mappings <- "redcap_event_name" %in% colnames(raw)
    has_repeating <- "redcap_repeat_instrument" %in% colnames(raw)
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
  DB
}

select_redcap_records<-function(DB, records=NULL){
  DB_selected<-DB
  DB_selected$data<-list()
  if(!is.null(records)){
    BAD <-records[which(!records%in%DB$data$identity[[DB$id_col]])]
    if(length(BAD)>0)stop(message("Following records are not found in DB: ", paste0(BAD,collapse = ", ")))
  }
  for(x in DB$instruments$instrument_name){
    OUT <- DB[["data"]][[x]]
    if(!is.null(records)){
      OUT<-OUT[which(OUT[[DB$id_col]]%in%records),]
    }
    DB_selected[["data"]][[x]]<-OUT
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

clean_to_raw_form <- function(FORM,DB){
  use_missing_codes <- !is.na(DB$missing_codes)
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

merge_non_repeating_DB <- function(DB){
  if("megrged" %in% names(DB$data))stop("Already merged!")
  instrument_names <- DB$instruments$instrument_name[which(!DB$instruments$repeating)] %>% as.list()
  if (!length(instrument_names)>1) stop('No need to merge you only have one form that is non-repeating')
  merged <- merge(DB$data[[instrument_names[[1]]]],DB$data[[instrument_names[[2]]]],by=DB$id_col)
  DB$data[[instrument_names[[1]]]] <- NULL
  DB$data[[instrument_names[[2]]]] <- NULL
  instrument_names[1:2]<-NULL
  while (length(instrument_names)>0) {
    merged <- merge(merged,DB$data[[instrument_names[[1]]]],by=DB$id_col)
    DB$data[[instrument_names[[1]]]] <- NULL
    instrument_names[[1]]<-NULL
  }
  DB$data$merged <- merged
  DB
}

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

