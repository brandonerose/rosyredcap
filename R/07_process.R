raw_process_redcap <- function(raw,DB){
  if(nrow(raw)>0){
    if(is.null(DB$internals$data_extract_merged)){
      DB$internals$data_extract_merged <- F
    }
    was_merged <- F
    if(DB$internals$data_extract_merged){
      was_merged <- T
      DB <- unmerge_non_repeating_DB(DB)
    }
    raw  <- raw %>% all_character_cols()
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
    if(DB$redcap$is_longitudinal){
      raw$id_temp <- 1:nrow(raw)
      raw <-  merge(raw,DB$redcap$events[,c("arm_num","event_name","unique_event_name")],by.x="redcap_event_name",by.y="unique_event_name",sort = F)
      add_ons  <- add_ons[which(add_ons%in%colnames(raw))]
      cols <- c(add_ons, colnames(raw)) %>% unique()
      raw <- raw[order(raw$id_temp),cols%>% sapply(function(c){which(colnames(raw)==c)}) %>% as.integer()]
      raw$id_temp <- NULL
    }
    add_ons  <- add_ons[which(add_ons%in%colnames(raw))]
    if(any(!DB$redcap$raw_structure_cols %in% colnames(raw)))stop("raw is missing one of the following... and that's weird: ", DB$redcap$raw_structure_cols %>% paste0(collapse = ", "))
    for(instrument_name in DB$redcap$instruments$instrument_name){
      add_ons_x <- add_ons
      #instrument_name <-  DB$redcap$instruments$instrument_name %>% sample(1)
      is_repeating_instrument <- instrument_name%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
      rows  <- 1:nrow(raw)
      if(!DB$redcap$is_longitudinal){
        if("redcap_repeat_instrument"%in%colnames(raw)){
          if(is_repeating_instrument){
            rows <- which(raw$redcap_repeat_instrument==instrument_name)
          }
          if(!is_repeating_instrument){
            rows <- which(is.na(raw$redcap_repeat_instrument))
          }
        }
      }
      if(DB$redcap$is_longitudinal){
        events_ins <- DB$redcap$event_mapping$unique_event_name[which(DB$redcap$event_mapping$form==instrument_name)] %>% unique()
        rows <- which(raw$redcap_event_name%in%events_ins)
      }
      if(!is_repeating_instrument){
        add_ons_x <- add_ons_x[which(!add_ons_x%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
      }
      cols <- unique(c(add_ons_x,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(raw))]))
      DB$data_extract[[instrument_name]] <- raw[rows,cols]
    }
    if(was_merged){
      DB <- merge_non_repeating_DB()
    }
  }
  DB
}
#' @title Select REDCap records from DB
#' @inheritParams save_DB
#' @param records character vector of the IDs you want to filter the DB by
#' @return DB object that has been filtered to only include the specified records
#' @export
select_redcap_records <- function(DB, records=NULL){
  DB_selected <- DB
  if(!is.null(records)){
    if (length(records)==0)stop("Must supply records")
    DB_selected$data_extract <- list()
    BAD  <- records[which(!records%in%DB$all_records[[DB$redcap$id_col]])]
    GOOD  <- records[which(records%in%DB$all_records[[DB$redcap$id_col]])]
    if(length(BAD)>0)stop("Following records are not found in DB: ", BAD %>% paste0(collapse = ", "))
    for(FORM in names(DB$data_extract)){
      DB_selected[["data_extract"]][[FORM]]  <- DB$data_extract[[FORM]][which(DB$data_extract[[FORM]][[DB$redcap$id_col]]%in%GOOD),]
    }
    for(FORM in names(DB$data_transform)){
      DB_selected[["data_transform"]][[FORM]]  <- DB$data_transform[[FORM]][which(DB$data_transform[[FORM]][[DB$redcap$id_col]]%in%GOOD),]
    }
  }
  DB_selected
}
field_names_to_instruments <- function(DB,field_names){
  instruments <- DB$redcap$metadata$form_name[
    which(
      DB$redcap$metadata$field_name%in%field_names&
        !DB$redcap$metadata$field_name%in%DB$redcap$raw_structure_cols
    )
  ] %>% unique()
  return(instruments)

}
field_names_metadata <- function(DB,field_names){
  # if(!deparse(substitute(FORM))%in%DB$redcap$instruments$instrument_name)stop("To avoid potential issues the form name should match one of the instrument names" )
  if(any(!field_names%in%c(DB$redcap$metadata$field_name,DB$redcap$raw_structure_cols,"arm_num","event_name")))stop("All column names in your form must match items in your metadata, `DB$redcap$metadata$field_name`")
  # metadata <- DB$redcap$metadata[which(DB$redcap$metadata$form_name%in%instruments),]
  metadata <- DB$redcap$metadata[which(DB$redcap$metadata$field_name%in%field_names),]
  # metadata <- metadata[which(metadata$field_name%in%field_names),]
  return(metadata)
}
filter_metadata_from_form <- function(FORM,DB){
  instruments <- DB %>% field_names_to_instruments(field_names = colnames(FORM))
  if(any(instruments%in%DB$redcap$instruments$repeating))stop("All column names in your form must match only one form in your metadata, `DB$redcap$instruments$instrument_name`, unless they are all non-repeating")
  metadata <- DB %>% field_names_metadata(field_names = colnames(FORM))
  metadata <- metadata[which(metadata$field_type!="descriptive"),]
  metadata$has_choices <- !is.na(metadata$select_choices_or_calculations)
  return(metadata)
}
#' @title Clean to Raw REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of labelled REDCap to be converted to raw REDCap (for uploads)
#' @return DB object that has been filtered to only include the specified records
#' @export
labelled_to_raw_form <- function(FORM,DB){
  use_missing_codes <- is.data.frame(DB$redcap$missing_codes)
  metadata <- filter_metadata_from_form(FORM = FORM,DB = DB)
  for(i in 1:nrow(metadata)){ # i <-  1:nrow(metadata) %>% sample(1)
    COL_NAME <- metadata$field_name[i]
    has_choices <- metadata$has_choices[i]
    if(has_choices){
      z <- metadata$select_choices_or_calculations[i] %>% split_choices()
      FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
        OUT <- NA
        if(!is.na(C)){
          coded_redcap <- which(z$name==C)
          if(length(coded_redcap)>0){
            OUT <- z$code[coded_redcap]
          }else{
            if(use_missing_codes){
              coded_redcap2 <- which(DB$redcap$missing_codes$name==C)
              if(length(coded_redcap2)>0){
                OUT <- DB$redcap$missing_codes$code[coded_redcap2]
              }else{
                stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }else{
              stop("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
            }
          }
        }
        OUT
      }) %>% unlist() %>% as.character()
    }else{
      if(use_missing_codes){
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- C
          if(!is.na(C)){
            D <- which(DB$redcap$missing_codes$name==C)
            if(length(D)>0){
              OUT <- DB$redcap$missing_codes$code[D]
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }
    }
  }
  FORM
}
#' @title Raw to Labelled REDCap forms
#' @inheritParams save_DB
#' @param FORM data.frame of raw REDCap to be converted to labelled REDCap
#' @return DB object
#' @export
raw_to_labelled_form <- function(FORM,DB){
  if(nrow(FORM)>0){
    use_missing_codes <- is.data.frame(DB$redcap$missing_codes)
    metadata <- filter_metadata_from_form(FORM = FORM,DB = DB)
    for(i in 1:nrow(metadata)){ # i <-  1:nrow(metadata) %>% sample(1)
      COL_NAME <- metadata$field_name[i]
      has_choices <- metadata$has_choices[i]
      if(has_choices){
        z <- metadata$select_choices_or_calculations[i] %>% split_choices()
        FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
          OUT <- NA
          if(!is.na(C)){
            coded_redcap <- which(z$code==C)
            if(length(coded_redcap)>0){
              OUT <- z$name[coded_redcap]
            }else{
              if(use_missing_codes){
                coded_redcap2 <- which(DB$redcap$missing_codes$code==C)
                if(length(coded_redcap2)>0){
                  OUT <- DB$redcap$missing_codes$name[coded_redcap2]
                }else{
                  warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C,". Also not a missing code.")
                }
              }else{
                warning("Mismatch in choices compared to REDCap (above)! Column: ", COL_NAME,", Choice: ",C)
              }
            }
          }
          OUT
        }) %>% unlist() %>% as.character()
      }else{
        if(use_missing_codes){
          z <- DB$redcap$missing_codes
          FORM[[COL_NAME]] <- FORM[[COL_NAME]] %>% sapply(function(C){
            OUT <- C
            if(!is.na(C)){
              D <- which(z$code==C)
              if(length(D)>0){
                OUT <- z$name[D]
              }
            }
            OUT
          }) %>% unlist() %>% as.character()
        }
      }
    }
  }
  FORM
}
labelled_to_raw_DB <- function(DB){
  DB <- validate_DB(DB)
  if(!DB$internals$data_extract_labelled)stop("DB is already raw/coded (not labelled values)")
  for(TABLE in names(DB$data_extract)){
    DB$data_extract[[TABLE]] <- labelled_to_raw_form(FORM = DB$data_extract[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- F
  DB
}
raw_to_labelled_DB <- function(DB){
  DB <- validate_DB(DB)
  if(DB$internals$data_extract_labelled)stop("DB is already labelled (not raw coded values)")
  for(TABLE in names(DB$data_extract)){
    DB$data_extract[[TABLE]] <- raw_to_labelled_form(FORM = DB$data_extract[[TABLE]],DB=DB)
  }
  DB$internals$data_extract_labelled <- T
  DB
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
  included <- "missing_data_codes"%in%colnames(DB$redcap$project_info)
  if(included){
    is_na  <- is.na(DB$redcap$project_info$missing_data_codes)
    if(!is_na){
      return(DB$redcap$project_info$missing_data_codes %>% split_choices())
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
merge_non_repeating_DB <- function(DB){ # need to adjust for events, currently destructive
  if(DB$internals$data_extract_merged)stop("Already merged!")
  data_choice <- "data_extract"
  all_instrument_names <- DB$redcap$instruments$instrument_name
  keep_instruments <- NULL
  instrument_names <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]
  if(DB$redcap$is_longitudinal){
    instrument_names <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating&!DB$redcap$instruments$repeating_via_events)]
    keep_instruments <- all_instrument_names[which(!all_instrument_names%in% instrument_names)]
    data_choice <- "data_transform"
  }
  DB[[data_choice]][[DB$internals$merge_form_name]] <- merge_from_extact(DB,instrument_names)
  if(data_choice=="data_extract") {
    for(instrument_name in instrument_names){
      DB[["data_extract"]][[instrument_name]] <- NULL
    }
    DB$internals$data_extract_merged <- T
  }else{
    for(keep in keep_instruments){
      DB[["data_transform"]][[keep]] <- DB[["data_extract"]][[keep]]
    }
  }
  DB
}
merge_from_extact <- function(DB,instrument_names){
  instrument_names <- instrument_names %>% as.list()
  if (length(instrument_names)==1) warning('No need to merge you only have one form that is non-repeating')
  merged <- DB[["data_extract"]][[instrument_names[[1]]]]
  merged$redcap_event_name <- NULL
  # merged$arm_num <- NULL
  merged$event_name <- NULL
  merged$redcap_repeat_instrument <- NULL
  merged$redcap_repeat_instance <- NULL
  instrument_names[[1]] <- NULL
  while (length(instrument_names)>0) {
    dfx <- DB[["data_extract"]][[instrument_names[[1]]]]
    dfx$redcap_event_name <- NULL
    # dfx$arm_num <- NULL
    dfx$event_name <- NULL
    dfx$redcap_repeat_instrument <- NULL
    dfx$redcap_repeat_instance <- NULL
    (in_common <- colnames(merged)[which(colnames(merged)%in%colnames(dfx))])
    merged <- merge(merged,dfx,by=in_common,all = T)
    instrument_names[[1]] <- NULL
  }
  merged
}
#' @title Unmerge non-repeating, not ready for multi-event projects
#' @inheritParams save_DB
#' @return DB object that has merged all non repeating forms
#' @export
unmerge_non_repeating_DB <- function(DB){
  if(!DB$internals$data_extract_merged)stop("No DB$data_extract named as 'merged'!")
  instrument_names <- DB$data_extract[[DB$internals$merge_form_name]] %>% colnames() %>% sapply(function(COL){DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name==COL)]}) %>% unique() %>% as.list()
  merged <- DB$data_extract[[DB$internals$merge_form_name]]
  while (length(instrument_names)>0) {
    instrument_name  <- instrument_names[[1]]
    DB$data_extract[[instrument_name]] <- merged[,unique(c(DB$redcap$id_col,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(merged))]))]
    instrument_names[[1]] <- NULL
  }
  DB$data_extract[[DB$internals$merge_form_name]] <- NULL
  DB$internals$data_extract_merged <- F
  DB
}
#' @title Deidentify the REDCap DB according to REDCap or your choices
#' @inheritParams save_DB
#' @param identifiers optional character vector of column names that should be excluded from DB. Otherwise `DB$redcap$metadata$identifier =="y` will be used.
#' @return DB object that has deidentified forms
#' @export
deidentify_DB <- function(DB,identifiers){
  DB <- validate_DB(DB)
  missing_identifiers <- missing(identifiers)
  if(!missing_identifiers){
    identifiers <- identifiers %>% unique()
    bad_identifiers <- identifiers[which(!identifiers%in%DB$redcap$metadata$field_name)]
    if(length(bad_identifiers)>0)stop("You have an identifier that is not included in the set of `DB$redcap$metadata$field_name` --> ",bad_identifiers %>% paste0(collapse = ", "))
    if(DB$redcap$id_col%in%identifiers)stop("Your REDCap ID, ",DB$redcap$id_col,", should not be deidentified.") #If you want to pass a new set of random IDs to make this data use `scramble_ID_DB(DB)`.")
  }
  if(missing_identifiers){
    identifiers <-  DB$redcap$metadata$field_name[which(DB$redcap$metadata$identifier=="y")]
    if(length(identifiers)==0)warning("You have no identifiers marked in `DB$redcap$metadata$identifier`. You can set it in REDCap Project Setup and update DB OR define your idenitifiers in this functions `identifiers` argument." ,immediate. = T)
  }
  drop_list <- Map(function(NAME, COLS) {identifiers[which(identifiers %in% COLS)]},names(DB$data_extract), lapply(DB$data_extract, colnames))
  drop_list <- drop_list[sapply(drop_list, length) > 0]
  if(length(drop_list)==0)message("Nothing to deidentify from --> ",identifiers %>% paste0(collapse = ", "))
  for (FORM in names(drop_list)) {
    for(DROP in drop_list[[FORM]]){
      DB$data_extract[[FORM]][[DROP]] <- NULL
      message("Dropped ",DROP," from ", FORM)
    }
  }
  DB
}
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
  return(metadata)
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
#' @title add REDCap ID to any dataframe using a ref_id
#' @description
#'  add REDCap ID to any dataframe using a ref_id
#' @param DF dataframe
#' @inheritParams save_DB
#' @param ref_id column name that matches a REDCap variable name that could be an ALT id such as MRN
#' @return original dataframe with REDCap id_col added as the first column
#' @export
add_ID_to_DF <- function(DF,DB,ref_id){
  if(!ref_id%in%DB$redcap$metadata$field_name)stop("The ref_id not valid. Must be a REDCap raw colname")
  form <- DB$redcap$metadata$form_name[which(DB$redcap$metadata$field_name==ref_id)]
  if(DB$internals$data_extract_merged){
    if(form %in% DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]){
      form <- DB$internals$merge_form_name
    }
  }
  id_col <- DF[[ref_id]] %>% sapply(function(ID){
    DB$data_extract[[form]][[DB$redcap$id_col]][which(DB$data_extract[[form]][[ref_id]]==ID)]
  }) %>% as.data.frame()
  colnames(id_col) <- DB$redcap$id_col
  DF <- cbind(id_col,DF)
  DF
}
#' @title grab data table for an individual(s)
#' @description
#' grab data table for an individual(s)
#' @inheritParams select_redcap_records
#' @return list of data tables
#' @export
grab_record_tables <- function(DB, records){
  OUT  <- list()
  for(TABLE in names(DB$data_extract)){
    OUT[[TABLE]] <-   DB$data_extract[[TABLE]][which(DB$data_extract[[TABLE]][[DB$redcap$id_col]]%in%records),]
  }
  OUT
}
split_choices <- function(x){
  oops <- x
  x <- gsub("\n", " | ",x)  #added this to account for redcap metadata output if not a number
  x <- x %>% strsplit(" [:|:] ") %>% unlist()
  check_length <- length(x)
  # code <- x %>% stringr::str_extract("^[^,]+(?=, )")
  # name <- x %>% stringr::str_extract("(?<=, ).*$")
  result <- x %>% stringr::str_match("([^,]+), (.*)")
  # x <- data.frame(
  #   code=x %>% strsplit(", ") %>% sapply(`[`, 1),
  #   name=x %>% strsplit(", ")%>% sapply(`[`, -1) %>% sapply(function(y){paste0(y,collapse = ", ")})
  # )
  x <- data.frame(
    code=result[,2],
    name=result[,3]
  )
  rownames(x) <- NULL
  if(nrow(x)!=check_length)stop("split choice error: ",oops)
  x
}
make_codebook <- function(DB){
  choices <- which(DB$redcap$metadata$field_type%in%c("radio","dropdown","checkbox_choice","yesno"))
  if(length(choices)>0){
    for(field in DB$redcap$metadata$field_name[choices]){
      DB[["choices"]][[field]] <- DB$redcap$metadata$select_choices_or_calculations[which(DB$redcap$metadata$field_name==field)] %>% split_choices()
    }
  }
  OUT <- NULL
  for (CHOICE in names(DB[["choices"]])){
    x <- DB[["choices"]][[CHOICE]]
    x$field_name <- CHOICE
    OUT <- OUT %>% dplyr::bind_rows(x)
  }
  OUT <- OUT %>% dplyr::select("field_name","code","name")
  rownames(OUT) <- NULL
  OUT
}
