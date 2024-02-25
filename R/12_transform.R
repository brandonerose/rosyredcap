remap_process <- function(DB){
  DB <- validate_DB(DB)
  if(is.data.frame(DB$redcap$metadata)){
    metadata_remap <-   DB$remap$metadata_remap
    instruments_remap <- DB$redcap$instruments
    instruments_remap$instrument_name_remap <- instruments_remap$instrument_name %>% sapply(function(instrument_name){
      x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
      if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
      return(x)
    })
    if(DB$redcap$is_longitudinal){
      if(DB$redcap$has_arms_that_matter){
        #can add remapping of arms but not smart if they matter
      }else{
        # DB$redcap$events->x
        event_mapping_remap <- DB$remap$event_mapping_remap
        events_remap <- DB$redcap$events
        events_remap$unique_event_name_remap <- events_remap$unique_event_name %>% sapply(function(unique_event_name){
          x<-event_mapping_remap$unique_event_name_remap[which(event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()
          if(length(x)>1)stop("unique_event_names cannot match multiple unique_event_name_remaps: ",unique_event_name)
          return(x)
        })
        events_new <- events_remap[,c("unique_event_name_remap","event_name")] %>% unique()
        colnames(events_new)[1] <- "unique_event_name"
        event_mapping_new <- event_mapping_remap[,c("unique_event_name_remap","event_name","form")] %>% unique()
        event_mapping_new$form <- event_mapping_new$form %>% sapply(function(instrument_name){
          x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
          if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
          return(x)
        })
        event_mapping_new <- event_mapping_new %>% unique()
        colnames(event_mapping_new)[1] <- "unique_event_name"
        events_new$former_unique_event_names <- events_new$unique_event_name %>% sapply(function(unique_event_name){
          events_remap$unique_event_name[which(events_remap$unique_event_name_remap==unique_event_name)] %>% unique() %>% paste0(collapse = " | ")
        })
        event_mapping_new$former_unique_event_names <- event_mapping_new$unique_event_name %>% sapply(function(unique_event_name){
          event_mapping_remap$unique_event_name[which(event_mapping_remap$unique_event_name_remap==unique_event_name)] %>% unique() %>% paste0(collapse = " | ")
        })
        x<- event_mapping_new[which(event_mapping_new$form%in%instruments_remap$instrument_name_remap),]
        instruments_remap$repeating_via_events[
          which(
            instruments_remap$instrument_name_remap %>% sapply(function(instrument_name_remap){
              # instrument_name <- DB$redcap$instruments$instrument_name %>% sample(1)
              if(DB$internals$merge_form_name==instrument_name_remap){
                F
              }else{
                anyDuplicated(event_mapping_new$form[which(event_mapping_new$form==instrument_name_remap)])>0
              }
            })
          )
        ] <- T
      }
      DB$remap$events_new <- events_new
      DB$remap$event_mapping_new <- event_mapping_new
    }
    non_reps <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)]
    ins_new_cols <- c("instrument_name_remap","repeating")
    if(DB$redcap$is_longitudinal){
      non_reps <- DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating&!DB$redcap$instruments$repeating_via_events)]
      ins_new_cols <- c("instrument_name_remap","repeating","repeating_via_events")
    }
    if(length(non_reps)>0){
      metadata_remap$form_name_remap[which( metadata_remap$form_name%in%non_reps)] <- DB$internals$merge_form_name
    }
    instruments_new <- instruments_remap[,ins_new_cols] %>% unique()
    instruments_new$former_instrument_names <- instruments_new$instrument_name_remap %>% sapply(function(instrument_name_remap){
      instruments_remap$instrument_name[which(instruments_remap$instrument_name_remap==instrument_name_remap)] %>% unique() %>% paste0(collapse = " | ")
    })
    colnames(instruments_new)[1] <- "instrument_name"
    metadata_new <- metadata_remap
    metadata_new$field_name <- metadata_new$field_name_remap
    metadata_new$field_name_remap <- NULL
    metadata_new$form_name <- metadata_new$form_name_remap
    metadata_new$form_name_remap <- NULL
    keep_cols <- c(
      "field_name",
      "form_name",
      "field_type",
      "select_choices_or_calculations" # could handle choice remaps if labelled
    )
    drop_cols <- colnames(metadata_new)[which(!colnames(metadata_new)%in%keep_cols)]
    metadata_new <- metadata_new[,keep_cols] %>% unique()
    if(metadata_new$field_name %>% anyDuplicated() %>% magrittr::is_greater_than(0))stop("metadata_new has duplicate field names")

    for (col in drop_cols){
      metadata_new[[col]] <- metadata_new$field_name %>% sapply(function(field_name){
        metadata_remap[[col]][which(metadata_remap$field_name==field_name)[1]]
      })
    }
    DB$remap$metadata_new <- metadata_new %>% annotate_metadata()
    DB$remap$instruments_new <- instruments_new
    DB$remap$instruments_remap <- instruments_remap
    # if(save_file) metadata_new %>% rio::export(file = DB$dir_path %>% file.path("input","metadata_new_default.xlsx"))
  }
  return(DB)
}
generate_default_remap <- function(DB,save_file=!is.null(DB$dir_path),merge_non_repeating = T){
  DB <- validate_DB(DB)
  if(is.data.frame(DB$redcap$metadata)){
    metadata_remap <-   DB$redcap$metadata
    metadata_remap$field_name_remap <- metadata_remap$field_name
    metadata_remap$form_name_remap <- metadata_remap$form_name
    if(merge_non_repeating){
      metadata_remap$form_name_remap[which(metadata_remap$form_name%in%DB$redcap$instruments$instrument_name[which(!DB$redcap$instruments$repeating)])] <- DB$internals$merge_form_name
    }
    if(DB$redcap$is_longitudinal){
      if(DB$redcap$has_arms_that_matter){
        #can add remapping of arms but not smart if they matter
      }else{
        # DB$redcap$events->x
        event_mapping_remap <- DB$redcap$event_mapping
        event_mapping_remap <- merge(event_mapping_remap, DB$redcap$events[,c("event_name","unique_event_name")], by="unique_event_name")
        event_mapping_remap$unique_event_name_remap <- tolower(gsub(" ","_",event_mapping_remap$event_name))
        event_mapping_remap$form_remap <-  event_mapping_remap$form %>% sapply(function(instrument_name){
          x<-metadata_remap$form_name_remap[which(metadata_remap$form_name==instrument_name)] %>% unique()
          if(length(x)>1)stop("instrument_names cannot match multiple instrument_name_remaps: ",instrument_name)
          return(x)
        })
      }
      if(save_file) event_mapping_remap %>% rio::export(file = DB$dir_path %>% file.path("input","event_mapping_remap_default.xlsx"))
      DB$remap$event_mapping_remap <- event_mapping_remap
    }
    if(save_file) metadata_remap %>% rio::export(file = DB$dir_path %>% file.path("input","metadata_remap_default.xlsx"))
    DB$remap$metadata_remap <- metadata_remap
  }
  DB <- remap_process(DB)
  return(DB)
}
#' @title Generate custom remap files from input
#' @inheritParams save_DB
#' @return DB object that has DB$remap populated from input folder
#' @export
generate_custom_remap_from_dir <- function(DB){
  DB <- validate_DB(DB)
  input_folder <- DB$dir_path %>% file.path("input")
  # input_folder %>% file.path(c("metadata_remap.xlsx","event_mapping_remap.xlsx"))
  # input_folder %>% list.files(full.names = T)
  for(file in c("metadata_remap","event_mapping_remap")){
    path <- input_folder %>% file.path(paste0(file,".xlsx"))
    if(file.exists(path)){
      DB$remap[[file]] <- rio::import(file = path)
    }
  }
  DB <- remap_process(DB)
  return(DB)
}
#' @title Transform DB
#' @inheritParams save_DB
#' @return DB object that has DB$data_transform, can be based on a remap file from input folder or default
#' @export
transform_DB <- function(DB, merge_non_rep_to_reps = F, records=NULL){
  DB  <- validate_DB(DB)
  selected <- DB %>% select_redcap_records(records = records,data_choice = "data_extract")
  if(!DB$remap %>% is_something()){
    DB <- generate_default_remap(DB)
  }
  instrument_names <- DB$remap$instruments_new$instrument_name
  for (instrument_name in instrument_names) {# instrument_name <- instrument_names %>%  sample (1)
    if(instrument_name == DB$internals$merge_form_name){
      old_instruments <- DB$remap$instruments_remap$instrument_name[which(DB$remap$instruments_remap$instrument_name_remap == instrument_name)]
      DB$data_transform[[instrument_name]] <- merge_from_extact(DB, old_instruments)
    }else{
      old_instruments <- DB$remap$instruments_remap$instrument_name[which(DB$remap$instruments_remap$instrument_name_remap == instrument_name)]
      final_out <- NULL
      for(old_instrument in old_instruments){# old_instrument <- old_instruments %>%  sample (1)
        keep <- selected[[old_instrument]]
        colnames(keep) <- colnames(keep) %>% sapply(function(col){
          out <- col
          x<-DB$remap$metadata_remap$field_name_remap[which(DB$remap$metadata_remap$field_name == col)]
          if(length(x)>0)out <- x
          out
        })
        if("redcap_event_name"%in%colnames(keep)){
          keep$redcap_event_name <- keep$redcap_event_name %>% sapply(function(unique_event_name){DB$remap$event_mapping_remap$unique_event_name_remap[which(DB$remap$event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()})
          # keep$event_name <- keep$event_name %>% sapply(function(event_name){DB$remap$event_mapping_remap$[which(DB$remap$event_mapping_remap$unique_event_name==unique_event_name)] %>% unique()})
        }
        final_out <- final_out %>% dplyr::bind_rows(keep)
      }
      DB$data_transform[[instrument_name]] <- final_out
    }
  }
  if(merge_non_rep_to_reps){

  }
  return(DB)
}
summarize_DB <- function(DB){
  #project --------
  summary <- list()
  was_remapped <-DB$remap %>% is_something()
  pull_from <- "redcap"
  df_names1 <- c("metadata","instruments","event_mapping","events")
  if(was_remapped){
    pull_from <- "remap"
    df_names2 <- paste0(df_names1,"_new")
  }
  for(i in 1:length(df_names1)){
    summary[[df_names1[[i]]]] <- DB[[pull_from]][[df_names2[[i]]]]
  }
  DB$redcap$project_info
  metadata <- DB$redcap$metadata
  instruments <- DB$redcap$instruments
  arms <- DB$redcap$arms
  users <- DB$redcap$instruments
  events <- DB$redcap$events
  event_mapping <- DB$redcap$event_mapping
  list(
    short_name=NULL,
    token_name=NULL,
    dir_path=NULL,
    internals = list(
      last_metadata_check=NULL,
      last_metadata_update=NULL,
      last_metadata_dir_save=NULL,
      last_data_check=NULL,
      last_data_update=NULL,
      last_data_dir_save = NULL,
      last_data_transformation = NULL,
      last_directory_save=NULL,
      data_extract_labelled = NULL,
      data_extract_merged = NULL,
      merge_form_name = "merged",
      data_extract_clean = NULL
    ),
    redcap = list(
      project_id=NULL,
      project_title= NULL,
      id_col=NULL,
      version=NULL,
      project_info=NULL,
      metadata=NULL,
      instruments=NULL,
      arms=NULL,
      events=NULL,
      event_mapping = NULL,
      missing_codes=NULL,
      log=NULL,
      users=NULL,
      current_user=NULL,
      codebook=NULL,
      choices=NULL,
      raw_structure_cols = NULL,
      is_longitudinal = NULL,
      has_arms = NULL,
      has_multiple_arms = NULL,
      has_arms_that_matter = NULL,
      has_repeating_instruments_or_events = NULL,
      has_repeating_instruments = NULL,
      has_repeating_events = NULL
    ),
    auto_jobs = NULL,
    remap = list(
      metadata_remap=NULL,
      metadata_new=NULL,
      instruments_remap=NULL,
      instruments_new=NULL,
      arms_map=NULL,
      arms_new=NULL,
      events_remap=NULL,
      events_new=NULL,
      event_mapping_remap=NULL,
      event_mapping_new=NULL
    ),
    data_extract = NULL,
    data_transform = NULL,
    data_upload = NULL,
    summary = NULL,
    links = list(
      redcap_base = NULL,
      redcap_uri = NULL,
      redcap_home = NULL,
      redcap_records = NULL,
      redcap_API = NULL,
      redcap_API_playground = NULL,
      github = "https://github.com/brandonerose/rosyredcap",
      thecodingdocs = "https://www.thecodingdocs.com/"
    )
  )
  if(was_remapped){
  }
  #records belong to arms 1 to 1 ----------
  summary$records_n <- 0
  if(!is.null(DB$summary$all_records)){
    summary$records_n <- DB$summary$all_records %>% length()
  }
  #arms----------------
  summary$arms_n <- 0
  if(is.data.frame(DB$redcap$arms)){
    summary$arms_n <- DB$redcap$arms %>% nrow()
    id_pairs <- DB$redcap$instruments$instrument_name %>%  lapply(function(IN){DB$data_extract[[IN]][,c(DB$redcap$id_col,"arm_num")]}) %>% dplyr::bind_rows() %>% unique()
    DB$redcap$arms$arm_records_n <- DB$redcap$arms$arm_num %>% sapply(function(arm){
      which(id_pairs$arm_num==arm)%>% length()
    })
  }
  #events belong to arms many to 1 ----------------
  # summary$events_n <- DB$redcap$events %>% nrow()
  summary$events_n <- 0
  if(is.data.frame(DB$redcap$events)){
    summary$events_n <- DB$redcap$events %>% nrow()
    summary$event_names_n <- DB$redcap$events$event_name %>% unique() %>% length()
    # 1:nrow(DB$redcap$event_mapping) %>% lapply(function(i){
    #   (DB$data_extract[[DB$redcap$event_mapping$form[i]]][['redcap_event_name']]==DB$redcap$event_mapping$unique_event_name[i]) %>% which() %>% length()
    # })
    # for(event in ){
    #   summary[[paste0(event,"_records_n")]] <- DB$data_extract[[]][which(DB$redcap$arms$arm_num==arm)]
    # }
  }
  #instruments/forms belong to events many to 1 (if no events/arms) ----------------
  summary$instruments_n <- 0
  if(is.data.frame(DB$redcap$instruments)){ # can add expected later
    summary$instruments_n <- DB$redcap$instruments %>% nrow()
    DB$redcap$instruments$incomplete <- DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Incomplete") %>% which() %>% length()
    })
    DB$redcap$instruments$unverified <- DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Unverified") %>% which() %>% length()
    })
    DB$redcap$instruments$complete <- DB$redcap$instruments$instrument_name %>% sapply(function(instrument_name){
      (DB$data_extract[[instrument_name]][[paste0(instrument_name,"_complete")]]=="Complete") %>% which() %>% length()
    })
  }
  #fields belong to instruments/forms 1 to 1 ----------------
  summary$metadata_n <- 0
  summary$metadata_n <- DB$redcap$metadata[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive")),] %>% nrow()
  # DB$redcap$metadata$field_type[which(!DB$redcap$metadata$field_type%in%c("checkbox_choice","descriptive"))] %>% table()
  #metadata/codebook =============
  metadata <- DB$redcap$metadata
  codebook <- DB$redcap$codebook %>% dplyr::select("field_name", "code", "name")
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
  for(form in names(DB$data_extract)){
    DF <- DB$data_extract[[form]]
    for(col in colnames(DF)){
      if (!exact){
        DF[[col]] <- tolower(DF[[col]])
      }
      rows <- which(grepl(text,DF[[col]]))
      if(length(rows)>0){
        out <- out %>%dplyr::bind_rows(
          data.frame(
            record_id = DF[[DB$redcap$id_col]][rows],
            col = col,
            row = as.character(rows)
          )
        )
      }
    }
  }
  return(out)
}
extract_instrument_from_merged <- function(DB,instrument_name){
  merged <- DB$data_extract[[DB$internals$merge_form_name]]
  if(nrow(merged)>0){
    add_ons <- c(DB$redcap$id_col,"arm_num","event_name","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")
    add_ons  <- add_ons[which(add_ons%in%colnames(merged))]
    if(!instrument_name%in%DB$redcap$instruments$instrument_name)stop("instrument_name must be included in set of DB$redcap$instruments$instrument_name")
    #instrument_name <-  DB$redcap$instruments$instrument_name %>% sample(1)
    is_repeating_instrument <- instrument_name%in%DB$redcap$instruments$instrument_name[which(DB$redcap$instruments$repeating)]
    rows  <- 1:nrow(merged)
    if(is_repeating_instrument){
      # rows <- which(merged$redcap_repeat_instrument==instrument_name)
    }
    if(!is_repeating_instrument){
      rows <- which(!is.na(merged[[paste0(instrument_name,"_complete")]]))
    }
    #
    # if(!DB$redcap$is_longitudinal){
    #   if("redcap_repeat_instrument"%in%colnames(merged)){
    #     if(is_repeating_instrument){
    #       rows <- which(merged$redcap_repeat_instrument==instrument_name)
    #     }
    #     if(!is_repeating_instrument){
    #       rows <- which(is.na(merged$redcap_repeat_instrument))
    #     }
    #   }
    # }
    # if(DB$redcap$is_longitudinal){
    #   events_ins <- DB$redcap$event_mapping$unique_event_name[which(DB$redcap$event_mapping$form==instrument_name)] %>% unique()
    #   rows <- which(merged$redcap_event_name%in%events_ins)
    # }
    # if(!is_repeating_instrument){
    #   add_ons <- add_ons[which(!add_ons%in%c("redcap_repeat_instrument","redcap_repeat_instance"))]
    # }
    cols <- unique(c(add_ons,DB$redcap$metadata$field_name[which(DB$redcap$metadata$form_name==instrument_name&DB$redcap$metadata$field_name%in%colnames(merged))]))
    return(merged[rows,cols])
  }
}
