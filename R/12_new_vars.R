#' @import rosyutils

#' @title add_new_varriable
#' @export
add_new_varriable <- function(
    DB,
    field_name,
    form_name,
    field_type,
    field_type_R = NA,
    field_label = NA,
    select_choices_or_calculations = NA,
    field_note = NA,
    identifier = "",
    units = NA,
    insert_after,
    DF = NA
) {
  DB <-validate_DB(DB)
  metadata_new_var <- data.frame(
    field_name = field_name,
    form_name = form_name,
    field_type = field_type,
    field_label = field_label,
    select_choices_or_calculations = select_choices_or_calculations,
    field_note = field_note,
    identifier = identifier,
    field_type_R = field_type_R,
    units = units,
    in_original_redcap = F,
    field_label_short = field_label
  )
  # metadata <- DB$
  DB$remap$metadata_new <-  DB$remap$metadata_new[which(DB$remap$metadata_new$field_name!=field_name),]
  if(!missing(insert_after)){
    i<-which(DB$remap$metadata_new$field_name == insert_after)
    if(length(i)==1){
      top <- DB$remap$metadata_new[1:i,]
      bottom <- DB$remap$metadata_new[(i+1):nrow(DB$remap$metadata_new),]
      DB$remap$metadata_new <- dplyr::bind_rows(top,metadata_new_var) %>% dplyr::bind_rows(bottom)
    }else{
      stop("insert_after error")
    }
  }else{
    DB$remap$metadata_new <-  DB$remap$metadata_new %>% dplyr::bind_rows(metadata_new_var)
  }
  # original <- DB$data_transform[[form_name]]
  # original_struc <- colnames(original)[which(colnames(original)%in%DB$redcap$raw_structure_cols)]
  # if(any(!original_struc%in% colnames(DF)))stop("all of structural columns for the ",form_name," form must be in DF")
  # original <- merge(original,DF,by = original_struc,all.x = T)
  # final_cols <-c(original_struc,DB$remap$metadata_new$field_name[which(DB$remap$metadata_new$form_name==form_name)]) %>% unique()
  # final_cols[which(!final_cols%in%colnames(original))]
  return(DB)
}
