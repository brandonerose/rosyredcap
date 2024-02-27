add_new_varriable <- function(
    DB,
    field_name,
    form_name,
    field_type,
    field_label = NA,
    select_choices_or_calculations = NA,
    field_note = NA,
    identifier = "n",
    units = NA,
    insert_after,
    DF = NA
) {
  DB <-validate_DB()
  metadata_new_var <- data.frame(
    field_name = field_name,
    form_name = form_name,
    field_type = field_type,
    field_label = field_label,
    select_choices_or_calculations = select_choices_or_calculations,
    field_note = field_note,
    identifier = identifier,
    units = units
  )
  # metadata <- DB$
  # if()
  metadata_new_var %>% metadata_to_codebook()
}


