add_new_varriable <- function(field_name,form_name,field_type,field_label = NA,select_choices_or_calculations = NA,field_note = NA, identifier = "n", insert_after){
  data.frame(
    field_name = field_name,
    form_name = form_name,
    field_type = field_type,
    field_label = field_label,
    select_choices_or_calculations = select_choices_or_calculations,
    field_note = field_note,
    identifier = identifier
  )
}


