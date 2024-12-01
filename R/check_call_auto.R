# Automatically generated module API check functions. Think twice before editing them manually.
({
# styler: off

# dv.clinlines::mod_clinical_timelines
check_mod_clinical_timelines_auto <- function(afmm, datasets, module_id, basic_info, mapping, drug_admin,
    filter, subjid_var, default_plot_settings, ms, receiver_id, warn, err) {
    OK <- logical(0)
    used_dataset_names <- new.env(parent = emptyenv())
    OK[["module_id"]] <- CM$check_module_id("module_id", module_id, warn, err)
    "TODO: basic_info (group)"
    "TODO: mapping (group)"
    "TODO: drug_admin (group)"
    "TODO: filter (group)"
    "TODO: subjid_var (character)"
    "TODO: default_plot_settings (group)"
    "TODO: ms (group)"
    "TODO: receiver_id (character)"
    return(OK)
}

})
# styler: on
