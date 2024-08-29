create_analysis_prop_select_multiple_t <- function (design, group_var = NA, analysis_var, level = 0.95, 
                                                    sm_separator = "/") 
{
  if (is.na(group_var)) {
    across_by <- c()
  }
  else {
    across_by <- group_var %>% char_to_vector()
  }
  sm_var <- analysis_var
  sm_var_choice_start <- paste0(analysis_var, sm_separator)
  predesign <- design %>% dplyr::mutate(dplyr::across(dplyr::starts_with(sm_var_choice_start), 
                                                      as.numeric)) %>% dplyr::group_by(dplyr::across(dplyr::any_of(across_by)))
  results <- predesign %>% dplyr::filter(!is.na(!!rlang::sym(analysis_var)), 
                                         .preserve = T) %>% srvyr::summarise(dplyr::across(dplyr::starts_with(sm_var_choice_start), 
                                                                                           .fns = list(stat = ~srvyr::survey_mean(.x, vartype = "ci", 
                                                                                                                                  na.rm = TRUE, level = level), n_w = ~srvyr::survey_total(.x, 
                                                                                                                                                                                           vartype = "ci", na.rm = TRUE, level = level), n = ~sum(.x, 
                                                                                                                                                                                                                                                  na.rm = TRUE), n_w_total = ~srvyr::survey_total(!is.na(.x), 
                                                                                                                                                                                                                                                                                                  vartype = "ci", na.rm = TRUE), n_total = ~sum(!is.na(.x), 
                                                                                                                                                                                                                                                                                                                                                na.rm = TRUE)), .names = "{.fn}...{.col}"))
  n...sm_var_NA <- paste0("n...", sm_var_choice_start, "NA")
  NA_counts <- predesign %>% dplyr::filter(is.na(!!rlang::sym(analysis_var)), 
                                           .preserve = T) %>% dplyr::summarise(`:=`(!!rlang::sym(n...sm_var_NA), 
                                                                                  dplyr::n())) %>% dplyr::ungroup() %>% dplyr::select(tidyr::contains("n..."))
  paste0(results)  
   results <- results %>% cbind(NA_counts)
  results <- results %>% dplyr::mutate(analysis_var = sm_var) %>% 
    tidyr::pivot_longer(-c(analysis_var, dplyr::all_of(across_by)), 
                        names_to = "analysis_var_value", values_to = "stat") %>% 
    tidyr::separate_wider_delim(analysis_var_value, delim = "...", 
                                names = c("type", "analysis_var_value")) %>% dplyr::mutate(analysis_var_value = gsub(sm_var_choice_start, 
                                                                                                                     "", analysis_var_value), type = dplyr::case_when(stringr::str_detect(analysis_var_value, 
                                                                                                                                                                                          "_low$") ~ paste0(type, "_low"), stringr::str_detect(analysis_var_value, 
                                                                                                                                                                                                                                               "_upp$") ~ paste0(type, "_upp"), TRUE ~ type), analysis_var_value = stringr::str_replace(analysis_var_value, 
                                                                                                                                                                                                                                                                                                                                        "_low$|_upp$", ""), group_var = create_group_var(group_var), 
                                                                                           analysis_type = "prop_select_multiple") %>% dplyr::filter(type %in% 
                                                                                                                                                       c("stat", "stat_low", "stat_upp", "n_w", "n", "n_w_total", 
                                                                                                                                                         "n_total", "na_count"))
  
  paste0(typeof(results))
  results <- results %>% tidyr::pivot_wider(id_cols = c(dplyr::all_of(across_by), 
                            group_var, analysis_var, analysis_var_value, analysis_type), 
                names_from = type, values_from = stat) %>% dplyr::filter(!(analysis_var_value == 
                                                                             "NA" & n == 0)) %>% correct_nan_analysis_var_variable_is_na() %>% 
    correct_nan_total_is_0()
  results <- adding_group_var_value(results = results, group_var = group_var, 
                                    grouping_vector = across_by)
  results <- adding_analysis_key(results = results)
  results %>% arranging_results_columns()
}
