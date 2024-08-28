create_analysis_t <-function (design, loa = NULL, group_var = NULL, sm_separator = ".") 
{
  if (!"tbl_svy" %in% attributes(design)$class) {
    stop("It seems object design is not a design, did you use srvyr::as_survey ?")
  }
  if (!is.null(loa) & !is.null(group_var)) {
    warning("You have provided a list of analysis and group variable, group variable will be ignored")
  }
  paste0("tom0")
  
  if (!is.null(loa)) {
    loa <- check_loa(loa, design)
  }
  if (is.null(loa)) {
    loa <- create_loa(design = design, group_var = group_var, 
                      sm_separator = sm_separator)
  }
  
  paste0("tom1")
  results_list <- loa %>% split(1:nrow(.)) %>% purrr::map(function(loa) {
    paste0("tom2")
    
    if (loa[["analysis_type"]] == "mean") {
      return(create_analysis_mean(design, group_var = loa[["group_var"]], 
                                  analysis_var = loa[["analysis_var"]], level = loa[["level"]]))
    }
    if (loa[["analysis_type"]] == "median") {
      return(create_analysis_median(design, group_var = loa[["group_var"]], 
                                    analysis_var = loa[["analysis_var"]], level = loa[["level"]]))
    }
    if (loa[["analysis_type"]] == "prop_select_one") {
      return(create_analysis_prop_select_one(design, group_var = loa[["group_var"]], 
                                             analysis_var = loa[["analysis_var"]], level = loa[["level"]]))
    }
    if (loa[["analysis_type"]] == "prop_select_multiple") {
      return(create_analysis_prop_select_multiple(design, 
                                                  group_var = loa[["group_var"]], analysis_var = loa[["analysis_var"]], 
                                                  level = loa[["level"]], sm_separator = sm_separator))
    }
    if (loa[["analysis_type"]] == "ratio") {
      return(create_analysis_ratio(design, group_var = loa[["group_var"]], 
                                   analysis_var_numerator = loa[["analysis_var_numerator"]], 
                                   analysis_var_denominator = loa[["analysis_var_denominator"]], 
                                   numerator_NA_to_0 = loa[["numerator_NA_to_0"]], 
                                   filter_denominator_0 = loa[["filter_denominator_0"]], 
                                   level = loa[["level"]]))
    }
    paste0("tom3")
    
  }, .progress = TRUE)
  results_table <- results_list %>% do.call(rbind, .)
  return(list(results_table = results_table, dataset = design$variables, 
              loa = loa))
}
