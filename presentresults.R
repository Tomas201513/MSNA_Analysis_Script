install.packages("writexl")
install.packages("readxl")
install.packages("stringr")
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)
library(presentresults)


example_variable_x_group <- presentresults_resultstable %>%
  create_table_variable_x_group(analysis_key = "analysis_key")
View(presentresults_resultstable)

example_variable_x_group[1:6, 1:9]


example_variable_x_group %>%
  create_xlsx_variable_x_group(file_path = "mytable.xlsx")

View(example_variable_x_group)



example_variable_x_group <- presentresults_resultstable %>%
  create_table_variable_x_group(value_columns = "stat")

example_variable_x_group[1:6, 1:9]
View(example_variable_x_group)


presentresults_resultstable %>%
  create_table_variable_x_group() %>%
  create_xlsx_variable_x_group(
    file_path = "mytablee.xlsx",
    value_columns = "stat"
  )


example_group_x_variable <- create_table_group_x_variable(presentresults_resultstable, value_columns = "stat")

example_group_x_variable[1:6, 1:10]

presentresults_resultstable %>%
  create_table_group_x_variable() %>%
  create_xlsx_group_x_variable(file_path = "mytableee.xlsx")



#You can add labels to the results table.

label_results <- add_label_columns_to_results_table(
  results_table = presentresults_MSNA2024_results_table,
  dictionary = presentresults_MSNA2024_dictionary
)

# Work in progress, but the idea will be to export it after.

label_results <- label_results %>%
  dplyr::filter(group_var != "hoh_gender")
example_variable_x_group <- label_results %>%
  create_table_group_x_variable(analysis_key = "label_analysis_key", value_columns = "stat")


#Example for the IPC table


