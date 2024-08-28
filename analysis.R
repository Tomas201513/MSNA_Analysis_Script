install.packages("writexl")
install.packages("readxl")
install.packages("stringr")
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)
library(analysistools)

########################################################################################
file_path="C:/Users/test/Downloads/20240826_ETH2403_eth_msna_cleaned_data_NEW.xlsx"
print(file_path)
sheet_names <- excel_sheets(file_path)
dataframes1 <- list()

for (sheet in sheet_names) {
  dataframes1[[sheet]] <- read_excel(file_path, sheet = sheet)}

print(sheet_names)

df_main <- dataframes1[[2]]
View(df_main)

View(df_main)

shorter_df <- analysistools_MSNA_template_data[, c(
  "admin1",
  "admin2",
  "expenditure_debt",
  "income_v1_salaried_work",
  "wash_drinkingwatersource", 
  grep("edu_learning_conditions_reasons_v1", names(analysistools_MSNA_template_data), value = T)
)]


########################################################################################

#View(analysistools_MSNA_template_data)
#write_xlsx(analysistools_MSNA_template_data, "analysis_default_data.xlsx")


shorter_df <- analysistools_MSNA_template_data[, c(
  "admin1",
  "admin2",
  "expenditure_debt",
  "income_v1_salaried_work",
  "wash_drinkingwatersource", 
  grep("edu_learning_conditions_reasons_v1", names(analysistools_MSNA_template_data), value = T)
)]

View(shorter_df)
########################################################################################
my_shorter_df <- df_main[, c(
  "admin1",
  "admin2",
  "cm_income_source_salaried_n",
  "wash_drinking_water_source", 
  grep("edu_community_modality", names(df_main), value = T)
)]

View(my_shorter_df)
paste0(unique(df_main$admin1))
########################################################################################

# i think we need sample data to test this
example_sample <- data.frame(
  strata = c("ET01", "ET02", "ET04","ET06","ET16","ET08"),
  population = c(30000, 50000, 80000,80000,80000,80000)
)

########################################################################################
my_example_sample <- data.frame(
  strata = c("ET01", "ET02", "ET04","ET06","ET16","ET08"),
  population = c(30000, 50000, 80000,80000,80000,80000)
)

########################################################################################

#View(example_sample)

my_weighted_shorter_df <- my_shorter_df %>%
  add_weights(my_example_sample,
              strata_column_dataset = "admin1",
              strata_column_sample = "strata",
              population_column = "population"
  )

View(my_weighted_shorter_df)

my_weighted_shorter_df[, c("admin1", "weights")] %>% head()

my_example_design <- srvyr::as_survey(my_weighted_shorter_df, strata = admin1, weights = weights)

View(my_example_design)

ex1_results <- create_analysis(design = my_example_design, sm_separator = "/")

#typeof(ex1_results)
#View(ex1_results)

# i want to write ex1_results to excel with multiple sheets
#write_xlsx(ex1_results, "ex1_results.xlsx")

