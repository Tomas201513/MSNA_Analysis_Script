install.packages("writexl")
install.packages("readxl")
install.packages("stringr")
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)
library(analysistools)
source("create_analysis_t.R")

# remove environment
#rm(list=ls())

file_path="C:/Users/test/Downloads/20240826_ETH2403_eth_msna_cleaned_data_NEW.xlsx"
print(file_path)
sheet_names <- excel_sheets(file_path)
dataframes1 <- list()
for (sheet in sheet_names) {
  dataframes1[[sheet]] <- read_excel(file_path, sheet = sheet)}
print(sheet_names)

df_main <- dataframes1[["cleaned_data"]]
df_educ <- dataframes1[["cleaned_educ"]]

#View(df_main)
#View(df_educ)

my_shorter_df <- df_main
#View(my_shorter_df)

# my_shorter_df <- df_main[, c(
#   "_uuid",
#   "admin1",
#   "admin2",
#   "hh_size",
#   "ind_age_schooling_n",
#   "cm_income_source_salaried_n",
#   "wash_drinking_water_source", 
#   grep("edu_community_modality", names(df_main), value = T)
# )]
#View(my_shorter_df)
#paste0(unique(df_main$admin1))

# drop column that end with _other or /other 
#my_shorter_df <- my_shorter_df %>% select(-ends_with("_other"), -ends_with("/other"))
#View(my_shorter_df)

# convert columns which numeric with value NA to 0 and those column which are character with value NA to "NA"
my_shorter_df[ , sapply(my_shorter_df, is.numeric)][is.na(my_shorter_df[ , sapply(my_shorter_df, is.numeric)])] <- 0
#View(my_shorter_df)

# my_example_sample <- data.frame(
#   strata = c("ET01", "ET02", "ET04","ET06","ET16","ET08"),
#   population = c(30000, 50000, 280000,180000,800000,80000)
# )
#View(example_sample)

# my_weighted_shorter_df <- my_shorter_df %>%
#   add_weights(my_example_sample,
#               strata_column_dataset = "admin1",
#               strata_column_sample = "strata",
#               population_column = "population")
# 
# View(my_weighted_shorter_df)
# 
# my_weighted_shorter_df[, c("admin1", "weights")] %>% head()

#View(my_weighted_shorter_df)

#drop column that dont have any value
my_shorter_df <- my_shorter_df %>% select(which(colSums(is.na(my_shorter_df)) != nrow(my_shorter_df)))
#View(my_shorter_df)
my_example_design <- srvyr::as_survey(my_shorter_df, strata = admin1, weights = weight)
#View(my_example_design)

ex1_results <- create_analysis_t(design = my_example_design, sm_separator = "/")
#View(ex1_results)

ex1_results[["loa"]] %>% head()
#View(ex1_results[["loa"]])

ex2_results <- create_analysis(design = srvyr::as_survey(my_shorter_df), group_var = "admin1", sm_separator = "/")

ex2_results[["loa"]]
#View(ex2_results)

ex3_results <- create_analysis(design = srvyr::as_survey(my_shorter_df), group_var = c("admin1", "admin2"), sm_separator = "/")

ex3_results[["loa"]]
loa<- ex3_results[["loa"]]


ex4_results <- create_analysis(design = srvyr::as_survey(my_shorter_df), group_var = "admin1, admin2", sm_separator = "/")
#View(ex4_results)
loa<- ex4_results[["loa"]]
#View(ex4_results[["loa"]])
#View(analysistools_MSNA_template_loa)
#View(loa)
ex5_results <- create_analysis(design = srvyr::as_survey(my_shorter_df), loa = loa, sm_separator = "/")

ex5_results[["loa"]]


# somedata <- data.frame(
#   aa = 1:10,
#   bb = rep(c("a", "b"), 5),
#   weights = rep(c(.5, 1.5), 5),
#   stratas = rep(c("strata_a", "strata_b"), 5)
# )
# View(somedata)
# 
# me_design <- srvyr::as_survey(somedata)
# View(me_design)

###my_example_design <- srvyr::as_survey(my_weighted_shorter_df, strata = admin1, weights = weights)
me_design_w <- srvyr::as_survey(my_shorter_df)
create_analysis_mean(me_design_w, analysis_var = "cm_income_source_salaried_n")
create_analysis_mean(me_design_w,group_var = "admin1", analysis_var = "cm_income_source_salaried_n")
create_analysis_median(me_design_w, analysis_var = "cm_income_source_salaried_n")
create_analysis_median(me_design_w,group_var = "admin1", analysis_var = "cm_income_source_salaried_n")

#View(my_shorter_df)

my_shorter_df_W <- srvyr::as_survey(my_shorter_df, weights = weight)
create_analysis_mean(my_shorter_df_W, analysis_var = "cm_income_source_salaried_n")
create_analysis_mean(my_shorter_df_W, group_var = "admin1", analysis_var = "cm_income_source_salaried_n")
create_analysis_median(my_shorter_df_W, analysis_var = "cm_income_source_salaried_n")
create_analysis_median(my_shorter_df_W, group_var = "admin1", analysis_var = "cm_income_source_salaried_n")


#View(my_shorter_df)

###############################_Proportion_Select_One_###################################################

create_analysis_prop_select_one(srvyr::as_survey(my_shorter_df, strata = admin1),
                                group_var = NA,
                                analysis_var = "wash_drinking_water_source",
                                level = .95)

create_analysis_prop_select_one(srvyr::as_survey(my_shorter_df, strata = admin1),
                                group_var = "admin1",
                                analysis_var = "wash_drinking_water_source",
                                level = .95)



###############################_Proportion_Select_Multiple_###################################################

# replace column names that are separated by / with "."
colnames(my_shorter_df) <- gsub("/", ".", colnames(my_shorter_df))
#colnames(my_shorter_df)
#View(my_shorter_df)


#source("create_analysis_prop_select_multiple_t.R")
create_analysis_prop_select_multiple(srvyr::as_survey(my_shorter_df),
                                     group_var = NA,
                                     analysis_var = "edu_community_modality",
                                     level = 0.95)


create_analysis_prop_select_multiple(srvyr::as_survey(my_shorter_df),
                                     group_var = "admin1",
                                     analysis_var = "edu_community_modality",
                                     level = 0.95
)


########################################_Ratio_###############################################################

me_design <- srvyr::as_survey(my_shorter_df)
#View(me_design)

create_analysis_ratio(me_design,
                      analysis_var_numerator = "ind_age_schooling_n",
                      analysis_var_denominator = "hh_size",
)

create_analysis_ratio(me_design,
                      analysis_var_numerator = "ind_age_schooling_n",
                      analysis_var_denominator = "hh_size",
                      numerator_NA_to_0 = FALSE
)

### Get number of children aged 5-17 who are attending school ###

# rename columns of df_edu from "_submission__uuid" to "_uuid"
colnames(df_educ) <- gsub("_submission__uuid", "_uuid", colnames(df_educ))
#View(df_educ)
merged_data <- df_educ %>%
  group_by(`_uuid`) %>%
  summarise(edu_ind_age_schooling = sum(edu_ind_age_schooling == 1))
#View(merged_data)

final_data <- df_main %>%
  left_join(merged_data, by = "_uuid")
#View(final_data)
# make NA to 0 for edu_ind_age_schooling
final_data$edu_ind_age_schooling[is.na(final_data$edu_ind_age_schooling)] <- 0
View(final_data)

set.seed(8988)

create_analysis_ratio(srvyr::as_survey(final_data, weights = weight, strata = admin1),
                      group_var = NA,
                      analysis_var_numerator = "edu_ind_age_schooling",
                      analysis_var_denominator = "ind_age_5_17_n",
                      level = 0.95)

x<-create_analysis_ratio(srvyr::as_survey(final_data, weights = weight, strata = admin1),
                      group_var = "admin1",
                      analysis_var_numerator = "edu_ind_age_schooling",
                      analysis_var_denominator = "ind_age_5_17_n",
                      level = 0.95)


View(x)

######################################How_to_review_results#############################################################



results_to_review <- analysistools::analysistools_MSNA_template_with_ratio_results_table$results_table
View(results_to_review)

dataset_to_analyse <- analysistools::analysistools_MSNA_template_data

me_analysis <- create_analysis(srvyr::as_survey(final_data),
                               loa = loa,
                               sm_separator = "/")
View(me_analysis)

binded_results <- ex4_results$results_table %>%
  dplyr::full_join(me_analysis$results_table, by ="analysis_key")
View(binded_results)



review_results <- review_analysis(binded_results, 
                                  stat_columns_to_review = c("stat.x", "stat_low.x", "stat_upp.x"),
                                  stat_columns_to_compare_with = c("stat.y", "stat_low.y", "stat_upp.y"))

review_results$review_table %>%
  dplyr::group_by(stat) %>%
  dplyr::summarise(prop_correct = mean(review_check))


review_results$review_table %>%
  dplyr::group_by(stat, review_comment) %>%
  dplyr::tally(sort = T)

review_results$review_table %>%
  dplyr::filter(!review_check) %>%
  dplyr::select(analysis_type,analysis_var,group_var) %>% 
  dplyr::distinct()


analysis_key_column <-  c("mean @/@ income %/% NA @/@ NA %/% NA",
                          "prop_select_one @/@ water_source %/% tap_water @/@ district %/% district_a",
                          "prop_select_one @/@ water_source %/% tap_water @/@ district %/% district_a -/- population %/% displaced",
                          "prop_select_multiple @/@ source_information %/% relatives @/@ NA %/% NA")
test_analysis_results <- data.frame(
  test = c(
    "test equality",
    "test difference",
    "test Missing in y",
    "test Missing in x"
  ),
  stat_col.x = c(0, 1, 2, NA),
  stat_col.y = c(0, 2, NA, 3),
  analysis_key = analysis_key_column
)
review_results2 <- review_analysis(test_analysis_results,
                                   stat_columns_to_review = "stat_col.x",
                                   stat_columns_to_compare_with = "stat_col.y")
review_results2$review_table %>%
  dplyr::group_by(stat) %>%
  dplyr::summarise(prop_correct = mean(review_check))

review_results2$review_table %>%
  dplyr::group_by(stat, review_comment) %>%
  dplyr::tally(sort = T)

review_results2$review_table %>%
  dplyr::filter(!review_check) %>%
  dplyr::select(review_check, analysis_type,analysis_var,group_var) %>% 
  dplyr::distinct()

resultstable <- data.frame(analysis_index = c(
  "mean @/@ v1 %/% NA @/@ NA %/% NA",
  "mean @/@ v1 %/% NA @/@ gro %/% A",
  "mean @/@ v1 %/% NA @/@ gro %/% B"
))

key_table <- create_analysis_key_table(resultstable, "analysis_index")
key_table

View(key_table)

unite_variables(key_table)
