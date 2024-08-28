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

file_path="C:/Users/test/Downloads/20240826_ETH2403_eth_msna_cleaned_data_NEW.xlsx"
print(file_path)
sheet_names <- excel_sheets(file_path)
dataframes1 <- list()
for (sheet in sheet_names) {
  dataframes1[[sheet]] <- read_excel(file_path, sheet = sheet)}
print(sheet_names)

df_main <- dataframes1[[2]]
#View(df_main)

# remove environment
#rm(list=ls())

my_shorter_df <- df_main[, c(
  "admin1",
  "admin2",
  "cm_income_source_salaried_n",
  "wash_drinking_water_source", 
  grep("edu_community_modality", names(df_main), value = T)
)]
#View(my_shorter_df)
paste0(unique(df_main$admin1))

# drop column that end with _other or /other 
my_shorter_df <- my_shorter_df %>% select(-ends_with("_other"), -ends_with("/other"))
#View(my_shorter_df)

# convert columns which numeric with value NA to 0 and those column which are character with value NA to "NA"
my_shorter_df[ , sapply(my_shorter_df, is.numeric)][is.na(my_shorter_df[ , sapply(my_shorter_df, is.numeric)])] <- 0
#View(my_shorter_df)

my_example_sample <- data.frame(
  strata = c("ET01", "ET02", "ET04","ET06","ET16","ET08"),
  population = c(30000, 50000, 80000,80000,80000,80000)
)
#View(example_sample)

my_weighted_shorter_df <- my_shorter_df %>%
  add_weights(my_example_sample,
              strata_column_dataset = "admin1",
              strata_column_sample = "strata",
              population_column = "population"
  )
View(my_weighted_shorter_df)

my_weighted_shorter_df[, c("admin1", "weights")] %>% head()
#View(my_weighted_shorter_df)

my_example_design <- srvyr::as_survey(my_weighted_shorter_df, strata = admin1, weights = weights)
#View(my_example_design)

ex1_results <- create_analysis_t(design = my_example_design, sm_separator = "/")
#View(ex1_results)

ex1_results[["loa"]] %>% head()

ex2_results <- create_analysis(design = srvyr::as_survey(my_shorter_df), group_var = "admin1", sm_separator = "/")

ex2_results[["loa"]]

ex3_results <- create_analysis(design = srvyr::as_survey(my_shorter_df), group_var = c("admin1", "admin2"), sm_separator = "/")

ex3_results[["loa"]]

ex4_results <- create_analysis(design = srvyr::as_survey(my_shorter_df), group_var = "admin1, admin2", sm_separator = "/")

loa<- ex4_results[["loa"]]
View(ex4_results[["loa"]])
View(analysistools_MSNA_template_loa)
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
me_design_w <- srvyr::as_survey(my_weighted_shorter_df)
create_analysis_mean(me_design_w, analysis_var = "cm_income_source_salaried_n")
create_analysis_mean(me_design_w,group_var = "admin1", analysis_var = "cm_income_source_salaried_n")
create_analysis_median(me_design_w, analysis_var = "cm_income_source_salaried_n")
create_analysis_median(me_design_w,group_var = "admin1", analysis_var = "cm_income_source_salaried_n")

my_shorter_df_W <- srvyr::as_survey(my_weighted_shorter_df, weights = weights)
create_analysis_mean(my_shorter_df_W, analysis_var = "cm_income_source_salaried_n")
create_analysis_mean(my_shorter_df_W, group_var = "admin1", analysis_var = "cm_income_source_salaried_n")
create_analysis_median(my_shorter_df_W, analysis_var = "cm_income_source_salaried_n")
create_analysis_median(my_shorter_df_W, group_var = "admin1", analysis_var = "cm_income_source_salaried_n")




# Proportion_ Select_One

somedata <- data.frame(
  groups = sample(c("group_a", "group_b"),
                  size = 100,
                  replace = TRUE
  ),
  value = sample(c("a", "b", "c"),
                 size = 100, replace = TRUE,
                 prob = c(.6, .4, .1)
  )
)
View(somedata)
create_analysis_prop_select_one(srvyr::as_survey(my_shorter_df, strata = admin1),
                                group_var = NA,
                                analysis_var = "wash_drinking_water_source",
                                level = .95
)

create_analysis_prop_select_one(srvyr::as_survey(my_shorter_df, strata = admin1),
                                group_var = "admin1",
                                analysis_var = "wash_drinking_water_source",
                                level = .95
)




# Proportion_ Select_Multiple

somedata <- data.frame(
  groups = sample(c("group_a", "group_b"), size = 100, replace = T),
  smvar = rep(NA_character_, 100),
  smvar.option1 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.7, .3)),
  smvar.option2 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.6, .4)),
  smvar.option3 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.1, .9)),
  smvar.option4 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.8, .2)),
  uuid = 1:100 %>% as.character()
) %>%
  cleaningtools::recreate_parent_column(uuid = "uuid", sm_separator = ".")

somedata <- somedata$data_with_fix_concat
View(somedata)

View(my_shorter_df)
# makeedu_community_modality where its value is NA to "NA"
my_shorter_df[ , sapply(my_shorter_df, is.character)][is.na(my_shorter_df[ , sapply(my_shorter_df, is.character)])] <- "na"
View(my_shorter_df)

x= "none oms improve_pcosi improve_cowis improve_ss provide_twtpocrt  support_for_enrolment increase_tp increase_qt change_curriculum provide_edu awareness_raising school_feeding dnk pnta provide_class provide_mhealth provide_ssupplies provide_cash provide_transportation improve_access"

# make x array of strings
x <- x %>% str_split(" ") %>% unlist()
paste0(x)
# count the number of comma separated values in x
length(strsplit(x, ",")[[1]])
# find all the uniq values in my_shorter_df$edu_community_modality treat space as a separator and make them array of strings
y=unique(my_shorter_df$edu_community_modality) %>% str_split(" ") %>% unlist() %>% unique()
paste0(y)

# make y array of strings
y <- y %>% str_split(" ") %>% unlist()
paste0(y)



source("create_analysis_prop_select_multiple_t.R")
create_analysis_prop_select_multiple_t(srvyr::as_survey(my_shorter_df),
                                     group_var = NA,
                                     analysis_var = "edu_community_modality",
                                     level = 0.95
)

create_analysis_prop_select_multiple(srvyr::as_survey(my_shorter_df),
                                     group_var = "admin1",
                                     analysis_var = "edu_community_modality",
                                     level = 0.95
)
