# 02_analysis_99_miscellaneous.R -----------------------------------------------
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script contains small snippets of code that don't warrant their own
# separate script file.

# Load required libraries.
library(tidyverse)
library(here)



# Calculate the percentile of meta-analyses with supplemental information ------

# Import the data and code sharing data file
data_dcss <- read_csv(file = here("data", "clean",
                                    "data_code_sharing_status.csv"))

temp <- 100*table(data_dcss$SupplementsIncluded, useNA = "ifany")/nrow(data_dcss)
has_supp_info <- round(temp[[2]],2)

no_supp <- data_dcss %>% 
  filter(SupplementsIncluded == FALSE)



# Calculate the percentage of article which shared data ------------------------
temp_df <- data_dcss %>% 
  filter(status_act_sharing %in% c("act_shared_both_code_data",
                                   "act_shared_data_only"))

n_shared_data <- nrow(temp_df)
pct_shared_data <- 100*nrow(temp_df)/nrow(data_dcss)



# Calculate the percentage of articles which nominally shared data -------------
temp_df <- data_dcss %>% 
  filter(status_nom_sharing %in% c("nom_shared_both_code_data",
                                   "nom_shared_data_only"))

n_nom_shared_data <- nrow(temp_df)
pct_nom_shared_data <- 100*nrow(temp_df)/nrow(data_dcss)



# Explore where the information about the nominally shared data came from ------
temp_df <- data_dcss %>% 
  filter(status_nom_sharing %in% c("nom_shared_both_code_data",
                                   "nom_shared_data_only"))

table(temp_df$DatasetInfoPaper, useNA = "ifany")
table(temp_df$DatasetInfoWebsite, useNA = "ifany")

temp_df <- data_dcss %>% 
  filter(status_nom_sharing %in% c("nom_shared_both_code_data",
                                   "nom_shared_data_only")) %>% 
  filter(is.na(DatasetInfoPaper))



# Explore where the information about the actually shared data came from -------
temp_df <- data_dcss %>% 
  filter(status_act_sharing %in% c("act_shared_both_code_data",
                                   "act_shared_data_only"))
table(temp_df$DatasetInfoPaper, useNA = "ifany")
table(temp_df$DatasetInfoWebsite, useNA = "ifany")

temp_df <- temp_df %>% 
  filter(DatasetInfoPaper == FALSE)



# Calculate pct of articles sharing code ---------------------------------------
table(data_dcss$status_nom_sharing)
table(data_dcss$status_act_sharing)

# Nominally shared code
temp_df <- data_dcss %>% 
  filter(status_nom_sharing %in% c("nom_shared_both_code_data",
                                   "nom_shared_code_only"))

n_nom_shared_code <- nrow(temp_df)
pct_nom_shared_code <- 100*nrow(temp_df)/nrow(data_dcss)

# Actually shared code
temp_df <- data_dcss %>% 
  filter(status_act_sharing %in% c("act_shared_both_code_data",
                                   "act_shared_code_only"))

n_act_shared_code <- nrow(temp_df)
pct_act_shared_code <- 100*nrow(temp_df)/nrow(data_dcss)

# The nominal shared code variable isn't helpful here, look at the variables
# about whether the code was mentioned in the article.
temp_df <- data_dcss %>% 
  filter(CodeInfoPaper == TRUE)

n_code_info_in_paper <- nrow(temp_df)
pct_code_info_in_paper <- 100*n_code_info_in_paper/nrow(data_dcss)



# Calculate % of articles with both data and code ------------------------------
temp_df <- data_dcss %>% 
  filter(status_act_sharing == "act_shared_both_code_data")

n_shared_both <- nrow(temp_df)
pct_shared_both <- 100*n_shared_both/nrow(data_dcss)



# Create a table breaking down the types of code shared ------------------------
data_codeshared <- data_dcss %>% 
  filter(status_act_sharing %in% c("act_shared_both_code_data",
                                   "act_shared_code_only")) %>% 
  mutate(code_type_clean = case_when(
    CodeType %in% c("R (txt)", "R, markdown") ~ "R",
    CodeType %in% c("FORTRAN?") ~ "FORTRAN",
    CodeType %in% c("R, cpp") ~ "R and C++",
    TRUE ~ CodeType
  ))

code_bytype <- data_codeshared %>% 
  group_by(code_type_clean) %>%
  summarise(n=n()) %>%
  mutate(pct = 100*n/sum(n))

# Add in a total row at the bottom.
code_total <- code_bytype %>%
  mutate(code_type_clean = "Total") %>%
  group_by(code_type_clean) %>%
  summarise_all(sum) %>%
  ungroup()

# Compile into a single object, add LaTeX-formatted row syntax for tabular
# environment.
code_bytype_final <- code_bytype %>%
  add_row(code_total) %>% 
  mutate(latex_fmt = paste(code_type_clean, "&", n, "&",
                           sprintf("%.1f", pct), "\\\\"))

# Export formatted data set.
write_csv(x = code_bytype_final, file = here("analysis", "output",
                                             "code_type_breakdown_latex.csv"))