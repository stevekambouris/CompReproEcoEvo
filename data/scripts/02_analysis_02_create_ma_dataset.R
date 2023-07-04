# 02_analysis_02_create_ma_dataset.R
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script takes the Excel spreadsheet used for data entry about which
# articles ought to be regarded as meta-analyses and creates a formatted
# dataset with a final flag of which articles are meta-analyses (for the
# purposes of this study).

# Load required libraries.
library(tidyverse)
library(here)
library(openxlsx)

# Import the data entry spreadsheet.
ma_flags_raw <- read.xlsx(xlsxFile = here("data", "raw",
                                   "Coding_Form_Models_and_Methods.xlsx"),
                         sheet = "Model_choice")



# Format the data.
ma_flags_fmt <- ma_flags_raw %>% 
  mutate(is_ma_flag = case_when(
    is.na(claim_ma_recheck) == TRUE ~ Claim_MA,
    is.na(claim_ma_recheck) == FALSE ~ claim_ma_recheck,
    TRUE ~ NA_character_
  )) %>% 
  mutate(is_ma_flag_notes = case_when(
    is.na(claim_ma_recheck_notes) == TRUE ~ Claim_MA_notes,
    is.na(claim_ma_recheck_notes) == FALSE ~ claim_ma_recheck_notes,
    TRUE ~ NA_character_
  )) %>% 
  select(SearchID, Article_title, Claim_MA_title, Claim_MA_abstract,
         Claim_MA_keywords, Claim_MA_body, Claim_MA_quote, Claim_MA_pageref,
         is_ma_flag, is_ma_flag_notes) %>% 
  filter(is_ma_flag == "Y") %>% 
  arrange(SearchID)



# Import bibliometric data and attach to the meta-analysis info dataset.
bib_data <- read_csv(file = here("data", "clean",
                                 "bibliometric_data_all_studies.csv"))

ma_flags_bib <- ma_flags_fmt %>% 
  left_join(y = bib_data, by = "SearchID")



# Check if the article titles differ between the coding form and the Zotero
# export - would be sign of a mismatch of IDs or some other error.
check_titles <- ma_flags_bib %>% 
  filter(Article_title != Title)



# After confirming all article titles match, finalise (drop redundant field).
ma_flags_final <- ma_flags_bib %>% 
  select(-Article_title)



# Export the finalised dataset to a CSV.
write_csv(x = ma_flags_final, file = here("data", "clean",
                                          "list_of_meta_analyses.csv"))
