# 02_analysis_06_journal_policy_table.R
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script imports lists of journal data/code sharing policies, and extracts
# the journal titles relevant to this study.

# Load required libraries.
library(tidyverse)
library(here)
library(readxl)



# Specify the titles of the journals relevant to this study.
ma_journal_titles <- read_csv(file = here("analysis", "output",
                                          "journal_breakdown_latex.csv")) %>% 
  filter(journal_title != "Total") %>% 
  arrange(journal_title) %>% 
  pull(journal_title)



# Import data from Mislan, et al. (2016)----------------------------------------
mislan_raw <- readr::read_csv(file = here("data", "raw",
                                          "journal_policies_mislan_2016",
                                          "SoftwareInEcologyAnalysis3.csv"))

# Clean the data from Mislan, et al.
mislan_clean <- mislan_raw %>% 
  mutate(JournalTitle = case_when(
    JournalTitle == "American Naturalist" ~ "The American Naturalist",
    TRUE ~ JournalTitle
  )) %>% 
  arrange(JournalTitle)

# Check: which MA journal titles can be found in Mislan?
titles_found <- mislan_clean %>% 
  arrange(JournalTitle) %>% 
  filter(JournalTitle %in% ma_journal_titles)

# Check: which MA journal titles can't be found in Mislan?
titles_not_found <- ma_journal_titles[which(!ma_journal_titles %in% titles_found$JournalTitle)]



# Import data from Culina, et al. (2020)----------------------------------------
culina_raw <- readxl::read_xlsx(path = here::here("data", "raw",
                                                  "journal_policies_culina_2020",
                                                  "Updated_Table_Mislan_2020_v2.xlsx"),
                                sheet = "SoftwareInEcologyAnalysis")

# Clean the data.
culina_clean <- culina_raw %>% 
  mutate(`Full Journal Title` = case_when(
    `Full Journal Title` == "American Naturalist" ~ "The American Naturalist",
    TRUE ~ `Full Journal Title`
  )) %>% 
  arrange(`Full Journal Title`)

# Check: which MA journal titles can be found in Mislan?
titles_found2 <- culina_clean %>% 
  arrange(`Full Journal Title`) %>% 
  filter(`Full Journal Title` %in% ma_journal_titles)

# Check: which MA journal titles can't be found in Mislan?
titles_not_found2 <- ma_journal_titles[which(!ma_journal_titles %in% titles_found2$`Full Journal Title`)]



# Create a data policy summary table -------------------------------------------

# Create df with all journal titles.
# Base the data on the results from Mislan, et al. (2016).
# Add manually checked journals.
# Add JDAP member status.
# Add source column.
data_policies <- tibble(title = ma_journal_titles) %>% 
  left_join(titles_found, by = c("title" = "JournalTitle")) %>% 
  select(title, RequireDataRelease) %>% 
  rename(data_sharing_req = RequireDataRelease) %>% 
  mutate(source = case_when(
    is.na(data_sharing_req) == FALSE ~ "\\textcite{mislan_elevating_2016}",
    title == "Animal Behaviour" ~ "\\textcite{caetano_forgotten_2014}",
    title == "New Phytologist" ~ "\\textcite{magee_dawn_2014}",
    title %in% c("Biological Reviews",
                 "The Quarterly Review of Biology") ~ "journal website",
    TRUE ~ NA_character_
  )) %>% 
  mutate(is_jdap = case_when(
    title %in% c("Evolution",
                 "Journal of Evolutionary Biology",
                 "Molecular Ecology",
                 "The American Naturalist",
                 "Functional Ecology",
                 "Journal of Animal Ecology",
                 "Journal of Applied Ecology",
                 "Journal of Ecology") ~ "Y",
    TRUE ~ "N"
  )) %>% 
  mutate(data_sharing_req = case_when(
    data_sharing_req == "Yes" ~ "Y",
    data_sharing_req == "No" ~ "N",
    title %in% c("Animal Behaviour",
                 "Biological Reviews",
                 "New Phytologist",
                 "The Quarterly Review of Biology") ~ "N",
    TRUE ~ NA_character_
  )) %>% 
  mutate(latex_string = paste0(title, " & ", is_jdap, " & ", data_sharing_req,
                              " & ", source, " \\\\"))

# Export the data table to file.
write_csv(x = data_policies,
          file = here::here("analysis", "output",
                            "journal_data_policies_latex.csv"))


# Create a code policy summary table -------------------------------------------

code_policies <- tibble(title = ma_journal_titles) %>% 
  left_join(titles_found, by = c("title" = "JournalTitle")) %>% 
  select(title, RequireCodeRelease) %>% 
  left_join(titles_found2, by = c("title" = "Full Journal Title")) %>% 
  select(title, RequireCodeRelease,
         `Require computer code with publication_2020`) %>% 
  rename(policy_2015 = RequireCodeRelease,
         policy_2020 = `Require computer code with publication_2020`) %>% 
  mutate(policy_2015 = case_when(
    policy_2015 == "Yes" ~ "Y",
    policy_2015 == "No" ~ "N",
    TRUE ~ "-"),
    policy_2020 = case_when(
      policy_2020 == "No" ~ "N",
      policy_2020 == "Encouraged" ~ "E",
      policy_2020 == "Mandatory" ~ "M",
      policy_2020 == "Encouraged/Mandatory" ~ "E/M",
      TRUE ~ "-"
    ),
    check_2021 = case_when(
      title == "Animal Behaviour" ~ "E",
      title == "Biological Reviews" ~ "N.F.",
      title == "New Phytologist" ~ "N.F.",
      title == "The Quarterly Review of Biology" ~ "N.F.",
      TRUE ~ "-"
    )) %>% 
  mutate(latex_string = paste0(title, " & ", policy_2015, " & ", policy_2020,
                               " & ", check_2021, " \\\\"))

# Export the data table to file.
write_csv(x = code_policies,
          file = here::here("analysis", "output",
                            "journal_code_policies_latex.csv"))