# 02_analysis_05_journal_breakdown_table.R -------------------------------------
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script imports formatted bibliographic data of all studies, and creates
# a summary listing the number of studies from each journal title.

# Load required libraries.
library(tidyverse)
library(here)



# Import data and perform initial cleaning/formatting --------------------------

# Import all bibliographic records from the study.
all_studies <- read_csv(file = here("data", "clean",
                                    "bibliometric_data_all_studies.csv"))

# Import the meta-analyses only data set.
ma_studies <- read_csv(file = here("data", "clean",
                                   "list_of_meta_analyses.csv")) %>% 
  mutate(is_ma = TRUE) %>% 
  select(SearchID, is_ma)



# Create a breakdown of article numbers by journal title -----------------------

# Add the is_ma flag to the all-studies data and summarise by journal title.
# Add in a row for the journal Evolutionary Ecology, which was included in the
# literature search but returned no results.
# Sort by the number of meta-analyses found, descending.
journal_data_ee <- tibble(`Publication Title` = "Evolutionary Ecology",
                          is_meta_n = 0,
                          not_meta_n = 0, .rows = 1)

journal_data <- all_studies %>%
  left_join(ma_studies, by = "SearchID") %>%
  mutate(is_ma = case_when(
    is.na(is_ma) ~ FALSE,
    TRUE ~ is_ma
  )) %>%
  group_by(`Publication Title`, is_ma) %>%
  summarise(n=n()) %>%
  spread(is_ma, n) %>%
  rename(is_meta_n=`TRUE`, not_meta_n=`FALSE`) %>%
  replace_na(list(is_meta_n = 0, not_meta_n = 0)) %>%
  ungroup() %>%
  arrange(desc(is_meta_n), `Publication Title`) %>% 
  add_row(journal_data_ee) %>%
  rowwise() %>%
  mutate(all_n = sum(is_meta_n, not_meta_n)) %>%
  ungroup() %>%
  mutate(is_meta_pct = 100*is_meta_n/sum(is_meta_n),
         not_meta_pct = 100*not_meta_n/sum(not_meta_n),
         all_pct = 100*all_n/sum(all_n)) %>%
  relocate(`Publication Title`,
           is_meta_n, is_meta_pct,
           not_meta_n, not_meta_pct,
           all_n, all_pct)

# Add in a total row at the bottom.
journal_total <- journal_data %>%
  mutate(`Publication Title` = "Total") %>%
  group_by(`Publication Title`) %>%
  summarise_all(sum) %>%
  ungroup()

# Compile into a single object, add LaTeX-formatted row syntax for tabular
# environment.
journal_data_final <- journal_data %>%
  add_row(journal_total) %>% 
  rename(journal_title=`Publication Title`) %>% 
  mutate(latex_fmt = paste(journal_title, "&", is_meta_n, "&",
                           sprintf("%.1f", is_meta_pct), "&",
                           not_meta_n, "&",
                           sprintf("%.1f", not_meta_pct), "&",
                           all_n, "&",
                           sprintf("%.1f", all_pct), "\\\\"))



# Export formatted data sets ---------------------------------------------------

write_csv(x = journal_data_final, file = here("analysis", "output",
                                              "journal_breakdown_latex.csv"))



# Create a breakdown of article numbers by publication year --------------------

pubyear_data <- ma_studies %>%
  left_join(all_studies, by = "SearchID") %>%
  group_by(`Publication Year`) %>%
  summarise(n=n()) %>%
  mutate(pct = 100*n/sum(n))

# Add in a total row at the bottom.
pubyear_total <- pubyear_data %>%
  mutate(`Publication Year` = "Total") %>%
  group_by(`Publication Year`) %>%
  summarise_all(sum) %>%
  ungroup()

# Compile into a single object, add LaTeX-formatted row syntax for tabular
# environment.
pubyear_data_final <- pubyear_data %>%
  mutate(`Publication Year` = as.character(`Publication Year`)) %>% 
  add_row(pubyear_total) %>% 
  mutate(latex_fmt = paste(`Publication Year`, "&", n, "&",
                           sprintf("%.1f", pct), "\\\\"))

# Export formatted data set.
write_csv(x = pubyear_data_final, file = here("analysis", "output",
                                              "ma_pubyear_breakdown_latex.csv"))