# 02_analysis_52_check_pdfs_for_keywords.R
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script reads in PDF text and checks it for keywords that are likely to
# be about the software used to calculate results.
#
# Keywords to consider:
# " R "
# "package"
# "metafor"
# "software"
# "R 3"
# "r-project"
# "mcmcglmm"

# Load required libraries ------------------------------------------------------
library(tidyverse)
library(readr)
library(here)

# This is a non-CRAN package, https://github.com/stevekambouris/pdfkeywords
library(pdfkeywords)



# List all the PDFs of meta-analysis articles ----------------------------------
pdf_filenames <- paste0(here("article_pdfs"), "/",
                        list.files(path = here("article_pdfs"),
                                   pattern = "*.pdf"))


# Extract the keyword "software".
keyword_software <- pdfkeywords::extract_keyword(file = pdf_filenames,
                                                 keyword = "software",
                                                 nbefore = 3,
                                                 nafter = 3)

keyword_cma <- pdfkeywords::extract_keyword(file = pdf_filenames,
                                            keyword = "cma",
                                            nbefore = 3,
                                            nafter = 3)

keyword_metawin <- pdfkeywords::extract_keyword(file = pdf_filenames,
                                                keyword = "metawin",
                                                nbefore = 3,
                                                nafter = 3)

keyword_metafor <- pdfkeywords::extract_keyword(file = pdf_filenames,
                                                keyword = "metafor",
                                                nbefore = 3,
                                                nafter = 3)

keyword_mcglmm <- pdfkeywords::extract_keyword(file = pdf_filenames,
                                                keyword = "mcmcglmm",
                                                nbefore = 3,
                                                nafter = 3)


# Clean the search results -----------------------------------------------------
keyword_cma_clean <- keyword_cma %>% 
  filter(grepl(x = Keyword_extract, pattern = "Vcmax") == FALSE) %>% 
  filter(grepl(x = Keyword_extract, pattern = "cMahon") == FALSE) %>% 
  filter(grepl(x = Keyword_extract,
               pattern = "chacma",
               ignore.case = TRUE) == FALSE) %>% 
  filter(grepl(x = Keyword_extract, pattern = "IMAR-CMA") == FALSE) %>% 
  filter(grepl(x = Keyword_extract,
               pattern = "arcmap",
               ignore.case = TRUE) == FALSE) %>% 
  filter(grepl(x = Keyword_extract,
               pattern = "dicmarker",
               ignore.case = TRUE) == FALSE) %>% 
  filter(grepl(x = Keyword_extract,
               pattern = "ccmar",
               ignore.case = TRUE) == FALSE) %>% 
  filter(grepl(x = Keyword_extract, pattern = "mcmaster") == FALSE) %>% 
  mutate(search_id = str_extract(PDF_file, "MA\\d\\d\\d")) %>% 
  group_by(search_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  mutate(flag_cma = TRUE) %>% 
  select(search_id, flag_cma)

keyword_metafor_clean <- keyword_metafor %>% 
  mutate(search_id = str_extract(PDF_file, "MA\\d\\d\\d")) %>% 
  group_by(search_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  mutate(flag_metafor = TRUE) %>% 
  select(search_id, flag_metafor)

keyword_metawin_clean <- keyword_metawin %>% 
  mutate(search_id = str_extract(PDF_file, "MA\\d\\d\\d")) %>% 
  group_by(search_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  mutate(flag_metawin = TRUE) %>% 
  select(search_id, flag_metawin)

keyword_mcglmm_clean <- keyword_mcglmm %>% 
  mutate(search_id = str_extract(PDF_file, "MA\\d\\d\\d")) %>% 
  group_by(search_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  mutate(flag_mcmcglmm = TRUE) %>% 
  select(search_id, flag_mcmcglmm)



# Import the list of all meta-analyses -----------------------------------------
all_mas <- readr::read_csv(file = here("data", "clean",
                                       "list_of_meta_analyses.csv")) %>% 
  rename(search_id = SearchID)



# Merge the results ------------------------------------------------------------
all_merged <- all_mas %>% 
  left_join(keyword_cma_clean, by = "search_id") %>% 
  left_join(keyword_mcglmm_clean, by = "search_id") %>% 
  left_join(keyword_metafor_clean, by = "search_id") %>% 
  left_join(keyword_metawin_clean, by = "search_id") %>% 
  mutate(flag_cma = replace_na(flag_cma, FALSE)) %>% 
  mutate(flag_mcmcglmm = replace_na(flag_mcmcglmm, FALSE)) %>% 
  mutate(flag_metafor = replace_na(flag_metafor, FALSE)) %>% 
  mutate(flag_metawin = replace_na(flag_metawin, FALSE))



# Export the meta-analysis data merged with keyword flags ----------------------
readr::write_csv(x = all_merged,
                 file = here("data", "raw",
                             "ma_software_keyword_details.csv"))
