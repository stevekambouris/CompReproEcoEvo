# 02_analysis_01_format_bib_data.R
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script imports the bibliographic information from the Zotero export CSV
# and formats it for use with other datasets.
#
# Background: The Zotero library was built up manually from the information
# provided in the Scopus literature search results. However, there were errors
# found in the Scopus bibliographic data, so the Zotero library information is
# preferred.

# Load required libraries.
library(tidyverse)
library(here)

# Import the CSV exported from Zotero.
zotero_bib_raw <- read_csv(file = here("data", "raw",
                                       "Zotero_Export_Reproducibility_of_MAs_in_Eco-Evo_2021-03-11.csv"))

# Keep only those cases with the correct Extra field values.
# Create the SearchID field.
zotero_bib_fmt <- zotero_bib_raw %>% 
  filter(str_detect(Extra, "SearchID: SSA") == TRUE) %>% 
  mutate(SearchID = str_c("MA",
                          str_extract(Extra, "(?<=SearchResultID: )\\d{3}")))

# Check for duplicate SearchIDs.
zotero_searchid_dups <- zotero_bib_fmt %>% 
  group_by(SearchID) %>% 
  filter(n() > 1)

# <> Remove a duplicate record (due to inclusion of a Dryad data repo record
# with the same Extra information as the original article).
# <> Drop unneeded bibliographic fields.
# <> Drop the record (MA154; DOI:10.1111/jocn.13212) identified as a Scopus
# database error.
zotero_bib_final <- zotero_bib_fmt %>% 
  filter(SearchID != "MA154") %>% 
  filter(DOI != "10.5061/dryad.559cs") %>% 
  select(!(Editor:`Legislative Body`)) %>% 
  select(!(Series:Place)) %>% 
  select(!(Notes:`Link Attachments`)) %>% 
  select(!c(`Call Number`, Type, Archive, `Archive Location`, ISBN,
            `Num Pages`, `Number Of Volumes`, Language, Rights)) %>% 
  relocate(SearchID) %>% 
  arrange(SearchID)

# Export the clean bibliometric data to CSV.
write_csv(x = zotero_bib_final,
          file = here("data", "clean", "bibliometric_data_all_studies.csv"))