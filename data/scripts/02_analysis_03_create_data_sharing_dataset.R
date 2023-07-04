# 02_analysis_03_create_data_sharing_dataset.R
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script takes the Excel spreadsheet used for data entry about which
# articles shared their data and/or code, and formats it for further analysis.



# Load required libraries.
library(tidyverse)
library(here)
library(openxlsx)



# Import data and perform initial cleaning/formatting --------------------------

# Import the list of meta-analyses.
list_of_mas <- read_csv(file = here("data", "clean",
                                    "list_of_meta_analyses.csv"))

# Import the data entry spreadsheet.
sharing_raw <- read.xlsx(xlsxFile = here("data", "raw",
                                         "Coding_Form_Data_and_Code_Sharing.xlsx"),
                         sheet = "Data_Sharing")

# Create an updated `SearchID` to be able to match entries from the data/code
# sharing data entry form to the list of meta-analyses.
sharing_fmt <- sharing_raw %>% 
  select(-SearchID) %>% 
  mutate(SearchID = paste0("MA", sprintf("%03d", SearchResultID))) %>% 
  relocate(SearchID) %>% 
  select(-SearchResultID) %>% 
  semi_join(list_of_mas, by = "SearchID")



# Perform checking of the imported data entry form -----------------------------


# Check what type of article the remaining articles in the data sharing form
# have been coded as.
# (They should all be coded as "Nominal MA".)
check_articletype <- sharing_fmt %>% 
  filter(ArticleType != "Nominal MA")

# Check bibliographic details


# Check all records have complete data for the sharing flag variables.
check_missing_flags <- sharing_fmt %>% 
  filter(is.na(DatasetsNominallyIncluded) == TRUE
         | is.na(DatasetsIncluded) == TRUE
         | is.na(CodeNominallyIncluded) == TRUE
         | is.na(CodeIncluded))

# Check that nominal and actual flags are consistent.
# (All other combinations - TRUE; TRUE, FALSE; FALSE, and TRUE; FALSE - are
# valid for the nominal and actual availability flags.)
check_flag_clashes <- sharing_fmt %>% 
  filter((DatasetsNominallyIncluded == FALSE & DatasetsIncluded == TRUE)
         | (CodeNominallyIncluded == FALSE & CodeIncluded == TRUE))

# Check missing dataset source.
check_missing_datasrc <- sharing_fmt %>% 
  filter(DatasetsIncluded == TRUE
         & is.na(DatasetsSource) == TRUE)

# Check missing dataset URL (where not on journal publisher website or tables
# are published within the article itself).
check_missing_dataurl <- sharing_fmt %>% 
  filter(DatasetsIncluded == TRUE
         & is.na(DatasetsURL) == TRUE
         & ! DatasetsSource %in% c("Journal website", "Tables in paper",
         "Table in paper"))

# Check missing data file type (where tables are not published within the
# article itself).
check_missing_datafmt <- sharing_fmt %>% 
  filter(DatasetsIncluded == TRUE
         & is.na(DataFormat) == TRUE
         & ! DatasetsSource %in% c("Tables in paper", "Table in paper"))

# Check the only records with 0 shared files are those who state the data is
# included in tables within the article.
check_zero_datafiles <- sharing_fmt %>% 
  filter(DatasetsIncluded == TRUE
         & NumDataFiles == 0
         & ! DatasetsSource %in% c("Tables in paper", "Table in paper"))

# Check missing code source.
check_missing_codesrc <- sharing_fmt %>% 
  filter(CodeIncluded == TRUE
         & is.na(CodeSource) == TRUE)

# Check missing code URL (where not on journal publisher website or tables
# are published within the article itself).
check_missing_codeurl <- sharing_fmt %>% 
  filter(CodeIncluded == TRUE
         & is.na(CodeURL) == TRUE
         & ! CodeSource %in% c("Journal website"))

# Check missing code file type.
check_missing_codetyp <- sharing_fmt %>% 
  filter(CodeIncluded == TRUE
         & is.na(CodeType) == TRUE)

# Check no records have 0 code files shared.
check_zero_codefiles <- sharing_fmt %>% 
  filter(CodeIncluded == TRUE
         & NumCodeFiles == 0)



# Add variables for further summary and analysis -------------------------------

# Add status variables which summarise the nominal and actual data/code sharing
# status of each meta-analysis, based on the boolean flags used during data
# entry.
sharing_foranalysis <- sharing_fmt %>% 
  mutate(status_nom_sharing = case_when(
    DatasetsNominallyIncluded == TRUE
    & CodeNominallyIncluded == TRUE ~ "nom_shared_both_code_data",
    DatasetsNominallyIncluded == TRUE
    & CodeNominallyIncluded == FALSE ~ "nom_shared_data_only",
    DatasetsNominallyIncluded == FALSE
    & CodeNominallyIncluded == TRUE ~ "nom_shared_code_only",
    DatasetsNominallyIncluded == FALSE
    & CodeNominallyIncluded == FALSE ~ "nom_shared_none",
    TRUE ~ NA_character_
  )) %>% 
  mutate(status_act_sharing = case_when(
    DatasetsIncluded == TRUE
    & CodeIncluded == TRUE ~ "act_shared_both_code_data",
    DatasetsIncluded == TRUE
    & CodeIncluded == FALSE ~ "act_shared_data_only",
    DatasetsIncluded == FALSE
    & CodeIncluded == TRUE ~ "act_shared_code_only",
    DatasetsIncluded == FALSE
    & CodeIncluded == FALSE ~ "act_shared_none",
    TRUE ~ NA_character_
  ))


# Check to see if the status variables make sense when summarised.
check_statuses <- sharing_foranalysis %>% 
  group_by(status_nom_sharing, status_act_sharing) %>% 
  summarise(n = n())



# Create summaries of data repositories and data file formats ------------------

# Create a data set which contains data locations (repositories, etc.),
# just for those meta-analyses which included data.
sharing_data_repos <- sharing_foranalysis %>% 
  filter(status_act_sharing %in% c("act_shared_both_code_data",
                                   "act_shared_data_only")) %>% 
  select(SearchID, status_nom_sharing, status_act_sharing,
         DatasetsSource, DatasetsURL) %>% 
  separate(col = DatasetsSource, into = c("source_1", "source_2"),
           sep = "\\,") %>% 
  pivot_longer(cols = starts_with("source_"),
               names_to = "original_source",
               values_to = "data_location",
               values_drop_na = TRUE) %>% 
  mutate(data_location = stringr::str_trim(data_location)) %>% 
  mutate(data_location = case_when(
    data_location == "journal website" ~ "Journal website",
    data_location == "Table in paper" ~ "Table(s) in article",
    data_location == "Tables in paper" ~ "Table(s) in article",
    TRUE ~ data_location
  )) %>% 
  select(-original_source)

# Check that no single meta-analysis has duplicates of the same data source.
check_repo_dupl <- sharing_data_repos %>% 
  group_by(SearchID, data_location) %>% 
  summarise(n = n()) %>% 
  filter(n != 1)

# Create a data set which contains data formats,
# just for those meta-analyses which included data.
sharing_data_fmts <- sharing_foranalysis %>% 
  filter(status_act_sharing %in% c("act_shared_both_code_data",
                                   "act_shared_data_only")) %>% 
  select(SearchID, status_nom_sharing, status_act_sharing,
         DatasetsSource, DataFormat) %>% 
  mutate(DataFormat = case_when(
    DatasetsSource %in% c("Table in paper", "Tables in paper")
    & is.na(DataFormat) ~ "pdf",
    TRUE ~ DataFormat
  )) %>% 
  select(-DatasetsSource) %>% 
  separate(col = DataFormat, into = c("format_1", "format_2", "format_3"),
           sep = "\\,") %>% 
  pivot_longer(cols = starts_with("format_"),
               names_to = "orig_format_var",
               values_to = "data_format",
               values_drop_na = TRUE) %>% 
  mutate(data_format = stringr::str_trim(data_format)) %>% 
  mutate(format_category = case_when(
    data_format == "csv" ~ "Comma-separated values (CSV)",
    data_format %in% c("doc", "docx") ~ "Microsoft Word document",
    data_format %in% c("xls", "xlsx") ~ "Microsoft Excel spreadsheet",
    data_format == "pdf" ~ "Portable Document Format (PDF)",
    data_format == "htm" ~ "Hypertext Markup Language (HTML)",
    data_format == "rtf" ~ "Rich Text Format (RTF)",
    data_format %in% c("new", "nex", "tre", "txt") ~ "Plain text formats",
    data_format == "Rdata" ~ "RData format",
    data_format == "obj" ~ "Other binary formats",
    TRUE ~ NA_character_
  )) %>% 
  select(-orig_format_var)

# Check cases where a single meta-analysis has multiple of the same data
# format category.
# It's OK if there are some "double ups" of format categories due to files of
# different formats within the same format category.
check_fmts_dupl <- sharing_data_fmts %>% 
  group_by(SearchID, format_category) %>% 
  summarise(n = n()) %>% 
  filter(n != 1) %>% 
  ungroup() %>% 
  left_join(sharing_foranalysis, by = "SearchID") %>% 
  select(SearchID, format_category, n, DataFormat)



# Export formatted data sets ---------------------------------------------------

write_csv(x = sharing_foranalysis, file = here("data", "clean",
                                          "data_code_sharing_status.csv"))

write_csv(x = sharing_data_repos, file = here("data", "clean",
                                               "shared_data_locations.csv"))

write_csv(x = sharing_data_fmts, file = here("data", "clean",
                                              "shared_data_formats.csv"))
