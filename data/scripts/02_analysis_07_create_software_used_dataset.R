# 02_analysis_07_create_software_used_dataset.R
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script imports the completed "software use" coding file, performs some
# checking, and then cleans it up for further analysis.

# Load packages ---------------------------------------------------------------- 
library(tidyverse)
library(here)
library(readxl)



# Import final list of meta-analyses -------------------------------------------
final_mas <- readr::read_csv(here::here("data", "clean",
                                        "list_of_meta_analyses.csv")) %>% 
  rename(search_id = SearchID)



# Import complete software use coding from Excel -------------------------------
softw_sum_raw <- readxl::read_xlsx(path = here::here("data", "raw",
                                                     "coding_form_software_mentioned.xlsx"),
                                   sheet = "software")

softw_det_raw <- readxl::read_xlsx(path = here::here("data", "raw",
                                                     "coding_form_software_mentioned.xlsx"),
                                   sheet = "software_details")



# Check the coded software data, Summary ---------------------------------------

# Are there any missing (or "extra") meta-analyses?
check_sum_missing <- final_mas %>% 
  anti_join(softw_sum_raw, by = "search_id")

check_sum_extra <- softw_sum_raw %>% 
  anti_join(final_mas, by = "search_id")

# Are there any invalid codes for any_software_specified?
check_sum_spec <- softw_sum_raw %>% 
  filter(!any_software_specified %in% c("Y", "N", "S"))

# Do all entries with a any_software_specified value of "Y" or "S" have at
# least one entry in the details sheet?
check_sum_indet <- softw_sum_raw %>% 
  filter(any_software_specified %in% c("Y", "S")) %>% 
  anti_join(softw_det_raw, by = "search_id")

# Do all entries with a any_software_specified value of "N" have NO matches in
# the details sheet?
check_sum_nodet <- softw_sum_raw %>% 
  filter(any_software_specified %in% c("N")) %>% 
  semi_join(softw_det_raw, by = "search_id")

# Perform a quick check to ensure all checks pass.
stopifnot(nrow(check_sum_missing) == 0,
          nrow(check_sum_extra) == 0,
          nrow(check_sum_spec) == 0,
          nrow(check_sum_indet) == 0,
          nrow(check_sum_nodet) == 0)

# Clean up the checking objects after inspecting them.
rm(list = c("check_sum_missing", "check_sum_extra", "check_sum_spec",
            "check_sum_indet", "check_sum_nodet"))



# Check the coded software data, Details ---------------------------------------

# Are any non open-text fields blank, or have invalid values?
check_det_invld <- softw_det_raw %>% 
  filter(is.na(software_details) == TRUE
         | is.na(software_pageref) == TRUE
         | is.na(version_specified) == TRUE
         | is.na(is_r_package) == TRUE
         | is.na(r_packages_mentioned) == TRUE
         | is.na(software_cited) == TRUE
         | !version_specified %in% c("Y", "N")
         | !is_r_package %in% c("Y", "N")
         | !r_packages_mentioned %in% c("Y", "N", "N/A")
         | !software_cited %in% c("Y", "N", "T")
         | !package_location %in% c("CRAN", "BioConductor", "other", "base",
                                    "N/A"))

# Check for problems with the detailing of software versions.
check_det_vers <- softw_det_raw %>% 
  filter((version_specified == "Y" & is.na(version_details) == TRUE)
         | (version_specified == "N" & is.na(version_details) == FALSE))

# Check for problems with the detailing of software citations.
check_det_cite <- softw_det_raw %>% 
  filter((software_cited %in% c("Y", "T") & is.na(citation_details) == TRUE)
         | (software_cited == "N" & is.na(citation_details) == FALSE))

# Check that all R software entries have the correct follow-on coding (and that
# all other cases don't).
check_det_R1 <- softw_det_raw %>% 
  filter((toupper(software_details) == "R" & !r_packages_mentioned %in% c("Y", "N"))
         | (toupper(software_details) != "R" & r_packages_mentioned != "N/A"))

# Check that cases of R with no packages mentioned have no associated R
# package entries, and the other way around.
r_packages <- softw_det_raw %>% 
  filter(is_r_package == "Y")
r_softwares <- softw_det_raw %>% 
  filter(toupper(software_details) == "R")

check_det_RpkgY <- r_softwares %>% 
  filter(r_packages_mentioned == "Y") %>% 
  anti_join(r_packages, by = "search_id")

check_det_RpkgN <- r_softwares %>% 
  filter(r_packages_mentioned == "N") %>% 
  semi_join(r_packages, by = "search_id")

# Check that all identified R packages have the correct follow-on coding (and
# that all non-R packages don't).
check_det_Rpkgs1 <- softw_det_raw %>% 
  filter((is_r_package == "Y" & !package_location %in% c("base", "CRAN", "BioConductor", "other"))
         | (is_r_package == "N" & package_location != "N/A"))

# Get all CRAN packages and format for matching.
all_pkgs <- available.packages()
all_pkg_names <- tibble(software_details = all_pkgs[, "Package"]) %>% 
  mutate(software_details = tolower(software_details))

# Check that the identified CRAN packages exist on CRAN.
check_det_Rpkgs2 <- softw_det_raw %>% 
  filter(is_r_package == "Y" & package_location == "CRAN") %>% 
  mutate(software_details = tolower(software_details)) %>% 
  anti_join(all_pkg_names, by = "software_details")

# Check that the identified non-CRAN packages do NOT exist on CRAN.
check_det_Rpkgs3 <- softw_det_raw %>% 
  filter(is_r_package == "Y" & package_location != "CRAN") %>% 
  mutate(software_details = tolower(software_details)) %>% 
  semi_join(all_pkg_names, by = "software_details")

# Perform a quick check to ensure all checks pass.
stopifnot(nrow(check_det_invld) == 0,
          nrow(check_det_vers) == 0,
          nrow(check_det_cite) == 0,
          nrow(check_det_R1) == 0,
          nrow(check_det_RpkgY) == 0,
          nrow(check_det_RpkgN) == 0,
          nrow(check_det_Rpkgs1) == 0,
          nrow(check_det_Rpkgs2) == 0,
          nrow(check_det_Rpkgs3) == 0)

# Print the packages picked up as no longer being on CRAN.
print(check_det_Rpkgs2$software_details)

# Clean up the checking objects after inspecting them.
rm(list = c("check_det_invld", "check_det_vers", "check_det_cite",
            "check_det_R1",
            "r_packages", "r_softwares", "check_det_RpkgY", "check_det_RpkgN",
            "check_det_Rpkgs1",
            "all_pkgs", "all_pkg_names", "check_det_Rpkgs2", "check_det_Rpkgs3"))



# Clean the data ---------------------------------------------------------------

# Summary-level data set
# (Note: no cleaning required.)
softw_sum_clean <- softw_sum_raw

# Detail-level data set
# Make some names of software packages consistent.
softw_det_clean <- softw_det_raw %>% 
  mutate(software_details = case_when(
    software_details == "ARCMAP" ~ "ArcMap",
    software_details == "Graphclick" ~ "GraphClick",
    software_details == "SPSS" ~ "SPSS/PASW",
    software_details == "PASW" ~ "SPSS/PASW",
    TRUE ~ software_details
  ))

# Check that software package names aren't duplicated within a particular
# meta-analysis article (which would likely be accidental duplication or
# double-entry).
check_det_dups <- softw_det_clean %>% 
  group_by(search_id, software_details) %>% 
  count() %>% 
  filter(n > 1)

# Clean up the checking objects after inspecting them.
stopifnot(nrow(check_det_dups) == 0)
rm(list = c("check_det_dups"))



# Export the data to file ------------------------------------------------------
write_csv(x = softw_sum_clean, file = here("data", "clean",
                                           "software_used_summary.csv"))

write_csv(x = softw_det_clean, file = here("data", "clean",
                                           "software_used_details.csv"))
