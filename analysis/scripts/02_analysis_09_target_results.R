# 02_analysis_09_target_results.R ----------------------------------------------
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script imports the CSV files output from the reproduction attempts,
# and summarises the data for the results section of the chapter.



# Load required libraries ------------------------------------------------------
library(tidyverse)
library(here)
library(ggplot2)
library(xtable)

# MA016
# MA060
# MA062
# MA065
# MA067
# MA068 - this case has an alternative result (led by code)
# MA071
# MA074
# MA081
# MA091
# MA092
# MA094 - this case has an alternative result (led by code)
# MA095
# MA126
# MA129 - this case has a char comparison file, too
# MA145
# MA147
# MA155
# MA188
# MA191
# MA198
# MA202
# MA211
# MA212 - this case has a char comparison file, too
# MA213
# MA229



# Import data, main target results ---------------------------------------------

# Import the result comparison files of the MAs identified as containing the
# original target result.
result_MA016 <- read_csv(here("reproducibility_reports", "MA016", "output", "MA016_result_comparison.csv"))
result_MA060 <- read_csv(here("reproducibility_reports", "MA060", "output", "MA060_result_comparison.csv"))
result_MA062 <- read_csv(here("reproducibility_reports", "MA062", "output", "MA062_result_comparison.csv"))
result_MA065 <- read_csv(here("reproducibility_reports", "MA065", "output", "MA065_result_comparison.csv"))
result_MA067 <- read_csv(here("reproducibility_reports", "MA067", "output", "MA067_result_comparison.csv"))
result_MA068 <- read_csv(here("reproducibility_reports", "MA068", "output", "MA068_result_comparison.csv"))
result_MA071 <- read_csv(here("reproducibility_reports", "MA071", "output", "MA071_result_comparison.csv"))
result_MA074 <- read_csv(here("reproducibility_reports", "MA074", "output", "MA074_result_comparison.csv"))
result_MA081 <- read_csv(here("reproducibility_reports", "MA081", "output", "MA081_result_comparison.csv"))
result_MA091 <- read_csv(here("reproducibility_reports", "MA091", "output", "MA091_result_comparison.csv"))
result_MA092 <- read_csv(here("reproducibility_reports", "MA092", "output", "MA092_result_comparison.csv"))
result_MA094 <- read_csv(here("reproducibility_reports", "MA094", "output", "MA094_result_comparison.csv"))
result_MA095 <- read_csv(here("reproducibility_reports", "MA095", "output", "MA095_result_comparison.csv"))
result_MA126 <- read_csv(here("reproducibility_reports", "MA126", "output", "MA126_result_comparison.csv"))
result_MA129 <- read_csv(here("reproducibility_reports", "MA129", "output", "MA129_result_comparison.csv"))
result_MA129_char <- read_csv(here("reproducibility_reports", "MA129", "output", "MA129_result_char_comparison.csv"))
result_MA145 <- read_csv(here("reproducibility_reports", "MA145", "output", "MA145_result_comparison.csv"))
result_MA147 <- read_csv(here("reproducibility_reports", "MA147", "output", "MA147_result_comparison.csv"))
result_MA155 <- read_csv(here("reproducibility_reports", "MA155", "output", "MA155_result_comparison.csv"))
result_MA188 <- read_csv(here("reproducibility_reports", "MA188", "output", "MA188_result_comparison.csv"))
result_MA191 <- read_csv(here("reproducibility_reports", "MA191", "output", "MA191_result_comparison.csv"))
result_MA198 <- read_csv(here("reproducibility_reports", "MA198", "output", "MA198_result_comparison.csv"))
result_MA202 <- read_csv(here("reproducibility_reports", "MA202", "output", "MA202_result_comparison.csv"))
result_MA211 <- read_csv(here("reproducibility_reports", "MA211", "output", "MA211_result_comparison.csv"))
result_MA212 <- read_csv(here("reproducibility_reports", "MA212", "output", "MA212_result_comparison.csv"))
result_MA212_char <- read_csv(here("reproducibility_reports", "MA212", "output", "MA212_result_char_comparison.csv"))
result_MA213 <- read_csv(here("reproducibility_reports", "MA213", "output", "MA213_result_comparison.csv"))
result_MA229 <- read_csv(here("reproducibility_reports", "MA229", "output", "MA229_result_comparison.csv"))



# Format data, main target results ---------------------------------------------

# Format case MA155 where there is an "extra" target value that wasn't reported
# in the original paper.
result_MA155_fmt1 <- result_MA155 %>% 
  filter(compared != "n")

# Format the cases where the result comparison output is not in the canonical
# effect size format (cases MA129 and MA212).

# MA129 - numeric result
result_MA129_fmt1 <- result_MA129 %>% 
  mutate(ID = "MA129", es_type = "N/A") %>% 
  mutate(result_type = paste0("Table 1A, ", compared)) %>% 
  select(-compared) %>% 
  rename(compared = column_name) %>% 
  relocate(ID, result_type, es_type, compared, original, repro, percent_error,
           note)

# MA129 - character result
result_MA129_fmt2 <- result_MA129_char %>% 
  select(-result_type) %>% 
  mutate(result_type = paste0("Table 1A, ", compared)) %>% 
  select(-compared) %>% 
  mutate(compared = paste0(column_name, " (km radius)")) %>% 
  select(-column_name) %>% 
  mutate(percent_error = NA_real_) %>% 
  mutate(note = case_when(
    values_identical == "Yes" ~ "Categorical values match exactly",
    values_identical == "No" ~ "Categorical values do not match",
    TRUE ~ "Unable to match categorical values"
  )) %>% 
  select(-values_identical) %>% 
  relocate(ID, result_type, es_type, compared, original, repro, percent_error,
           note)

# MA212 - numeric result
result_MA212_fmt1 <- result_MA212 %>% 
  mutate(ID = "MA212", es_type = "N/A", row_index = row_number()) %>% 
  mutate(result_type = case_when(
    row_index <= 12 ~ "Table 2, Match impact",
    row_index <= 24 ~ "Table 2, Match biomass",
    row_index <= 36 ~ "Table 2, No match",
    TRUE ~ NA_character_
  )) %>% 
  select(-row_index, -column_name) %>% 
  relocate(ID, result_type, es_type, compared, original, repro, percent_error,
           note)

# MA212 - character result
result_MA212_fmt2 <- result_MA212_char %>% 
  mutate(result_type = "Table 2, Overall match") %>% 
  select(-column_name) %>% 
  mutate(percent_error = NA_real_) %>% 
  mutate(note = case_when(
    values_identical == "Yes" ~ "Categorical values match exactly",
    values_identical == "No" ~ "Categorical values do not match",
    TRUE ~ "Unable to match categorical values"
  )) %>% 
  select(-values_identical) %>% 
  relocate(ID, result_type, es_type, compared, original, repro, percent_error,
           note)

# Consolidate all the results into a single data set, by type

# Numeric results
results_all_numeric <- bind_rows(result_MA016, result_MA060, result_MA062,
                                 result_MA065, result_MA067, result_MA068,
                                 result_MA071, result_MA074, result_MA081,
                                 result_MA091, result_MA092, result_MA094,
                                 result_MA095, result_MA126, result_MA129_fmt1,
                                 result_MA145, result_MA147, result_MA155_fmt1,
                                 result_MA188, result_MA191, result_MA198,
                                 result_MA202, result_MA211, result_MA212_fmt1,
                                 result_MA213, result_MA229)

# Character results
results_all_char <- bind_rows(result_MA129_fmt2, result_MA212_fmt2)



# Check data, main target results ----------------------------------------------

# Numeric cases

# Check for missing original or reproduced values
check_missing <- results_all_numeric %>% 
  filter(is.na(original) | is.na(repro))

# Check for cases where the original values is 0 (and therefore the percent
# error is undefined)
check_zero <- results_all_numeric %>% 
  filter(original == 0)

# Check for cases where the percent error is NA
check_error <- results_all_numeric %>% 
  filter(is.na(percent_error))



# Finalise data, main target results -------------------------------------------

# Make any final changes to the data set here.

# Rename the original and repro variables to include type, so that the two
# data files can be merged.
results_all_char_formerge <- results_all_char %>% 
  rename(original_chr = original,
         repro_chr = repro)
results_all_numeric_formerge <- results_all_numeric %>% 
  rename(original_num = original,
         repro_num = repro)

# Concatenate the character and numeric results together.
# Format the numerical results to follow a given format for string comparison.
# Compare the formatted string/character values.
results_all <- bind_rows(results_all_numeric_formerge,
                         results_all_char_formerge) %>% 
  arrange(ID, result_type, es_type) %>% 
  relocate(original_chr, repro_chr, .after = compared) %>% 
  mutate(original_chr = case_when(
    
    # Format count values (n) to show no decimal places.
    is.na(original_num) == FALSE &
      compared == "n" ~ sprintf("%.0f", original_num),
    
    # Format special count values to show no decimal places.
    is.na(original_num) == FALSE &
      compared %in% c("n_species", "n_studies") ~ sprintf("%.0f", original_num),
    
    # Format special count values to show no decimal places.
    is.na(original_num) == FALSE & ID == "MA212" &
      result_type %in% c("Table 2, Match biomass",
                         "Table 2, Match impact",
                         "Table 2, No match") ~ sprintf("%.0f", original_num),
    
    # Format unique comparison types to show three decimal places.
    is.na(original_num) == FALSE &
      compared %in% c("coeff_log_sla",
                      "coeff_log_wd") ~ sprintf("%.3f", original_num),
    
    # Format unique comparison types to show two decimal places.
    is.na(original_num) == FALSE &
      compared %in% c("r_squared_adj",
                      "rmse",
                      "intercept") ~ sprintf("%.2f", original_num),
    
    # Cases where all non-count values for a given MA are reported to two
    # decimal places.
    is.na(original_num) == FALSE & 
      ID %in% c("MA065", "MA068", "MA071",
                "MA081", "MA091", "MA095",
                "MA145", "MA191", "MA198",
                "MA211", "MA229") ~ sprintf("%.2f", original_num),
    
    # Cases where all non-count values for a given MA are reported to three
    # decimal places.
    is.na(original_num) == FALSE & 
      ID %in% c("MA060", "MA062", "MA074",
                "MA188", "MA202") ~ sprintf("%.3f", original_num),
    
    # Case with p-value, MA016.
    is.na(original_num) == FALSE & 
      ID == "MA016" &
      compared == "p_value" ~ paste0("< ", sprintf("%.3f", original_num)),
    is.na(original_num) == FALSE & 
      ID == "MA016" &
      compared != "p_value" ~ sprintf("%.2f", original_num),
    
    # Case with p-value, MA067.
    is.na(original_num) == FALSE & 
      ID == "MA067" &
      compared == "p_value" ~ sprintf("%.3f", original_num),
    is.na(original_num) == FALSE & 
      ID == "MA067" &
      compared == "z_value" ~ sprintf("%.1f", original_num),
    is.na(original_num) == FALSE & 
      ID == "MA067" &
      compared %in% c("point_est", "se") ~ sprintf("%.2f", original_num),
    
    # Case with p-value, MA094.
    is.na(original_num) == FALSE & 
      ID == "MA094" &
      compared == "p_value" ~ paste0("< ", sprintf("%.4f", original_num)),
    is.na(original_num) == FALSE & 
      ID == "MA094" &
      compared == "r_squared" ~ sprintf("%.3f", original_num),
    
    # Case with p-value, MA126.
    is.na(original_num) == FALSE & 
      ID == "MA126" &
      compared == "p_value" ~ sprintf("%.3f", original_num),
    is.na(original_num) == FALSE & 
      ID == "MA126" &
      compared != "p_value" ~ sprintf("%.2f", original_num),
    
    # Special case model weights, MA129.
    is.na(original_num) == FALSE &
      ID == "MA129" &
      compared %in% c("delta.AICc", "w.AICc") ~ sprintf("%.2f", original_num),
    
    # Special case inconsistent decimals, MA147.
    is.na(original_num) == FALSE & 
      ID == "MA147" &
      compared == "ci_lower" ~ sprintf("%.3f", original_num),
    is.na(original_num) == FALSE & 
      ID == "MA147" &
      compared != "ci_lower" ~ sprintf("%.2f", original_num),
    
    # Case with p-value, MA155.
    is.na(original_num) == FALSE &
      ID == "MA155" &
      compared %in% c("point_est", "p_value") ~ sprintf("%.2f", original_num),
    
    # Case with p-value, MA213.
    is.na(original_num) == FALSE & 
      ID == "MA213" &
      compared == "p_value" ~ sprintf("%.3f", original_num),
    is.na(original_num) == FALSE & 
      ID == "MA213" &
      compared != "p_value" ~ sprintf("%.2f", original_num),

    TRUE ~ original_chr
  )) %>% 
  mutate(repro_chr = case_when(
    
    # Format count values (n) to show no decimal places.
    is.na(repro_num) == FALSE &
      compared == "n" ~ sprintf("%.0f", repro_num),
    
    # Format special count values to show no decimal places.
    is.na(repro_num) == FALSE &
      compared %in% c("n_species", "n_studies") ~ sprintf("%.0f", repro_num),
    
    # Format special count values to show no decimal places.
    is.na(repro_num) == FALSE & ID == "MA212" &
      result_type %in% c("Table 2, Match biomass",
                         "Table 2, Match impact",
                         "Table 2, No match") ~ sprintf("%.0f", repro_num),
    
    # Format unique comparison types to show three decimal places.
    is.na(repro_num) == FALSE &
      compared %in% c("coeff_log_sla",
                      "coeff_log_wd") ~ sprintf("%.3f", repro_num),
    
    # Format unique comparison types to show two decimal places.
    is.na(repro_num) == FALSE &
      compared %in% c("r_squared_adj",
                      "rmse",
                      "intercept") ~ sprintf("%.2f", repro_num),
    
    # Cases where all non-count values for a given MA are reported to two
    # decimal places.
    is.na(repro_num) == FALSE & 
      ID %in% c("MA065", "MA068", "MA071",
                "MA081", "MA091", "MA095",
                "MA145", "MA191", "MA198",
                "MA211", "MA229") ~ sprintf("%.2f", repro_num),
    
    # Cases where all non-count values for a given MA are reported to three
    # decimal places.
    is.na(repro_num) == FALSE & 
      ID %in% c("MA060", "MA062", "MA074",
                "MA188", "MA202") ~ sprintf("%.3f", repro_num),
    
    # Case with p-value, MA016.
    is.na(repro_num) == FALSE &
      ID == "MA016" &
      compared == "p_value" &
      repro_num < 0.001 ~ "< 0.001",
    is.na(repro_num) == FALSE &
      ID == "MA016" &
      compared == "p_value" &
      repro_num > 0.001 ~ "> 0.001",
    is.na(repro_num) == FALSE &
      ID == "MA016" &
      compared == "p_value" &
      repro_num == 0.001 ~ sprintf("%.3f", repro_num),
    is.na(repro_num) == FALSE &
      ID == "MA016" &
      compared != "p_value" ~ sprintf("%.2f", repro_num),
    
    # Case with p-value, MA067.
    is.na(repro_num) == FALSE & 
      ID == "MA067" &
      compared == "p_value" ~ sprintf("%.3f", repro_num),
    is.na(repro_num) == FALSE & 
      ID == "MA067" &
      compared == "z_value" ~ sprintf("%.1f", repro_num),
    is.na(repro_num) == FALSE & 
      ID == "MA067" &
      compared %in% c("point_est", "se") ~ sprintf("%.2f", repro_num),
    
    # Case with p-value, MA094.
    is.na(repro_num) == FALSE &
      ID == "MA094" &
      compared == "p_value" &
      repro_num < 0.0001 ~ "< 0.0001",
    is.na(repro_num) == FALSE &
      ID == "MA094" &
      compared == "p_value" &
      repro_num > 0.0001 ~ "> 0.0001",
    is.na(repro_num) == FALSE &
      ID == "MA094" &
      compared == "p_value" &
      repro_num == 0.0001 ~ sprintf("%.4f", repro_num),
    is.na(repro_num) == FALSE &
      ID == "MA094" &
      compared == "r_squared" ~ sprintf("%.3f", repro_num),
    
    # Case with p-value, MA126.
    is.na(repro_num) == FALSE & 
      ID == "MA126" &
      compared == "p_value" ~ sprintf("%.3f", repro_num),
    is.na(repro_num) == FALSE & 
      ID == "MA126" &
      compared != "p_value" ~ sprintf("%.2f", repro_num),
    
    # Special case model weights, MA129.
    is.na(repro_num) == FALSE &
      ID == "MA129" &
      compared %in% c("delta.AICc", "w.AICc") ~ sprintf("%.2f", repro_num),
    
    # Special case inconsistent decimals, MA147.
    is.na(repro_num) == FALSE & 
      ID == "MA147" &
      compared == "ci_lower" ~ sprintf("%.3f", repro_num),
    is.na(repro_num) == FALSE & 
      ID == "MA147" &
      compared != "ci_lower" ~ sprintf("%.2f", repro_num),
    
    # Case with p-value, MA155.
    is.na(repro_num) == FALSE &
      ID == "MA155" &
      compared %in% c("point_est", "p_value") ~ sprintf("%.2f", repro_num),
    
    # Case with p-value, MA213.
    is.na(repro_num) == FALSE & 
      ID == "MA213" &
      compared == "p_value" ~ sprintf("%.3f", repro_num),
    is.na(repro_num) == FALSE & 
      ID == "MA213" &
      compared != "p_value" ~ sprintf("%.2f", repro_num),
    
    TRUE ~ repro_chr
  )) %>% 
  
  # Perform a comparison of all string values. Missing/NA repro values are
  # coded as not matching, i.e. FALSE.
  rowwise() %>% 
  mutate(match_chr = identical(x = original_chr, y = repro_chr)) %>% 
  ungroup() %>% 
  relocate(match_chr, .after = repro_chr)



# Check for cases which don't have a character value for the original or
# reproduced values.
check_chr_missing <- results_all %>% 
  filter(is.na(original_chr) | is.na(repro_chr))

# Check for cases where the identical() comparison of strings is FALSE.
check_chr_matchF <- results_all %>% 
  filter(match_chr == FALSE)

# Check cases where the string match is good, but the note (based on numeric)
# does not say exact match; these should be p-values, zeroes, etc.
check_chr_matchT_noteclash <- results_all |> 
  filter(match_chr == TRUE,
         ! note %in% c("Original and reproduced values match exactly",
                     "Categorical values match exactly"))



# Create a version of the results data which treats the six cases of irrelevant
# or partially relevant code as failures to reproduce.
# The six cases are:
# - MA016
# - MA068
# - MA092
# - MA094
# - MA155
# - MA212
#
# Of these, note that MA068 and MA094 have alternative results, so the original
# target values already show failures to reproduce.
# 
# Need to "wipe" the reproduced results from MA016, MA092, MA155, MA212.
# Need to add a flag(?) to the six non-relevant cases
results_relevantonly <- results_all |> 
  mutate(repro_chr = case_when(
    ID %in% c("MA016", "MA092", "MA155", "MA212") ~ NA_character_,
    TRUE ~ repro_chr
  ),
  match_chr = case_when(
    ID %in% c("MA016", "MA092", "MA155", "MA212") ~ FALSE,
    TRUE ~ match_chr
  ),
  repro_num = case_when(
    ID %in% c("MA016", "MA092", "MA155", "MA212") ~ NA_real_,
    TRUE ~ repro_num
  ),
  percent_error = case_when(
    ID %in% c("MA016", "MA092", "MA155", "MA212") ~ NA_real_,
    TRUE ~ percent_error
  ),
  note = case_when(
    ID %in% c("MA016", "MA092", "MA155", "MA212", "MA068",
              "MA094") ~ "Code not relevant or only partially relevant to result",
    TRUE ~ note
  ))



# Analyse data -----------------------------------------------------------------



# Get the number of target results associated with an article with irrelevant
# code.
irrelevant_target_results <- results_relevantonly |> 
  filter(note == "Code not relevant or only partially relevant to result") |> 
  group_by(ID) |> 
  count()
print(paste("Total no. target results from code-irrelevant articles: ",
            sum(irrelevant_target_results$n)))



# Get the number of target results associated with an article with relevant
# code.
relevant_target_results <- results_relevantonly |> 
  filter(note != "Code not relevant or only partially relevant to result") |> 
  group_by(ID) |> 
  count()
print(paste("Total no. target results from code-relevant articles: ",
            sum(relevant_target_results$n)))



# Table: Breakdown of repro attempts, code relevant ----------------------------

# Summarise the repro attempt results for the code-relevant dataset (where
# irrelevant code article target values are treated as effective failures).
attempt_summary_relevantonly <- results_relevantonly |> 
  mutate(attempt_result = case_when(
    match_chr == TRUE ~ "Original and reproduced values match exactly",
    note == "Original and reproduced values differ by less than 10%" ~ "Original and reproduced values differ by less than 10%",
    note == "Original and reproduced values differ by at least 10%" ~ "Original and reproduced values differ by 10% or more",
    note == "Cannot calculate result, invalid value(s)" ~ "Failed, could not calculate any value for target result",
    note == "Code not relevant or only partially relevant to result" ~ "Failed, code not relevant to target result",
    TRUE ~ "Other"
  )) |> 
  group_by(attempt_result) |> 
  count() |> 
  ungroup() |> 
  mutate(pct = 100*n/sum(n))

# Add total row
temp_total_row <- tibble::tibble(attempt_result = "Total",
                                 n = sum(attempt_summary_relevantonly$n),
                                 pct = sum(attempt_summary_relevantonly$pct))

attempt_summary_relevantonly <- bind_rows(attempt_summary_relevantonly,
                                          temp_total_row)

# Reorder rows
attempt_summary_relevantonly <- attempt_summary_relevantonly[c(5, 4, 3, 2, 1,
                                                               6), ]

# Convert this summary data to LaTeX table format for use in manuscript
tbl_attempt_summary_relonly <- xtable(attempt_summary_relevantonly,
                                      digits = c(0, 0, 0, 1),
                                      type = "latex",
                                      caption = "Breakdown of target result reproduction attempts.",
                                      label = "tab:ma_res_repro_summary_relevant")
names(tbl_attempt_summary_relonly) <- c("Outcome of target result reproduction attempt", "N", "%")
print(tbl_attempt_summary_relonly,
      file = here("analysis", "output", "table_repro_summary_relevant.tex"),
      include.rownames = FALSE)



# Get the details of the failures to reproduce (relevant code-only cases)
results_fail_relonly <- results_relevantonly |> 
  filter(note == "Cannot calculate result, invalid value(s)")



# Table: Details of repro attempts, summary effects only -----------------------

# Create a table of reproduced summary effect sizes only for use in the main
# results section.
results_esdiffs_relonly <- results_relevantonly |> 
  filter(note != "Code not relevant or only partially relevant to result",
         es_type != "N/A",
         compared == "point_est") |> 
  mutate(study = case_when(
    ID == "MA016" ~ "\\textcite{xu_variations_2017}",
    ID == "MA060" ~ "\\textcite{winternitz_patterns_2017}",
    ID == "MA062" ~ "\\textcite{grueber_intergenerational_2018}",
    ID == "MA065" ~ "\\textcite{noble_developmental_2018}",
    ID == "MA067" ~ "\\textcite{risely_migratory_2017}",
    ID == "MA068" ~ "\\textcite{ronget_causes_2017}",
    ID == "MA071" ~ "\\textcite{sievers_impacts_2017}",
    ID == "MA074" ~ "\\textcite{harts_mate_2016}",
    ID == "MA081" ~ "\\textcite{jaffe_beekeeping_2016}",
    ID == "MA091" ~ "\\textcite{lemoine_underappreciated_2016}",
    ID == "MA092" ~ "\\textcite{xu_diversity_2016}",
    ID == "MA094" ~ "\\textcite{turney_pyramids_2016}",
    ID == "MA095" ~ "\\textcite{gibert_link_2016}",
    ID == "MA126" ~ "\\textcite{anderson_plant_2016}",
    ID == "MA129" ~ "\\textcite{crouzeilles_which_2016}",
    ID == "MA145" ~ "\\textcite{moore_stress_2016}",
    ID == "MA147" ~ "\\textcite{holman_bet_2016}",
    ID == "MA155" ~ "\\textcite{strader_red_2016}",
    ID == "MA188" ~ "\\textcite{senior_overlooked_2015}",
    ID == "MA191" ~ "\\textcite{voje_scaling_2015}",
    ID == "MA198" ~ "\\textcite{pazvinas_evolutionary_2015}",
    ID == "MA202" ~ "\\textcite{mehrabi_relatedness_2015}",
    ID == "MA211" ~ "\\textcite{yuan_negative_2015}",
    ID == "MA212" ~ "\\textcite{valls_keystone_2015}",
    ID == "MA213" ~ "\\textcite{colautti_contemporary_2015}",
    ID == "MA229" ~ "\\textcite{gamfeldt_marine_2015}",
    TRUE ~ NA_character_
  )) |> 
  mutate(es_type = case_when(
    es_type == "Fisher z-transformation of correlation" ~ "Fisher \\(z\\)\\nobreakdash-transformation",
    es_type == "Hedges' d" ~ "Hedges' \\(d\\)",
    es_type == "Hedges' g" ~ "Hedges' \\(g\\)",
    es_type == "Pearson's r" ~ "Pearson's \\(r\\)",
    es_type == "Cohen's d" ~ "Cohen's \\(d\\)",
    TRUE ~ es_type
  )) |> 
select(ID, study, es_type, original_chr, repro_chr, percent_error)

# Convert this data to LaTeX table format for use in manuscript
tbl_esdiffs_relonly <- xtable(results_esdiffs_relonly,
                              digits = c(0, 0, 0, 0, 0, 0, 2),
                              align = c("l", "l", "l", "l", "r", "r", "r"),
                              type = "latex",
                              caption = "The original and reproduced values of the target summary effect sizes, for articles with relevant code.",
                              label = "tab:ma_res_esdiffs_relonly")
names(tbl_esdiffs_relonly) <- c("ID", "Study", "Effect size type", "Original", "Reproduced", "Percent error (\\%)")
print(tbl_esdiffs_relonly,
      file = here("analysis", "output", "table_repro_esdiffs_relevant.tex"),
      table.placement = "htbp",
      include.rownames = FALSE,
      sanitize.text.function=function(x){x})



# Table: Details of all repro attempts -----------------------------------------

# Make a master table of all reproduced results for the appendix, including
# code-irrelevant results, etc.
results_alldiffs_forapp <- results_all |> 
  mutate(study = case_when(
    ID == "MA016" ~ "\\textcite{xu_variations_2017}",
    ID == "MA060" ~ "\\textcite{winternitz_patterns_2017}",
    ID == "MA062" ~ "\\textcite{grueber_intergenerational_2018}",
    ID == "MA065" ~ "\\textcite{noble_developmental_2018}",
    ID == "MA067" ~ "\\textcite{risely_migratory_2017}",
    ID == "MA068" ~ "\\textcite{ronget_causes_2017}",
    ID == "MA071" ~ "\\textcite{sievers_impacts_2017}",
    ID == "MA074" ~ "\\textcite{harts_mate_2016}",
    ID == "MA081" ~ "\\textcite{jaffe_beekeeping_2016}",
    ID == "MA091" ~ "\\textcite{lemoine_underappreciated_2016}",
    ID == "MA092" ~ "\\textcite{xu_diversity_2016}",
    ID == "MA094" ~ "\\textcite{turney_pyramids_2016}",
    ID == "MA095" ~ "\\textcite{gibert_link_2016}",
    ID == "MA126" ~ "\\textcite{anderson_plant_2016}",
    ID == "MA129" ~ "\\textcite{crouzeilles_which_2016}",
    ID == "MA145" ~ "\\textcite{moore_stress_2016}",
    ID == "MA147" ~ "\\textcite{holman_bet_2016}",
    ID == "MA155" ~ "\\textcite{strader_red_2016}",
    ID == "MA188" ~ "\\textcite{senior_overlooked_2015}",
    ID == "MA191" ~ "\\textcite{voje_scaling_2015}",
    ID == "MA198" ~ "\\textcite{pazvinas_evolutionary_2015}",
    ID == "MA202" ~ "\\textcite{mehrabi_relatedness_2015}",
    ID == "MA211" ~ "\\textcite{yuan_negative_2015}",
    ID == "MA212" ~ "\\textcite{valls_keystone_2015}",
    ID == "MA213" ~ "\\textcite{colautti_contemporary_2015}",
    ID == "MA229" ~ "\\textcite{gamfeldt_marine_2015}",
    TRUE ~ NA_character_
  )) |> 
  mutate(es_type = case_when(
    es_type == "Fisher z-transformation of correlation" ~ "Fisher \\(z\\)\\nobreakdash-transformation",
    es_type == "Hedges' d" ~ "Hedges' \\(d\\)",
    es_type == "Hedges' g" ~ "Hedges' \\(g\\)",
    es_type == "Pearson's r" ~ "Pearson's \\(r\\)",
    es_type == "Cohen's d" ~ "Cohen's \\(d\\)",
    es_type == "N/A" ~ "n.a.",
    TRUE ~ es_type
  )) |> 
  mutate(value_type = case_when(
    is.na(original_num) == TRUE ~ "C",
    TRUE ~ "N"
  )) |> 
  mutate(percent_error = case_when(
    repro_chr == "< 0.001" ~ NA_real_,
    TRUE ~ percent_error
  )) |> 
  mutate(repro_type = case_when(
    match_chr == TRUE ~ "E",
    note == "Cannot calculate result, invalid value(s)" ~ "F",
    note == "Original and reproduced values differ by less than 10%" ~ "\\(<10\\%\\)",
    note == "Original and reproduced values differ by at least 10%" ~ "10\\%+",
    note == "Categorical values do not match" ~ "NC",
    TRUE ~ NA_character_
  )) |> 
  mutate(original_chr = case_when(
    original_chr == "< 0.001" ~ "\\(< 0.001\\)",
    original_chr == "< 0.0001" ~ "\\(< 0.0001\\)",
    TRUE ~ original_chr
  )) |> 
  mutate(repro_chr = case_when(
    repro_chr == "< 0.001" ~ "\\(< 0.001\\)",
    TRUE ~ repro_chr
  )) |> 
  mutate(compared = case_when(
    compared == "ci_lower" ~ "CI lower",
    compared == "ci_upper" ~ "CI upper",
    compared == "coeff_log_sla" ~ "\\(\\log(\\textrm{SLA})\\) coeff.",
    compared == "coeff_log_wd" ~ "\\(\\log(\\textrm{WD})\\) coeff.",
    compared == "delta.AICc" ~ "\\(\\Delta_{i}\\)",
    compared == "hpdi_lower" ~ "HPDI lower",
    compared == "hpdi_upper" ~ "HPDI upper",
    compared == "n" ~ "\\(N\\)",
    compared == "n_species" ~ "\\(N_{\\textrm{species}}\\)",
    compared == "n_studies" ~ "\\(N_{\\textrm{studies}}\\)",
    compared == "p_value" ~ "\\(p\\)-value",
    compared == "point_est" ~ "point est.",
    compared == "r_squared" ~ "\\(R^{2}\\)",
    compared == "r_squared_adj" ~ "\\(R^{2}_{\\textrm{adj}}\\)",
    compared == "rmse" ~ "RMSE",
    compared == "se" ~ "SE",
    compared == "w.AICc" ~ "\\(w_{i}\\)",
    compared == "z_value" ~ "\\(z\\)-score",
    TRUE ~ compared
  )) |> 
  select(ID, study, result_type, es_type, compared,
         value_type, original_chr, repro_chr, percent_error, repro_type)

check_repro_type <- results_alldiffs_forapp |> 
  filter(is.na(repro_type))

# Convert this data to LaTeX table format for use in manuscript
tbl_alldiffs_forapp <- xtable(results_alldiffs_forapp,
                              digits = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0),
                              align = c("l",
                                        "l", "l", "l", "l", "l", "l",
                                        "r", "r", "r",
                                        "l"),
                              type = "latex",
                              caption = "The original and reproduced values of all target results.",
                              label = "tab:app_ma_res_alldiffs")
names(tbl_alldiffs_forapp) <- c("ID", "Study", "Result type",
                                "Effect size type", "Target result",
                                "Value type", 
                                "Original", "Reproduced",
                                "Percent error (\\%)",
                                "Status")
print(tbl_alldiffs_forapp,
      file = here("analysis", "output", "table_repro_alldiffs_forappendix.tex"),
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.text.function=function(x){x},
      only.contents = TRUE,
      hline.after = NULL)



# Table: Summary of repro attempts, own-code -----------------------------------

# Keep only those four articles, where I wrote my own code to attempt to
# reproduce the target results: MA016, MA092, MA155, MA212
results_owncode <- results_all |> 
  filter(ID %in% c("MA016", "MA092", "MA155", "MA212"))

# Summarise the repro attempt results for the own-code dataset
attempt_summary_owncode <- results_owncode |> 
  mutate(attempt_result = case_when(
    match_chr == TRUE ~ "Original and reproduced values match exactly",
    note == "Original and reproduced values differ by less than 10%" ~ "Original and reproduced values differ by less than 10\\%",
    note == "Original and reproduced values differ by at least 10%" ~ "Original and reproduced values differ by 10\\% or more",
    note == "Cannot calculate result, invalid value(s)" ~ "Failed, could not calculate any value for target result",
    note == "Code not relevant or only partially relevant to result" ~ "Failed, code not relevant to target result",
    note == "Categorical values do not match" ~ "Original and reproduced values differ (non-numeric target result)",
    TRUE ~ "Other"
  )) |> 
  group_by(attempt_result) |> 
  count() |> 
  ungroup() |> 
  mutate(pct = 100*n/sum(n))

# Add total row to the data set
temp_total_row <- tibble::tibble(attempt_result = "Total",
                                 n = sum(attempt_summary_owncode$n),
                                 pct = sum(attempt_summary_owncode$pct))

attempt_summary_owncode <- bind_rows(attempt_summary_owncode,
                                     temp_total_row)

rm(list = c("temp_total_row"))

# Reorder rows of the data set
attempt_summary_owncode <- attempt_summary_owncode[c(4, 3, 2, 1, 5), ]

# Convert this summary data to LaTeX table format for use in manuscript
tbl_attempt_summary_owncode <- xtable(attempt_summary_owncode,
                                      digits = c(0, 0, 0, 1),
                                      type = "latex",
                                      caption = "Breakdown of the target result value reproduction attempts for the four articles with irrelevant code (MA016, MA092, MA155, and MA212). The reproduction attempts required the writing of entirely new code.",
                                      label = "tab:ma_res_repro_summary_owncode")

names(tbl_attempt_summary_owncode) <- c("Outcome of target result reproduction attempt",
                                        "N",
                                        "\\%")

print(tbl_attempt_summary_owncode,
      file = here("analysis", "output", "table_repro_summary_owncode.tex"),
      sanitize.text.function=function(x){x},
      include.rownames = FALSE,
      table.placement = "htbp")



# Import data, alternative target results --------------------------------------
result_MA068_alt <- read_csv(here("reproducibility_reports", "MA068", "output", "MA068_alt_result_comparison.csv"))
result_MA094_alt <- read_csv(here("reproducibility_reports", "MA094", "output", "MA094_alt_result_comparison.csv"))



# Table: Details of repro attempts, alternative target results -----------------
# Results are both all numeric
# Drop the "extra" blank case of the mean slope parameter
results_alt_details <- bind_rows(result_MA068_alt, result_MA094_alt) |> 
  filter(!is.na(original)) |> 
  mutate(study = case_when(
    ID == "MA068" ~ "\\textcite{ronget_causes_2017}",
    ID == "MA094" ~ "\\textcite{turney_pyramids_2016}",
    TRUE ~ NA_character_
  )) |> 
  mutate(repro_type = case_when(
    note == "Original and reproduced values match exactly" ~ "E",
    note == "Original and reproduced values differ by less than 10%" ~ "\\(<10\\%\\)",
    note == "Original and reproduced values differ by at least 10%" ~ "10\\%+",
    TRUE ~ NA_character_
  )) |> 
  mutate(compared = case_when(
    compared == "published_top" ~ "published food webs, top trophic level",
    compared == "published_intermediate" ~ "published food webs, intermediate trophic level",
    compared == "published_herbivore" ~ "published food webs, herbivore trophic level",
    compared == "published_basal" ~ "published food webs, basal trophic level",
    compared == "random_top" ~ "random food webs, top trophic level",
    compared == "random_intermediate" ~ "random food webs, intermediate trophic level",
    compared == "random_herbivore" ~ "random food webs, herbivore trophic level",
    compared == "random_basal" ~ "random food webs, basal trophic level",
    compared == "cascade_top" ~ "cascade food webs, top trophic level",
    compared == "cascade_intermediate" ~ "cascade food webs, intermediate trophic level",
    compared == "cascade_herbivore" ~ "cascade food webs, herbivore trophic level",
    compared == "cascade_basal" ~ "cascade food webs, basal trophic level",
    compared == "niche_top" ~ "niche food webs, top trophic level",
    compared == "niche_intermediate" ~ "niche food webs, intermediate trophic level",
    compared == "niche_herbivore" ~ "niche food webs, herbivore trophic level",
    compared == "niche_basal" ~ "niche food webs, basal trophic level",
    compared == "point_est" ~ "point est.",
    compared == "se" ~ "SE",
    compared == "std_dev" ~ "SD",
    compared == "n" ~ "\\(N\\)",
    TRUE ~ compared
  )) |> 
  mutate(original_char = case_when(
    es_type == "slope parameter" ~ sprintf("%.3f", original),
    compared %in% c("point est.", "SD") ~ sprintf("%.2f", original),
    TRUE ~ sprintf("%.0f", original)
  )) |> 
  mutate(repro_char = case_when(
    es_type == "slope parameter" ~ sprintf("%.3f", repro),
    compared %in% c("point est.", "SD") ~ sprintf("%.2f", repro),
    TRUE ~ sprintf("%.0f", repro)
  )) |> 
  select(ID, study, result_type, es_type, compared, original_char, repro_char,
         percent_error, repro_type)

# Convert this data to LaTeX table format for use in manuscript
tbl_alt_details <- xtable(results_alt_details,
                          digits = c(0, 0, 0, 0, 0, 0, 0, 0, 2, 0),
                          align = c("l",
                                    "l", "l", "l", "l", "l",
                                    "r", "r", "r",
                                    "l"),
                          type = "latex",
                          caption = "The original and reproduced values of all alternative target results for MA068 and MA094.",
                          label = "tab:ma_alt_repro_details")
names(tbl_alt_details) <- c("ID", "Study", "Result type",
                                "Effect size type", "Target result",
                                "Original", "Reproduced",
                                "Percent error (\\%)",
                                "Status")
print(tbl_alt_details,
      file = here("analysis", "output", "table_repro_altdetails_forappendix.tex"),
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.text.function=function(x){x},
      only.contents = TRUE,
      hline.after = NULL)



# Table: Article success rate, relevant only -----------------------------------

# Import the complete list of meta-analyses
all_mas <- read_csv(here("data", "clean", "list_of_meta_analyses.csv")) |> 
  select(SearchID) |> 
  rename("ID" = "SearchID")

# Recode the target result reproduction outcomes
forsr_relonly <- results_relevantonly |> 
  mutate(outcome = case_when(
    match_chr == TRUE ~ "1EXA",
    note == "Original and reproduced values differ by less than 10%" ~ "2L10",
    note == "Original and reproduced values differ by at least 10%" ~ "310P",
    note == "Cannot calculate result, original value is zero" ~ "4NOZ",
    note == "Cannot calculate result, invalid value(s)" ~ "5FAI",
    note == "Code not relevant or only partially relevant to result" ~ "6NOR",
    TRUE ~ "9OTH"
  )) |> 
  mutate(crit1 = case_when(
    outcome %in% c("1EXA") ~ "SUCCESS",
    TRUE ~ "FAILURE"
  ),
  crit2 = case_when(
    outcome %in% c("1EXA", "2L10") ~ "SUCCESS",
    TRUE ~ "FAILURE"
  ),
  crit3 = case_when(
    outcome %in% c("1EXA", "2L10", "310P") ~ "SUCCESS",
    TRUE ~ "FAILURE"
  ))

print(table(forsr_relonly$outcome))

#

GetArticleSuccessRate <- function(outcomes, criterion, cutoff, allids) {
  success_by_criterion <- outcomes |> 
    group_by(ID, .data[[criterion]]) |> 
    count() |> 
    group_by(ID) |> 
    mutate(pct = 100*n/sum(n)) |> 
    ungroup()
  
  #return(success_by_criterion)
  
  successful_articles <- success_by_criterion |> 
    filter(.data[[criterion]] == "SUCCESS", pct >= cutoff)
  
  #return(successful_articles)
  
  # Immediately return zero if there are no successful articles
  if (nrow(successful_articles) == 0) {
    out_tibble <- tibble(crit_used = criterion,
                         cutoff_used = cutoff,
                         n_success = 0,
                         pct_success = 0,
                         n_failure = nrow(allids),
                         pct_failure = 100)
    return(out_tibble)
  }
  
  successful_articles <- successful_articles |> 
    mutate(article_success_flag = "SUCCESS") |> 
    select(ID, article_success_flag)
  
  #return(successful_articles)
  
  all_articles <- allids |>
    left_join(successful_articles, by = "ID") |>
    mutate(article_success_flag = case_when(
      is.na(article_success_flag) == TRUE ~ "FAILURE",
      TRUE ~ article_success_flag
    )) |> 
    group_by(article_success_flag) |> 
    count() |> 
    ungroup() |> 
    mutate(pct = 100*n/sum(n))
  
  #return(all_articles)
  
  success_rate_n <- all_articles |> 
    filter(article_success_flag == "SUCCESS") |> 
    pull(n)
  success_rate_pct <- all_articles |> 
    filter(article_success_flag == "SUCCESS") |> 
    pull(pct)
  if (success_rate_pct < 100) {
    failure_rate_n <- all_articles |> 
      filter(article_success_flag == "FAILURE") |> 
      pull(n)
    failure_rate_pct <- all_articles |> 
      filter(article_success_flag == "FAILURE") |> 
      pull(pct)
  } else {
    failure_rate_n = 0
    failure_rate_pct = 0
  }
  
  out_tibble <- tibble(crit_used = criterion,
                       cutoff_used = cutoff,
                       n_success = success_rate_n,
                       pct_success = success_rate_pct,
                       n_failure = failure_rate_n,
                       pct_failure = failure_rate_pct)
  return(out_tibble)
}

sr_relonly_crit1_all <- GetArticleSuccessRate(outcomes = forsr_relonly,
                                              criterion = "crit1",
                                              cutoff = 100,
                                              allids = all_mas)

sr_relonly_crit1_maj <- GetArticleSuccessRate(outcomes = forsr_relonly,
                                              criterion = "crit1",
                                              cutoff = 50,
                                              allids = all_mas)

sr_relonly_crit2_all <- GetArticleSuccessRate(outcomes = forsr_relonly,
                                              criterion = "crit2",
                                              cutoff = 100,
                                              allids = all_mas)

sr_relonly_crit2_maj <- GetArticleSuccessRate(outcomes = forsr_relonly,
                                              criterion = "crit2",
                                              cutoff = 50,
                                              allids = all_mas)

# Format for inclusion in chapter
sr_relonly_rates <- bind_rows(sr_relonly_crit1_all,
                              sr_relonly_crit1_maj,
                              sr_relonly_crit2_all,
                              sr_relonly_crit2_maj) |> 
  mutate(result_for_article = case_when(
    crit_used == "crit1" & cutoff_used == 100 ~ "All target result values match original exactly",
    crit_used == "crit1" & cutoff_used == 50 ~ "At least 50\\% of target result values match original exactly",
    crit_used == "crit2" & cutoff_used == 100 ~ "All target result values within 10\\% of original",
    crit_used == "crit2" & cutoff_used == 50 ~ "At least 50\\% of target result values within 10\\% of original",
    TRUE ~ NA_character_
  )) |> 
  select(result_for_article, n_failure, pct_failure, n_success, pct_success)

# Convert this data to LaTeX table format for use in manuscript
tbl_sr_relonly <- xtable(sr_relonly_rates,
                          digits = c(0, 0, 0, 1, 0, 1),
                          align = c("l",
                                    "l", "r", "r", "r", "r"),
                          type = "latex",
                          caption = "",
                          label = "")
names(tbl_sr_relonly) <- c("Result for article",
                            "\\(N_\\textrm{{failure}}\\)",
                            "Failure rate (\\%)",
                            "\\(N_\\textrm{{success}}\\)",
                            "Success rate (\\%)")
print(tbl_sr_relonly,
      file = here("analysis", "output", "table_successrate_all_relevantonly.tex"),
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.text.function=function(x){x},
      only.contents = TRUE,
      hline.after = NULL)



# Table: Article success rate, relevant + own code only ------------------------

# Recode the target result reproduction outcomes
forsr_rel_own <- results_all |> 
  mutate(outcome = case_when(
    match_chr == TRUE ~ "1EXA",
    note == "Original and reproduced values differ by less than 10%" ~ "2L10",
    note == "Original and reproduced values differ by at least 10%" ~ "310P",
    note == "Cannot calculate result, original value is zero" ~ "4NOZ",
    note == "Cannot calculate result, invalid value(s)" ~ "5FAI",
    note == "Code not relevant or only partially relevant to result" ~ "6NOR",
    note == "Categorical values do not match" ~ "7NCM",
    TRUE ~ "9OTH"
  )) |> 
  mutate(crit1 = case_when(
    outcome %in% c("1EXA") ~ "SUCCESS",
    TRUE ~ "FAILURE"
  ),
  crit2 = case_when(
    outcome %in% c("1EXA", "2L10") ~ "SUCCESS",
    TRUE ~ "FAILURE"
  ),
  crit3 = case_when(
    outcome %in% c("1EXA", "2L10", "310P") ~ "SUCCESS",
    TRUE ~ "FAILURE"
  ))

print(table(forsr_rel_own$outcome))

sr_rel_own_crit1_all <- GetArticleSuccessRate(outcomes = forsr_rel_own,
                                              criterion = "crit1",
                                              cutoff = 100,
                                              allids = all_mas)

sr_rel_own_crit1_maj <- GetArticleSuccessRate(outcomes = forsr_rel_own,
                                              criterion = "crit1",
                                              cutoff = 50,
                                              allids = all_mas)

sr_rel_own_crit2_all <- GetArticleSuccessRate(outcomes = forsr_rel_own,
                                              criterion = "crit2",
                                              cutoff = 100,
                                              allids = all_mas)

sr_rel_own_crit2_maj <- GetArticleSuccessRate(outcomes = forsr_rel_own,
                                              criterion = "crit2",
                                              cutoff = 50,
                                              allids = all_mas)

# Format for inclusion in chapter
sr_rel_own_rates <- bind_rows(sr_rel_own_crit1_all,
                              sr_rel_own_crit1_maj,
                              sr_rel_own_crit2_all,
                              sr_rel_own_crit2_maj) |> 
  mutate(result_for_article = case_when(
    crit_used == "crit1" & cutoff_used == 100 ~ "All target result values match original exactly",
    crit_used == "crit1" & cutoff_used == 50 ~ "At least 50\\% of target result values match original exactly",
    crit_used == "crit2" & cutoff_used == 100 ~ "All target result values within 10\\% of original",
    crit_used == "crit2" & cutoff_used == 50 ~ "At least 50\\% of target result values within 10\\% of original",
    TRUE ~ NA_character_
  )) |> 
  select(result_for_article, n_failure, pct_failure, n_success, pct_success)

# Convert this data to LaTeX table format for use in manuscript
tbl_sr_rel_own <- xtable(sr_rel_own_rates,
                         digits = c(0, 0, 0, 1, 0, 1),
                         align = c("l",
                                   "l", "r", "r", "r", "r"),
                         type = "latex",
                         caption = "",
                         label = "")
names(tbl_sr_rel_own) <- c("Result for article",
                           "\\(N_\\textrm{{failure}}\\)",
                           "Failure rate (\\%)",
                           "\\(N_\\textrm{{success}}\\)",
                           "Success rate (\\%)")
print(tbl_sr_rel_own,
      file = here("analysis", "output", "table_successrate_all_relevant_and_owncode.tex"),
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.text.function=function(x){x},
      only.contents = TRUE,
      hline.after = NULL)



# Table: Success rate among code sharing articles only -------------------------

all_mas_wcode <- results_all |> 
  distinct(ID)

# Based on code-relevant target results only
src_relonly_crit1_all <- GetArticleSuccessRate(outcomes = forsr_relonly,
                                              criterion = "crit1",
                                              cutoff = 100,
                                              allids = all_mas_wcode)

src_relonly_crit1_maj <- GetArticleSuccessRate(outcomes = forsr_relonly,
                                              criterion = "crit1",
                                              cutoff = 50,
                                              allids = all_mas_wcode)

src_relonly_crit2_all <- GetArticleSuccessRate(outcomes = forsr_relonly,
                                              criterion = "crit2",
                                              cutoff = 100,
                                              allids = all_mas_wcode)

src_relonly_crit2_maj <- GetArticleSuccessRate(outcomes = forsr_relonly,
                                              criterion = "crit2",
                                              cutoff = 50,
                                              allids = all_mas_wcode)

# Format for inclusion in chapter
src_relonly_rates <- bind_rows(src_relonly_crit1_all,
                               src_relonly_crit1_maj,
                               src_relonly_crit2_all,
                               src_relonly_crit2_maj) |> 
  mutate(result_for_article = case_when(
    crit_used == "crit1" & cutoff_used == 100 ~ "All target result values match original exactly",
    crit_used == "crit1" & cutoff_used == 50 ~ "At least 50\\% of target result values match original exactly",
    crit_used == "crit2" & cutoff_used == 100 ~ "All target result values within 10\\% of original",
    crit_used == "crit2" & cutoff_used == 50 ~ "At least 50\\% of target result values within 10\\% of original",
    TRUE ~ NA_character_
  )) |> 
  select(result_for_article, n_failure, pct_failure, n_success, pct_success)

# Convert this data to LaTeX table format for use in manuscript
tbl_src_relonly <- xtable(src_relonly_rates,
                         digits = c(0, 0, 0, 1, 0, 1),
                         align = c("l",
                                   "l", "r", "r", "r", "r"),
                         type = "latex",
                         caption = "",
                         label = "")
names(tbl_src_relonly) <- c("Result for article",
                           "\\(N_\\textrm{{failure}}\\)",
                           "Failure rate (\\%)",
                           "\\(N_\\textrm{{success}}\\)",
                           "Success rate (\\%)")
print(tbl_src_relonly,
      file = here("analysis", "output", "table_successrate_wcode_relevantonly.tex"),
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.text.function=function(x){x},
      only.contents = TRUE,
      hline.after = NULL)



# Taking into account own code + relevant code target results
src_rel_own_crit1_all <- GetArticleSuccessRate(outcomes = forsr_rel_own,
                                              criterion = "crit1",
                                              cutoff = 100,
                                              allids = all_mas_wcode)

src_rel_own_crit1_maj <- GetArticleSuccessRate(outcomes = forsr_rel_own,
                                              criterion = "crit1",
                                              cutoff = 50,
                                              allids = all_mas_wcode)

src_rel_own_crit2_all <- GetArticleSuccessRate(outcomes = forsr_rel_own,
                                              criterion = "crit2",
                                              cutoff = 100,
                                              allids = all_mas_wcode)

src_rel_own_crit2_maj <- GetArticleSuccessRate(outcomes = forsr_rel_own,
                                              criterion = "crit2",
                                              cutoff = 50,
                                              allids = all_mas_wcode)

# Format for inclusion in chapter
src_rel_own_rates <- bind_rows(src_rel_own_crit1_all,
                               src_rel_own_crit1_maj,
                               src_rel_own_crit2_all,
                               src_rel_own_crit2_maj) |> 
  mutate(result_for_article = case_when(
    crit_used == "crit1" & cutoff_used == 100 ~ "All target result values match original exactly",
    crit_used == "crit1" & cutoff_used == 50 ~ "At least 50\\% of target result values match original exactly",
    crit_used == "crit2" & cutoff_used == 100 ~ "All target result values within 10\\% of original",
    crit_used == "crit2" & cutoff_used == 50 ~ "At least 50\\% of target result values within 10\\% of original",
    TRUE ~ NA_character_
  )) |> 
  select(result_for_article, n_failure, pct_failure, n_success, pct_success)

# Convert this data to LaTeX table format for use in manuscript
tbl_src_rel_own <- xtable(src_rel_own_rates,
                          digits = c(0, 0, 0, 1, 0, 1),
                          align = c("l",
                                    "l", "r", "r", "r", "r"),
                          type = "latex",
                          caption = "",
                          label = "")
names(tbl_src_rel_own) <- c("Result for article",
                            "\\(N_\\textrm{{failure}}\\)",
                            "Failure rate (\\%)",
                            "\\(N_\\textrm{{success}}\\)",
                            "Success rate (\\%)")
print(tbl_src_rel_own,
      file = here("analysis", "output", "table_successrate_wcode_relevant_and_owncode.tex"),
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.text.function=function(x){x},
      only.contents = TRUE,
      hline.after = NULL)



# Table: Consolidate all success rate tables -----------------------------------

sr_rel_own_rates2 <- sr_rel_own_rates |> 
  select(-c(n_failure, pct_failure)) |> 
  rename(n_allresults_allmas=n_success, pct_allresults_allmas=pct_success)

sr_relonly_rates2 <- sr_relonly_rates |> 
  select(-c(n_failure, pct_failure)) |> 
  rename(n_relonly_allmas=n_success, pct_relonly_allmas=pct_success)

src_rel_own_rates2 <- src_rel_own_rates |> 
  select(-c(n_failure, pct_failure)) |> 
  rename(n_allresults_codemas=n_success, pct_allresults_codemas=pct_success)

src_relonly_rates2 <- src_relonly_rates |> 
  select(-c(n_failure, pct_failure)) |> 
  rename(n_relonly_codemas=n_success, pct_relonly_codemas=pct_success)

successrates_all <- sr_relonly_rates2 |> 
  left_join(sr_rel_own_rates2, by = "result_for_article") |> 
  left_join(src_relonly_rates2, by = "result_for_article") |> 
  left_join(src_rel_own_rates2, by = "result_for_article") |> 
  filter(n_relonly_allmas == n_relonly_codemas,
         n_allresults_allmas == n_allresults_codemas) |> 
  select(-c(n_relonly_codemas, n_allresults_codemas)) |> 
  rename(n_relonly=n_relonly_allmas, n_allresults=n_allresults_allmas) |> 
  select(result_for_article,
         n_relonly, pct_relonly_allmas, pct_relonly_codemas,
         n_allresults, pct_allresults_allmas, pct_allresults_codemas)

# Convert this data to LaTeX table format for use in manuscript
tbl_successrates_all <- xtable(successrates_all,
                          digits = c(0, 0, 0, 1, 1, 0, 1, 1),
                          align = c("l",
                                    "l", "r", "r", "r", "r", "r", "r"),
                          type = "latex",
                          caption = "",
                          label = "")
# names(tbl_src_rel_own) <- c("Result for article",
#                             "\\(N_\\textrm{{failure}}\\)",
#                             "Failure rate (\\%)",
#                             "\\(N_\\textrm{{success}}\\)",
#                             "Success rate (\\%)")
print(tbl_successrates_all,
      file = here("analysis", "output", "table_successrate_consolidated.tex"),
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.text.function=function(x){x},
      only.contents = TRUE,
      hline.after = NULL)



# Table: Break down of target result dependence --------------------------------

# Get the results of all articles with (i) code-relevant results and (ii)
# summary effect result types.
# Rename the reproduction match types for easy sorting.
# Pivot wider so that target result types are columns.
# Keep only N, point estimate, and CI bounds.

dependence_data <- results_relevantonly |> 
  filter(note != "Code not relevant or only partially relevant to result") |> 
  filter(result_type %in% c("mean", "correlation")) |> 
  mutate(short_note = case_when(
    note == "Original and reproduced values match exactly" ~ "1Exact",
    note == "Original and reproduced values differ by less than 10%" ~ "2Within10%",
    note == "Original and reproduced values differ by at least 10%" ~ "3AtLeast10%",
    note == "Cannot calculate result, invalid value(s)" ~ "4Failure",
    TRUE ~ NA_character_
  )) |> 
  mutate(compared = case_when(
    compared == "hpdi_lower" ~ "ci_lower",
    compared == "hpdi_upper" ~ "ci_upper",
    TRUE ~ compared
  )) |> 
  pivot_wider(id_cols = ID,
              names_from = compared,
              values_from = short_note) |> 
  relocate(n, point_est, ci_lower, ci_upper, .after = ID) |> 
  arrange(n, point_est, ci_lower, ci_upper, ID) |> 
  select(ID, n, point_est, ci_lower, ci_upper)

dependence_data_fmt <- dependence_data |> 
  mutate(n = case_when(
    n == "1Exact" ~ "Exact",
    n == "2Within10%" ~ "Within 10\\%",
    n == "3AtLeast10%" ~ "At Least 10\\%",
    n == "4Failure" ~ "Failure",
    is.na(n) == TRUE ~ "n.a.",
    TRUE ~ NA_character_
  ),
  point_est = case_when(
    point_est == "1Exact" ~ "Exact",
    point_est == "2Within10%" ~ "Within 10\\%",
    point_est == "3AtLeast10%" ~ "At Least 10\\%",
    point_est == "4Failure" ~ "Failure",
    is.na(point_est) == TRUE ~ "n.a.",
    TRUE ~ NA_character_
  ),
  ci_lower = case_when(
    ci_lower == "1Exact" ~ "Exact",
    ci_lower == "2Within10%" ~ "Within 10\\%",
    ci_lower == "3AtLeast10%" ~ "At Least 10\\%",
    ci_lower == "4Failure" ~ "Failure",
    is.na(ci_lower) == TRUE ~ "n.a.",
    TRUE ~ NA_character_
  ),
  ci_upper = case_when(
    ci_upper == "1Exact" ~ "Exact",
    ci_upper == "2Within10%" ~ "Within 10\\%",
    ci_upper == "3AtLeast10%" ~ "At Least 10\\%",
    ci_upper == "4Failure" ~ "Failure",
    is.na(ci_upper) == TRUE ~ "n.a.",
    TRUE ~ NA_character_
  ))

tbl_dependence <- xtable(x = dependence_data_fmt,
                         digits = c(0,
                                    0, 0, 0, 0, 0),
                         align = c("l",
                                   "l", "l", "l", "l", "l"),
                         type = "latex",
                         caption = "",
                         label = "")

print(tbl_dependence,
      file = here("analysis", "output", "table_dependence.tex"),
      include.rownames = FALSE,
      include.colnames = FALSE,
      sanitize.text.function=function(x){x},
      only.contents = TRUE,
      hline.after = nrow(tbl_dependence))