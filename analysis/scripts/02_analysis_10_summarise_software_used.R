# 02_analysis_10_summarise_software_used.R
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script imports the cleaned "software use" data set and then summarises it
# for reporting purposes, creating suitable outputs for inclusion in the LaTeX
# draft of the chapter.

# Load packages ---------------------------------------------------------------- 
library(tidyverse)
library(here)
library(xtable)


# Import the cleaned software use data -----------------------------------------
sw_summary_clean <- readr::read_csv(here::here("data", "clean",
                                               "software_used_summary.csv"))
sw_details_clean <- readr::read_csv(here::here("data", "clean",
                                               "software_used_details.csv"))



# N by mention type ------------------------------------------------------------

# Breakdown of meta-analyses mentioning the software used (summary level)
result_sum_mentions <- sw_summary_clean %>% 
  group_by(any_software_specified) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct_of_mas = round(100*n/sum(n), 1)) %>% 
  mutate(description = case_when(
    any_software_specified == "Y" ~ "Mentions of software used in article",
    any_software_specified == "S" ~ "No mentions of software in article, but mentions in supplementary material",
    any_software_specified == "N" ~ "No mentions of software in article or supplementary material",
    TRUE ~ NA_character_
  )) %>% 
  arrange(desc(any_software_specified)) %>% 
  select(description, n, pct_of_mas)



# N mentions per article -------------------------------------------------------

# Distribution of number of software packages mentioned per article.
result_mentions_per_ma <- sw_details_clean %>% 
  group_by(search_id) %>% 
  count(name = "n_mentions") %>% 
  right_join(sw_summary_clean, by = "search_id") %>% 
  mutate(n_mentions = case_when(
    is.na(n_mentions) == TRUE ~ 0L,
    TRUE ~ n_mentions
  )) %>% 
  group_by(n_mentions) %>% 
  count(name = "n_of_mas") %>% 
  mutate(pct_of_mas = round(100*n_of_mas/nrow(sw_summary_clean), 1))


# Create a histogram of the number of software mentions per article.
plot_n_sw_mentions <- ggplot(data = result_mentions_per_ma,
                             mapping = aes(x = as.factor(n_mentions),
                                           y = pct_of_mas)) +
  geom_bar(stat = "identity", colour = "black", fill = "burlywood") +
  coord_cartesian(ylim = c(0, 30)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Number of software packages mentioned",
       y = "Percentage of articles (%)") +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.x = element_blank())

plot_n_sw_mentions

ggsave(filename = "figure_software_mentions_per_article.pdf",
       plot = plot_n_sw_mentions,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)


# Distribution of number of software packages mentioned per article, excluding
# R packages.
result_mentions_norpkgs <- sw_details_clean %>% 
  filter(is_r_package == "N") %>% 
  group_by(search_id) %>% 
  count(name = "n_mentions") %>% 
  right_join(sw_summary_clean, by = "search_id") %>% 
  mutate(n_mentions = case_when(
    is.na(n_mentions) == TRUE ~ 0L,
    TRUE ~ n_mentions
  )) %>% 
  group_by(n_mentions) %>% 
  count(name = "n_of_mas") %>% 
  mutate(pct_of_mas = round(100*n_of_mas/nrow(sw_summary_clean), 1))



# Create a histogram of the number of software mentions per article, excluding
# R packages.
plot_n_sw_mentions_norpkgs <- ggplot(data = result_mentions_norpkgs,
                                     mapping = aes(x = as.factor(n_mentions),
                                                   y = pct_of_mas)) +
  geom_bar(stat = "identity", colour = "black", fill = "burlywood") +
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Number of software packages mentioned",
       y = "Percentage of articles (%)") +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.x = element_blank())

plot_n_sw_mentions_norpkgs

ggsave(filename = "figure_software_mentions_per_article_norpkgs.pdf",
       plot = plot_n_sw_mentions_norpkgs,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)


# N by software package name (no R packages) -----------------------------------

# Breakdown of non-R package software used.
result_software_nopkg <- sw_details_clean %>% 
  filter(is_r_package == "N") %>% 
  group_by(software_details) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct_of_mas = round(100*n/nrow(sw_summary_clean), 1)) %>% 
  arrange(desc(n), software_details)



# Convert data set to LaTeX table format for use in manuscript.
# Top 10 (actually 12) software packages mentioned.
tbl_software_nopkg <- xtable(result_software_nopkg[1:12, ],
                             digits = c(0, 0, 0, 1),
                             type = "latex",
                             caption = "The most commonly mentioned software packages in the sample of meta-analysis articles. The software packages here were mentioned by at least 2\\% of articles in the sample.",
                             label = "tab:ma_res_top_software_mentions")
names(tbl_software_nopkg) <- c("Name of software package", "N", "%")
print(tbl_software_nopkg,
      file = here("analysis", "output", "table_software_used_norpkgs_top12.tex"),
      include.rownames = FALSE)



# Convert data set to LaTeX table format for use in manuscript.
# All software packages mentioned.
tbl_software_nopkg_all <- xtable(result_software_nopkg,
                                 digits = c(0, 0, 0, 1),
                                 type = "latex",
                                 caption = "All software packages mentioned in the 177 meta-analysis articles. Note that this table does not list individual R packages.",
                                 label = "tab:app_ma_res_all_software_mentions")
names(tbl_software_nopkg_all) <- c("Name of software package", "N", "%")
print(tbl_software_nopkg_all,
      file = here("analysis", "output", "table_software_used_norpkgs_all.tex"),
      include.rownames = FALSE)



# Breakdown of R packages used -------------------------------------------------

# Get number of R-using articles
n_using_r <- result_software_nopkg %>% 
  filter(software_details == "R") %>% pull(n)



# Get number of R-using articles mentioning at least one package
#
# This result is needed for use in paragraph text, no table/figure needed
r_using_articles <- sw_details_clean %>% 
  filter(software_details == "R") %>% 
  distinct(search_id, r_packages_mentioned)

result_mentions_rpkgs <- r_using_articles %>% 
  group_by(r_packages_mentioned) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct_of_mas_with_rpkgs = round(100*n/n_using_r, 1)) %>% 
  arrange(desc(n))



# Count mentions of each R package, calc as pct of number of R-using articles
result_software_rpkgs <- sw_details_clean %>% 
  filter(is_r_package == "Y") %>% 
  group_by(software_details, package_location) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct_of_mas_using_r = round(100*n/n_using_r, 1)) %>% 
  arrange(desc(n), software_details)

# Convert data set to LaTeX table format for use in manuscript, keep top 13
# (> 2% of total) R packages mentioned. Drop column with package locations,
# since in this case they are all CRAN
tbl_software_rpkgs <- xtable(result_software_rpkgs[1:13,
                                                   c("software_details",
                                                     "n",
                                                     "pct_of_mas_using_r")],
                             digits = c(0, 0, 0, 1),
                             type = "latex",
                             caption = "The most commonly mentioned R packages in the sample of meta-analysis articles which mentioned using R. The R packages here were mentioned by at least 2\\% of the R-using articles in the sample.",
                             label = "tab:ma_res_top_rpkg_mentions")
names(tbl_software_rpkgs) <- c("Name of R package", "N", "%")
print(tbl_software_rpkgs,
      file = here("analysis", "output", "table_rpkgs_used_top13.tex"),
      include.rownames = FALSE)



# Convert full data set to LaTeX table format for use in manuscript, keeping
# all R packages mentioned
tbl_software_rpkgs_all <- xtable(result_software_rpkgs,
                                 digits = c(0, 0, 0, 0, 1),
                                 type = "latex",
                                 caption = "All R packages mentioned in the sample of 141 meta-analysis articles which mentioned using R.",
                                 label = "tab:app_ma_res_all_rpkg_mentions")
names(tbl_software_rpkgs_all) <- c("Name of R package", "Package source", "N", "%")
print(tbl_software_rpkgs_all,
      file = here("analysis", "output", "table_rpkgs_used_all.tex"),
      include.rownames = FALSE)



# Calculate number of R package mentions per R-using article
result_rpkgs_perarticle <- sw_details_clean %>% 
  filter(is_r_package == "Y") %>% 
  group_by(search_id) %>% 
  count(name = "n_pkgs_mentioned") %>% 
  right_join(r_using_articles, by = "search_id") %>% 
  mutate(n_pkgs_mentioned = case_when(
    is.na(n_pkgs_mentioned) == TRUE ~ 0L,
    TRUE ~ n_pkgs_mentioned
  )) %>% 
  group_by(n_pkgs_mentioned) %>% 
  count(name = "n_of_mas") %>% 
  mutate(pct_of_mas = round(100*n_of_mas/n_using_r, 1))



# Create histogram of R package mentions per R-using article
plot_rpkgs_perarticle <- ggplot(data = result_rpkgs_perarticle,
                                mapping = aes(x = as.factor(n_pkgs_mentioned),
                                              y = pct_of_mas)) +
  geom_bar(stat = "identity", colour = "black", fill = "lavender") +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Number of R packages mentioned",
       y = "Percentage of articles mentioning R (%)") +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.x = element_blank())

plot_rpkgs_perarticle

ggsave(filename = "figure_rpkgs_per_article.pdf",
       plot = plot_rpkgs_perarticle,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)



# R packages by repository -----------------------------------------------------
#
# This result is needed to be referred to in the paragraph text, no table or
# figure needed.

n_rpkgs_mentioned <- result_software_rpkgs %>% 
  nrow()

result_rpkgs_byrepo <- result_software_rpkgs %>% 
  group_by(package_location) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct_of_mas_using_r = round(100*n/n_rpkgs_mentioned, 1)) %>% 
  arrange(desc(n), package_location)

check_other <- sw_details_clean |> 
  filter(package_location == "other")



# Software package citations (Non-R) -------------------------------------------
#
# These results are just to be mentioned in the paragraph text, no need for a
# figure/table.

# Get the non-R software packages only
sw_details_clean_nonR <- sw_details_clean |> 
  filter(software_details != "R", is_r_package == "N")

result_prop_citations_nonR <- sw_details_clean_nonR %>% 
  group_by(software_cited) %>% 
  count() %>% 
  mutate(pct = round(100*n/nrow(sw_details_clean_nonR), 1))

check_noncitations <- sw_details_clean_nonR %>% 
  filter(software_cited == "N") %>% 
  group_by(software_details) %>% 
  count()



# Software version specifications (Non-R) --------------------------------------
#
# These results are just to be mentioned in the paragraph text, no need for a
# figure/table.

result_prop_versions_nonR <- sw_details_clean_nonR |> 
  group_by(version_specified) |> 
  count() |> 
  mutate(pct = round(100*n/nrow(sw_details_clean_nonR), 1))



# Citations and versions (R, not packages) -------------------------------------

# Get the R-and-package mentions only
sw_details_clean_R <- sw_details_clean |> 
  filter(software_details == "R" | is_r_package == "Y")

# Calculate the rate at which articles cited the R software environment
result_prop_citations_r <- sw_details_clean_R |> 
  filter(software_details == "R") |> 
  group_by(software_cited) |> 
  count() |> 
  mutate(pct_using_r = round(100*n/n_using_r, 1))

# Calculate the rate at which articles included the version of the R software
# environment used.
result_prop_versions_r <- sw_details_clean_R |> 
  filter(software_details == "R") |> 
  group_by(version_specified) |> 
  count() |> 
  mutate(pct_using_r = round(100*n/n_using_r, 1))



# Citations and versions (R packages) ------------------------------------------

n_mentions_rpkgs <- sw_details_clean_R |> 
  filter(is_r_package == "Y") |> 
  nrow()

# Calculate the rate at which articles cited R packages
result_prop_citations_rpkgs <- sw_details_clean_R |> 
  filter(is_r_package == "Y") |> 
  group_by(software_cited) |> 
  count() |> 
  mutate(pct_using_r = round(100*n/n_mentions_rpkgs, 1))

# Calculate the rate at which articles included the version of the R package
result_prop_versions_rpkgs <- sw_details_clean_R |> 
  filter(is_r_package == "Y") |> 
  group_by(version_specified) |> 
  count() |> 
  mutate(pct_using_r = round(100*n/n_mentions_rpkgs, 1))



# Table of R versions mentioned ------------------------------------------------
result_breakdown_versions_r_all <- sw_details_clean_R |> 
  filter(software_details == "R") |> 
  group_by(version_details) |> 
  count() |> 
  ungroup() |> 
  mutate(version_details = case_when(
    is.na(version_details) ~ "(No version mentioned)",
    TRUE ~ version_details
  )) |> 
  mutate(pct_using_r = round(100*n/n_using_r, 1))

# Convert full data set to LaTeX table format for use in manuscript, keeping
# all R versions mentioned
tbl_breakdown_versions_r_all <- xtable(result_breakdown_versions_r_all,
                                       digits = c(0, 0, 0, 1),
                                       type = "latex",
                                       caption = "All R versions as originally mentioned in the sample of 141 meta-analysis articles which mentioned using R.",
                                       label = "tab:app_ma_res_all_rvers_mentions")
names(tbl_breakdown_versions_r_all) <- c("R version", "N", "%")
print(tbl_breakdown_versions_r_all,
      file = here("analysis", "output", "table_rvers_mentioned_all.tex"),
      include.rownames = FALSE)
