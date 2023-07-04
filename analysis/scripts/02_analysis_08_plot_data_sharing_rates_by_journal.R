# 02_analysis_08_plot_data_sharing_rates_by_journal.R --------------------------
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script imports the data sharing data, and creates plots showing the
# rates of data sharing by journal, and journal policy.

# Load required libraries.
library(tidyverse)
library(here)
library(ggplot2)



# Import data and perform initial cleaning/formatting --------------------------

# Import the data and code sharing data set.
data_dcss <- read_csv(file = here("data", "clean",
                                  "data_code_sharing_status.csv"))

# Clean the data.
data_dcss_clean <- data_dcss %>% 
  mutate(Journal = case_when(
    Journal == "Ecological applications : a publication of the Ecological Society of America" ~ "Ecological Applications",
    TRUE ~ Journal
  )) %>% 
  mutate(journal_short = case_when(
    Journal == "American Naturalist" ~ "AM NAT",
    Journal == "Animal Behaviour" ~ "ANIM BEHAV",
    Journal == "Behavioral Ecology" ~ "BEHAV ECOL",
    Journal == "Behavioral Ecology and Sociobiology" ~ "BEHAV ECOL SOCIOBIOL",
    Journal == "Biological Reviews" ~ "BIOL REV",
    Journal == "Ecological Applications" ~ "ECOL APPL",
    Journal == "Ecological Monographs" ~ "ECOL MONOGR",
    Journal == "Ecology" ~ "ECOLOGY",
    Journal == "Ecology Letters" ~ "ECOL LETT",
    Journal == "Evolution" ~ "EVOLUTION",
    Journal == "Functional Ecology" ~ "FUNCT ECOL",
    Journal == "Journal of Animal Ecology" ~ "J ANIM ECOL",
    Journal == "Journal of Applied Ecology" ~ "J APPL ECOL",
    Journal == "Journal of Ecology" ~ "J ECOL",
    Journal == "Journal of Evolutionary Biology" ~ "J EVOLUTION BIOL",
    Journal == "Molecular Ecology" ~ "MOL ECOL",
    Journal == "New Phytologist" ~ "NEW PHYTOL",
    Journal == "Oecologia" ~ "OECOLOGIA",
    Journal == "Oikos" ~ "OIKOS",
    TRUE ~ NA_character_
  )) %>% 
  mutate(shared_data_flag = case_when(
    status_act_sharing %in% c("act_shared_data_only",
                              "act_shared_both_code_data") ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(shared_code_flag = case_when(
    status_act_sharing %in% c("act_shared_code_only",
                              "act_shared_both_code_data") ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(jdap_flag = case_when(
    Journal == "American Naturalist" ~ "JDAP Journals",
    Journal == "Animal Behaviour" ~ "Non-JDAP Journals",
    Journal == "Behavioral Ecology" ~ "Non-JDAP Journals",
    Journal == "Behavioral Ecology and Sociobiology" ~ "Non-JDAP Journals",
    Journal == "Biological Reviews" ~ "Non-JDAP Journals",
    Journal == "Ecological Applications" ~ "Non-JDAP Journals",
    Journal == "Ecological Monographs" ~ "Non-JDAP Journals",
    Journal == "Ecology" ~ "Non-JDAP Journals",
    Journal == "Ecology Letters" ~ "Non-JDAP Journals",
    Journal == "Evolution" ~ "JDAP Journals",
    Journal == "Functional Ecology" ~ "JDAP Journals",
    Journal == "Journal of Animal Ecology" ~ "JDAP Journals",
    Journal == "Journal of Applied Ecology" ~ "JDAP Journals",
    Journal == "Journal of Ecology" ~ "JDAP Journals",
    Journal == "Journal of Evolutionary Biology" ~ "JDAP Journals",
    Journal == "Molecular Ecology" ~ "JDAP Journals",
    Journal == "New Phytologist" ~ "Non-JDAP Journals",
    Journal == "Oecologia" ~ "Non-JDAP Journals",
    Journal == "Oikos" ~ "Non-JDAP Journals",
    TRUE ~ NA_character_
  )) %>% 
  mutate(datapolicy_flag = case_when(
    Journal == "American Naturalist" ~ "Data sharing\nrequired",
    Journal == "Animal Behaviour" ~ "Data sharing\nnot required",
    Journal == "Behavioral Ecology" ~ "Data sharing\nnot required",
    Journal == "Behavioral Ecology and Sociobiology" ~ "Data sharing\nnot required",
    Journal == "Biological Reviews" ~ "Data sharing\nnot required",
    Journal == "Ecological Applications" ~ "Data sharing\nrequired",
    Journal == "Ecological Monographs" ~ "Data sharing\nrequired",
    Journal == "Ecology" ~ "Data sharing\nrequired",
    Journal == "Ecology Letters" ~ "Data sharing\nrequired",
    Journal == "Evolution" ~ "Data sharing\nrequired",
    Journal == "Functional Ecology" ~ "Data sharing\nrequired",
    Journal == "Journal of Animal Ecology" ~ "Data sharing\nrequired",
    Journal == "Journal of Applied Ecology" ~ "Data sharing\nrequired",
    Journal == "Journal of Ecology" ~ "Data sharing\nrequired",
    Journal == "Journal of Evolutionary Biology" ~ "Data sharing\nrequired",
    Journal == "Molecular Ecology" ~ "Data sharing\nrequired",
    Journal == "New Phytologist" ~ "Data sharing\nnot required",
    Journal == "Oecologia" ~ "Data sharing\nnot required",
    Journal == "Oikos" ~ "Data sharing\nrequired",
    TRUE ~ NA_character_
  )) %>% 
  mutate(codepolicy_flag = case_when(
    Journal == "American Naturalist" ~ "Code required\nMislan et al. (2016)",
    Journal == "Animal Behaviour" ~ "Policy\nnot known",
    Journal == "Behavioral Ecology" ~ "Code not required\nMislan et al. (2016)",
    Journal == "Behavioral Ecology and Sociobiology" ~ "Code not required\nMislan et al. (2016)",
    Journal == "Biological Reviews" ~ "Policy\nnot known",
    Journal == "Ecological Applications" ~ "Code required\nMislan et al. (2016)",
    Journal == "Ecological Monographs" ~ "Code required\nMislan et al. (2016)",
    Journal == "Ecology" ~ "Code required\nMislan et al. (2016)",
    Journal == "Ecology Letters" ~ "Code not required\nMislan et al. (2016)",
    Journal == "Evolution" ~ "Code not required\nMislan et al. (2016)",
    Journal == "Functional Ecology" ~ "Code required\nMislan et al. (2016)",
    Journal == "Journal of Animal Ecology" ~ "Code required\nMislan et al. (2016)",
    Journal == "Journal of Applied Ecology" ~ "Code required\nMislan et al. (2016)",
    Journal == "Journal of Ecology" ~ "Code required\nMislan et al. (2016)",
    Journal == "Journal of Evolutionary Biology" ~ "Code not required\nMislan et al. (2016)",
    Journal == "Molecular Ecology" ~ "Code required\nMislan et al. (2016)",
    Journal == "New Phytologist" ~ "Policy\nnot known",
    Journal == "Oecologia" ~ "Code not required\nMislan et al. (2016)",
    Journal == "Oikos" ~ "Code not required\nMislan et al. (2016)",
    TRUE ~ NA_character_
  ))



# Prepare data for data sharing rates by journal -------------------------------

# Get the total number of articles in the sample per (short) journal name.
num_articles_sjnl <- data_dcss_clean %>% 
  group_by(journal_short) %>% 
  count(name = "total_articles") %>% 
  ungroup()

# Get the number of articles which shared data per (short) journal name.
num_data_shared_sjnl <- data_dcss_clean %>% 
  filter(shared_data_flag == TRUE) %>% 
  group_by(journal_short) %>% 
  count(name = "num_shared_data") %>% 
  ungroup()

pct_data_shared_sjnl <- num_articles_sjnl %>% 
  left_join(num_data_shared_sjnl, by = "journal_short") %>% 
  mutate(pct_shared_data = 100*num_shared_data/total_articles) %>% 
  arrange(pct_shared_data, journal_short) %>% 
  mutate(factor_sjnl = factor(journal_short, levels = journal_short),
         frac_label = paste0(num_shared_data, "/", total_articles))



# Plot shared data rates by journal --------------------------------------------

data_shared_jnl_plot <- ggplot(data = pct_data_shared_sjnl,
                               mapping = aes(x = factor_sjnl,
                                             y = pct_shared_data)) +
  geom_bar(stat = "identity", colour = "black", fill = "yellow3") +
  geom_text(aes(label = frac_label), hjust = 1.1) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Percentage of journal's articles in sample which shared data (%)") +
  coord_flip(ylim = c(0, 100)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank())

data_shared_jnl_plot

ggsave(filename = "figure_data_sharing_rates_by_jnl.pdf",
       plot = data_shared_jnl_plot,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)



# Prepare data for code sharing rates by journal -------------------------------

# Get the total number of articles in the sample per (short) journal name.
num_articles_sjnl <- data_dcss_clean %>% 
  group_by(journal_short) %>% 
  count(name = "total_articles") %>% 
  ungroup()

# Get the number of articles which shared code per (short) journal name.
num_code_shared_sjnl <- data_dcss_clean %>% 
  filter(shared_code_flag == TRUE) %>% 
  group_by(journal_short) %>% 
  count(name = "num_shared_code") %>% 
  ungroup()

pct_code_shared_sjnl <- num_articles_sjnl %>% 
  left_join(num_code_shared_sjnl, by = "journal_short") %>% 
  mutate(num_shared_code = tidyr::replace_na(num_shared_code, 0)) %>% 
  mutate(pct_shared_code = 100*num_shared_code/total_articles) %>% 
  arrange(pct_shared_code, journal_short) %>% 
  mutate(factor_sjnl = factor(journal_short, levels = journal_short),
         frac_label = paste0(num_shared_code, "/", total_articles))



# Plot shared data rates by journal --------------------------------------------

code_shared_jnl_plot <- ggplot(data = pct_code_shared_sjnl,
                               mapping = aes(x = factor_sjnl,
                                             y = pct_shared_code)) +
  geom_bar(stat = "identity", colour = "black", fill = "dodgerblue1") +
  geom_text(aes(label = frac_label), hjust = -0.1) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Percentage of journal's articles in sample which shared code (%)") +
  coord_flip(ylim = c(0, 100)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank())

code_shared_jnl_plot

ggsave(filename = "figure_code_sharing_rates_by_jnl.pdf",
       plot = code_shared_jnl_plot,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)



# Prepare data for data sharing rates by JDAP status ---------------------------

# Get the total number of articles in the sample per JDAP category.
num_articles_jdap <- data_dcss_clean %>% 
  group_by(jdap_flag) %>% 
  count(name = "total_articles") %>% 
  ungroup()

# Get the number of articles which shared data per JDAP category.
num_data_shared_jdap <- data_dcss_clean %>% 
  filter(shared_data_flag == TRUE) %>% 
  group_by(jdap_flag) %>% 
  count(name = "num_shared_data") %>% 
  ungroup()

pct_data_shared_jdap <- num_articles_jdap %>% 
  left_join(num_data_shared_jdap, by = "jdap_flag") %>% 
  mutate(pct_shared_data = 100*num_shared_data/total_articles) %>% 
  arrange(pct_shared_data, jdap_flag) %>% 
  mutate(factor_jdap = factor(jdap_flag, levels = jdap_flag),
         frac_label = paste0(num_shared_data, "/", total_articles))



# Plot shared data rates by JDAP status ----------------------------------------

data_shared_jdap_plot <- ggplot(data = pct_data_shared_jdap,
                                mapping = aes(x = factor_jdap,
                                              y = pct_shared_data)) +
  geom_bar(stat = "identity", colour = "black", fill = "steelblue1") +
  geom_text(aes(label = frac_label), hjust = 1.1) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Percentage of articles in sample which shared data (%)") +
  coord_flip(ylim = c(0, 100)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank())

data_shared_jdap_plot

ggsave(filename = "figure_data_sharing_rates_by_jdap.pdf",
       plot = data_shared_jdap_plot,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)




# Prepare data for data sharing rates by journal data policy -------------------

# Get the total number of articles in the sample per policy category.
num_articles_dpol <- data_dcss_clean %>% 
  group_by(datapolicy_flag) %>% 
  count(name = "total_articles") %>% 
  ungroup()

# Get the number of articles which shared data per policy category.
num_data_shared_dpol <- data_dcss_clean %>% 
  filter(shared_data_flag == TRUE) %>% 
  group_by(datapolicy_flag) %>% 
  count(name = "num_shared_data") %>% 
  ungroup()

pct_data_shared_dpol <- num_articles_dpol %>% 
  left_join(num_data_shared_dpol, by = "datapolicy_flag") %>% 
  mutate(pct_shared_data = 100*num_shared_data/total_articles) %>% 
  arrange(pct_shared_data, datapolicy_flag) %>% 
  mutate(factor_dpol = factor(datapolicy_flag, levels = datapolicy_flag),
         frac_label = paste0(num_shared_data, "/", total_articles))



# Plot shared data rates by data policy status ---------------------------------

data_shared_dpol_plot <- ggplot(data = pct_data_shared_dpol,
                                mapping = aes(x = factor_dpol,
                                              y = pct_shared_data)) +
  geom_bar(stat = "identity", colour = "black", fill = "steelblue4") +
  geom_text(aes(label = frac_label), hjust = 1.1) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Percentage of articles in sample which shared data (%)") +
  coord_flip(ylim = c(0, 100)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank())

data_shared_dpol_plot

ggsave(filename = "figure_data_sharing_rates_by_dpol.pdf",
       plot = data_shared_dpol_plot,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)



# Prepare data for code sharing rates by journal code policy -------------------

# Get the total number of articles in the sample per policy category.
num_articles_cpol <- data_dcss_clean %>% 
  group_by(codepolicy_flag) %>% 
  count(name = "total_articles") %>% 
  ungroup()

# Get the number of articles which shared data per policy category.
num_code_shared_cpol <- data_dcss_clean %>% 
  filter(shared_code_flag == TRUE) %>% 
  group_by(codepolicy_flag) %>% 
  count(name = "num_shared_code") %>% 
  ungroup()

pct_code_shared_cpol <- num_articles_cpol %>% 
  left_join(num_code_shared_cpol, by = "codepolicy_flag") %>% 
  mutate(pct_shared_code = 100*num_shared_code/total_articles) %>% 
  arrange(desc(codepolicy_flag)) %>% 
  mutate(factor_cpol = factor(codepolicy_flag, levels = codepolicy_flag),
         frac_label = paste0(num_shared_code, "/", total_articles))



# Plot shared code rates by code policy status ---------------------------------

code_shared_cpol_plot <- ggplot(data = pct_code_shared_cpol,
                                mapping = aes(x = factor_cpol,
                                              y = pct_shared_code)) +
  geom_bar(stat = "identity", colour = "black", fill = "orange3") +
  geom_text(aes(label = frac_label), hjust = -0.1) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Percentage of articles in sample which shared code (%)") +
  coord_flip(ylim = c(0, 100)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank())

code_shared_cpol_plot

ggsave(filename = "figure_code_sharing_rates_by_cpol.pdf",
       plot = code_shared_cpol_plot,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)