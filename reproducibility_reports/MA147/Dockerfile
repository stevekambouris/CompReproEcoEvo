FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/R scripts and dataset information/Holman_bet_hedging_-_functions.R", "/home/rstudio/original/"]
COPY ["original/R scripts and dataset information/Holman_bet_hedging_-_open_this_one_to_run_the_analysis.R", "/home/rstudio/original/"]
COPY ["original/R scripts and dataset information/Dataset_info.csv", "/home/rstudio/original/R scripts and dataset information/"]
COPY ["original/Results of analyses/Varying N analyses/*.csv", "/home/rstudio/original/Results of analyses/Varying N analyses/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA147.Rmd", "/home/rstudio/"]
