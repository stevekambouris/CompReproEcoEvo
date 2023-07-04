FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/food_web_data.csv", "/home/rstudio/original/"]
COPY ["original/null_models.R", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA094.Rmd", "/home/rstudio/"]
