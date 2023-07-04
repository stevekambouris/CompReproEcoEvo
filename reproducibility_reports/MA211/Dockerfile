FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/Yuan_and_Chen_data.csv", "/home/rstudio/original/"]
COPY ["original/Yuan_and_Chen_R_codes.R", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA211.Rmd", "/home/rstudio/"]
