FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/Data_S1.csv", "/home/rstudio/original/"]
COPY ["original/Data_S2.R", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA091.Rmd", "/home/rstudio/"]
