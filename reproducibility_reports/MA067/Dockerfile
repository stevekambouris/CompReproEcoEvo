FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/Rcode.R", "/home/rstudio/original/"]
COPY ["original/Supplementary file 3.csv", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA067.Rmd", "/home/rstudio/"]
