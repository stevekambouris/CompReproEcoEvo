FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/Code.R", "/home/rstudio/original/"]
COPY ["original/Data.Objects.Rdata", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA188.Rmd", "/home/rstudio/"]
