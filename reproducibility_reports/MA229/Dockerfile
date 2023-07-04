FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/gamfeldt appendix 1ny.txt", "/home/rstudio/original/"]
COPY ["original/gamfeldt appendix 2ny.txt", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA229.Rmd", "/home/rstudio/"]
