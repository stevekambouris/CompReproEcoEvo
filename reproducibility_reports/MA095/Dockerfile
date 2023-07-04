FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/CompileData.csv", "/home/rstudio/original/"]
COPY ["original/README_for_CompileData.csv", "/home/rstudio/original/"]
COPY ["original/analysis.R", "/home/rstudio/original/"]
COPY ["original/build.R", "/home/rstudio/original/"]
COPY ["original/data_processing.R", "/home/rstudio/original/"]
COPY ["original/figures.R", "/home/rstudio/original/"]
COPY ["original/model.R", "/home/rstudio/original/"]
COPY ["original/plots.R", "/home/rstudio/original/"]
COPY ["original/utils.R", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA095.Rmd", "/home/rstudio/"]
