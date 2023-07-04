FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/analysis_TincMeta.R", "/home/rstudio/original/"]
COPY ["original/func.R", "/home/rstudio/original/"]
COPY ["original/inc.csv", "/home/rstudio/original/"]
COPY ["original/phylo_matrices", "/home/rstudio/original/"]
COPY ["original/Sinv_matrices", "/home/rstudio/original/"]
COPY ["original/tincdat_fin.csv", "/home/rstudio/original/"]
COPY ["original/tincNoInc.csv", "/home/rstudio/original/"]
COPY ["original/tree_fin.txt", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA065.Rmd", "/home/rstudio/"]
