FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/corMLPE.R", "/home/rstudio/original/"]
COPY ["original/DMA_rd_roads.csv", "/home/rstudio/original/"]
COPY ["original/DMA-May2015.csv", "/home/rstudio/original/"]
COPY ["original/Full_Script.R", "/home/rstudio/original/"]
COPY ["original/mavg.R", "/home/rstudio/original/"]
COPY ["original/MLPElikelihood_take2.cpp", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA081.Rmd", "/home/rstudio/"]
