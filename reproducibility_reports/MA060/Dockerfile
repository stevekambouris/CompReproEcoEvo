FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/human.primate.matechoice.csv", "/home/rstudio/original/"]
COPY ["original/primate_tree.nex", "/home/rstudio/original/"]
COPY ["original/Winternitz_et_al_Rcode_MHCDissimilarity.R", "/home/rstudio/original/"]

# Copy over the RMarkdown report for the current article
COPY ["reproducibility_report_MA060.Rmd", "/home/rstudio/"]
