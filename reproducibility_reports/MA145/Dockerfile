FROM rpmaecoevo/withhelper:0.5.3

# Copy over the specific data/code files for the current article
COPY ["original/Supplementary_material.docx", "/home/rstudio/original/"]
COPY ["manual/MA145_recreated_all_species_cladogram.txt", "/home/rstudio/original/"]

# Copy over the RMarkdown report(s) for the current article
COPY ["reproducibility_report_MA145.Rmd", "/home/rstudio/"]
