# Computationally reproducing results from meta-analyses in Ecology and Evolutionary Biology using shared code and data

## Overview

This is a repository containing the data and code for reproducing results from _Computationally reproducing results from meta-analyses in Ecology and Evolutionary Biology using shared code and data_ by Steven Kambouris, David P. Wilkinson, Eden T. Smith, and Fiona Fidler (2023).

## Repository contents

The repository contains the following folders:
- data
- analysis
- docker_images
- reproducibility_reports

### `data` folder

The `data` folder contains the raw data files that are the basis of the study (literature search results, coding data files), the cleaned data files used for analysis, and the R scripts used to clean the raw data.

(Note that the `data/raw/` folder contains a readme file which details where to obtain two data files regarding journals' code sharing policies from other articles which are not included in this repository.)

### `analysis` folder

The `analysis` folder contains the R scripts used to calculate the results reported in the paper, and the results files output by those scripts.

### `docker_images` folder

The `docker_images` folder contains the Dockerfiles and other files required to build the Docker images used in this study to contain the reproducibility report for each meta-analysis article.

Refer to the readme.md file in this folder for more details of the Docker images.

### `reproducibility_reports` folder

The `reproducibility_reports` contains 26 reproducibility reports in individual subfolders for the articles with shared data and code investigated in this study. In addition to the reports (in HTML format) and output data (in CSV format), the folder contains the Dockerfiles and RMarkdown files required to build the Docker images and re-run the reproducibility reports.

## Data availability and provenance

Note that the original shared code and data files obtained for the 26 articles and which are required to successfully run the reproducibility reports are **not included in this repository**. _The original shared data and code files for the articles would need to be independently downloaded from their original locations and copied into the appropriate folder in order to re-run a reproducibility report._

The `original` folder in each reproducibility report's subfolder (inside `reproducibility_reports`) contains details of the original data and code files required for the reproducing the target results.

## Licence

The files in this dataset are licensed under the Creative Commons Attribution 4.0 International License (to view a copy of this license, visit [http://creativecommons.org/licenses/by/4.0/](http://creativecommons.org/licenses/by/4.0/)).

## Contact

**Name:** Steven Kambouris

**ORCID:** [0000-0002-3876-7472](https://orcid.org/0000-0002-3876-7472)

**Institution:** The University of Melbourne

**Email:** s.kambouris AT student.unimelb.edu.au
