# Dockerfile for building docker image rpmaecoevo/withpkgs:0.5.0
# This image builds directly on the rocker image (available independently of
# this project).
FROM rocker/tidyverse:3.5.0

RUN apt-get update -qq && \
    apt-get install -y openjdk-8-jdk && \
    apt-get install -y liblzma-dev && \
    apt-get install -y libbz2-dev && \
    apt-get install -y libpoppler-cpp-dev && \
    apt-get install -y libglu1-mesa-dev && \
    apt-get install -y freeglut3-dev && \
    apt-get install -y mesa-common-dev

RUN R -e "options(repos = \
    list(CRAN = 'https://mran.microsoft.com/snapshot/2018-07-02')); \
    install.packages(pkgs = c('ade4', 'AICcmodavg', 'ape', 'bibtex', \
    'car', 'corpcor', 'docxtractr', 'downloader', 'effects', 'ggthemes', \
    'glmulti', 'lme4', 'lmerTest', 'lmodel2', 'matrixcalc', 'MCMCglmm', \
    'meta', 'metafor', 'mice', 'MuMIn', 'orthopolynom', 'phytools', \
    'plotMCMC', 'rotl', 'shape', 'smatr', 'striprtf', 'truncnorm', \
    'VIM', 'xlsx', 'here', 'officer', 'pdftools', 'foodweb'))"

RUN R -e "BiocInstaller::biocLite('metahdep', suppressUpdates = TRUE)"

RUN mkdir home/rstudio/original
