# Dockerfile for building docker image rpmaecoevo/withhelper:0.5.3
# This image performs additional setup for repro reports
# It is based on image rpmaecoevo/withpkgs
FROM rpmaecoevo/withpkgs:0.5.0

# Create a dedicated output folder that can be shared with the host
# Change the folder's owner to the rstudio user
RUN mkdir /home/rstudio/output
RUN chown rstudio: /home/rstudio/output/

# Copy over the helper functions package files
COPY reprohelper/ /home/rstudio/code/reprohelper

# Install the helper function package in R
RUN R -e "install.packages(pkgs = '/home/rstudio/code/reprohelper', \
    repos = NULL, \
    type = 'source')"
