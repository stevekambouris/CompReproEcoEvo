## Computationally reproducing results from meta-analyses in Ecology and Evolutionary Biology using shared code and data

# `docker_images` folder

The reproducibility reports in this repository were produced within specially-created [Docker](https://www.docker.com/) containers. The Docker images created for the reproducibility attempts were based on Docker images from the [Rocker project](https://rocker-project.org/) (see Boettiger and Eddelbuettel, 2017), which builds Docker images for running R.

The image for each reproduction attempt was based on the following layers:

- `rocker/tidyverse:3.5.0`
  - `rpmaecoevo/withpkgs`
    - `rpmaecoevo/withhelper`
      - `rpmaecoevo/reprorpt:MAxxx`

The subfolders in this folder contain the `Dockerfile` files and additional files required to build the `rpmaecoevo/withpkgs` and `rpmaecoevo/withhelper` images.

## `rpmaecoevo/withpkgs`

This image builds on the Rocker image `rocker/tidyverse:3.5.0` and installs the necessarily libraries and R packages required by all of the reproducibility attempts.

## `rpmaecoevo/withhelper`

This image builds on `rpmaecoevo/withpkgs` and installs the custom R package `reprohelper`, included in the `withhelper` subfolder. The image also creates an output folder.

(All reproducibility report Docker images are built on top of `rpmaecoevo/withhelper`, refer to the `Dockerfile` files in the subfolders of `reproducibility_reports/`.)

## References

Carl Boettiger and Dirk Eddelbuettel, An Introduction to Rocker: Docker Containers for R, _The R Journal_ (2017) 9(2), pages 527-536.