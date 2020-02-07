FROM rocker/rstudio

# Metadata
LABEL name="bioconductor/..." \
      version="3.11.1" \
      url="https://github.com/.../" \
      maintainer="Kristian, Steffen" \
      description="Supplemental." \
      license="Artistic-2.0"

## Inspiration taken from
## https://github.com/Bioconductor/bioconductor_docker/blob/master/Dockerfile

# nuke cache dirs before installing pkgs; tip from Dirk E fixes broken img
RUN rm -f /var/lib/dpkg/available && rm -rf  /var/cache/apt/*

# issues with '/var/lib/dpkg/available' not found
# this will recreate
RUN dpkg --clear-avail

# This is to avoid the error
# 'debconf: unable to initialize frontend: Dialog'
ENV DEBIAN_FRONTEND noninteractive

# Update apt-get
RUN apt-get update \
	&& apt-get install -y --no-install-recommends apt-utils \
	&& apt-get install -y --no-install-recommends \
	## Basic deps
	zlib1g-dev

## Now add R dependencies

ADD install.R /tmp
RUN Rscript /tmp/install.R

