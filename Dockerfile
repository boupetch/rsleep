FROM ubuntu:latest

RUN export DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true \
  && echo "tzdata tzdata/Areas select Europe" >> preseed.txt \
  && echo "tzdata tzdata/Zones/Europe select Paris" >> preseed.txt \
  && debconf-set-selections preseed.txt \
  && apt-get update \
  && apt-get install -y apt-transport-https ca-certificates gnupg2 software-properties-common \
  && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
  && add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(. /etc/os-release; echo "$UBUNTU_CODENAME")-cran35/" \
  && apt-get update \
  && apt-get install -y \
    r-base r-base-dev pandoc pandoc-citeproc texlive-full libssl-dev sshfs libcurl4-gnutls-dev libgit2-dev libssl-dev \
    libgsl-dev xorg libx11-dev libglu1-mesa-dev curl libxml2-dev libprotobuf-dev libudunits2-dev \
    libftgl2 libfreetype6-dev libcgal-dev unzip libv8-dev libjq-dev libgdal-dev protobuf-compiler python-pip python-virtualenv libmagick++-dev python3-dev  \
  && curl -fsSL https://download.docker.com/linux/$(. /etc/os-release; echo "$ID")/gpg > /tmp/dkey; apt-key add /tmp/dkey \
  && add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/$(. /etc/os-release; echo "$ID") $(lsb_release -cs) stable" \
  && apt-get update \
  && apt-get -y install docker-ce docker-compose  \
  && R -e "install.packages(c('devtools','keras'))" \
  && R -e "devtools::install_github(\"boupetch/rsleep\")" \
  && R -e "library(keras);install_keras()" \
