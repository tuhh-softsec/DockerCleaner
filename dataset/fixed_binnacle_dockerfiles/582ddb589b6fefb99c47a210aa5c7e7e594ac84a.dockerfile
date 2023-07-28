#   Base image
FROM ubuntu:16.04
#  ################# METADATA ######################
LABEL base_image="ubuntu:16.04"
LABEL version="5"
LABEL software="biocontainers"
LABEL software.version="1.0.0"
LABEL about.summary="Base image for BioDocker"
LABEL about.home="http://biocontainers.pro"
LABEL about.documentation="https://github.com/BioContainers/specs/wiki"
LABEL about.license_file="https://github.com/BioContainers/containers/blob/master/LICENSE"
LABEL about.license="SPDX:Apache-2.0"
LABEL about.tags="Genomics,Proteomics,Transcriptomics,General,Metabolomics"
#  ################# MAINTAINER ######################
MAINTAINER Felipe da Veiga Leprevost <felipe@leprevost.com.br>
ENV DEBIAN_FRONTEND="noninteractive"
RUN mv /etc/apt/sources.list /etc/apt/sources.list.bkp \
 && bash -c 'echo -e "deb mirror://mirrors.ubuntu.com/mirrors.txt xenial main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-updates main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-backports main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-security main restricted universe multiverse\n\n" > /etc/apt/sources.list' \
 && cat /etc/apt/sources.list.bkp >> /etc/apt/sources.list \
 && cat /etc/apt/sources.list
RUN apt-get clean all \
 && apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends autotools-dev=20150820.1 automake=1:1.15-4ubuntu1 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 grep=2.25-1~16.04.1 sed=4.2.2-7 dpkg=1.18.4ubuntu1.7 fuse=2.9.4-1ubuntu3.1 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 zip=3.0-11 openjdk-8-jre=8u292-b10-0ubuntu1~16.04.1 build-essential=12.1ubuntu2 pkg-config=0.29.1-0ubuntu1 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 libglib2.0-0=2.48.2-0ubuntu4.8 libxext6=2:1.3.3-1 libsm6=2:1.2.2-1 libxrender1=1:0.9.9-0ubuntu1 mercurial=3.7.3-1ubuntu1.2 subversion=1.9.3-2ubuntu1.3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda2-4.0.5-Linux-x86_64.sh -O ~/miniconda.sh \
 && /bin/bash ~/miniconda.sh -b -p /opt/conda \
 && rm ~/miniconda.sh
RUN TINI_VERSION=`curl https://github.com/krallin/tini/releases/latest | grep -o "/v.*\\"" | sed 's:^..\\(.*\\).$:\\1:' ` \
 && curl -L "https://github.com/krallin/tini/releases/download/v${TINI_VERSION}/tini_${TINI_VERSION}.deb" > tini.deb \
 && dpkg -i tini.deb \
 && rm tini.deb \
 && apt-get clean
RUN mkdir /data /config
#   Add user biodocker with password biodocker
RUN groupadd fuse \
 && useradd --create-home --shell /bin/bash --user-group --uid 1000 --groups sudo,fuse biodocker \
 && echo `echo "biodocker\\nbiodocker\\n" | passwd biodocker ` \
 && chown biodocker:biodocker /data \
 && chown biodocker:biodocker /config
#   give write permissions to conda folder
RUN chmod 777 -R /opt/conda/
#   Change user
USER biodocker
ENV PATH="$PATH:/opt/conda/bin"
ENV PATH="$PATH:/home/biodocker/bin"
ENV HOME="/home/biodocker"
RUN mkdir /home/biodocker/bin
RUN conda config --add channels r
RUN conda config --add channels bioconda
RUN conda upgrade conda
VOLUME ["/data", "/config"]
#   Overwrite this with 'CMD []' in a dependent Dockerfile
#   CMD ["/bin/bash"]
WORKDIR /data
# Please add your HEALTHCHECK here!!!
