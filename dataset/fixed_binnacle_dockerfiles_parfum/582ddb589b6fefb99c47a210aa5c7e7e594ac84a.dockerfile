#  Base image
FROM ubuntu:16.04
# ################# METADATA ######################
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
# ################# MAINTAINER ######################
MAINTAINER Felipe da Veiga Leprevost <felipe@leprevost.com.br>
ENV DEBIAN_FRONTEND="noninteractive"
RUN mv /etc/apt/sources.list /etc/apt/sources.list.bkp \
 && bash -c 'echo -e "deb mirror://mirrors.ubuntu.com/mirrors.txt xenial main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-updates main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-backports main restricted universe multiverse\ndeb mirror://mirrors.ubuntu.com/mirrors.txt xenial-security main restricted universe multiverse\n\n" > /etc/apt/sources.list' \
 && cat /etc/apt/sources.list.bkp >> /etc/apt/sources.list \
 && cat /etc/apt/sources.list
RUN apt-get clean all \
 && apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends autotools-dev automake cmake curl grep sed dpkg fuse git wget zip openjdk-8-jre build-essential pkg-config python python-dev python-pip bzip2 ca-certificates libglib2.0-0 libxext6 libsm6 libxrender1 mercurial subversion zlib1g-dev -y \
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
#  Add user biodocker with password biodocker
RUN groupadd fuse \
 && useradd --create-home --shell /bin/bash --user-group --uid 1000 --groups sudo,fuse biodocker \
 && echo `echo "biodocker\\nbiodocker\\n" | passwd biodocker ` \
 && chown biodocker:biodocker /data \
 && chown biodocker:biodocker /config
#  give write permissions to conda folder
RUN chmod 777 -R /opt/conda/
#  Change user
USER biodocker
ENV PATH="$PATH:/opt/conda/bin"
ENV PATH="$PATH:/home/biodocker/bin"
ENV HOME="/home/biodocker"
RUN mkdir /home/biodocker/bin
RUN conda config --add channels r
RUN conda config --add channels bioconda
RUN conda upgrade conda
VOLUME ["/data", "/config"]
#  Overwrite this with 'CMD []' in a dependent Dockerfile
#  CMD ["/bin/bash"]
WORKDIR /data
