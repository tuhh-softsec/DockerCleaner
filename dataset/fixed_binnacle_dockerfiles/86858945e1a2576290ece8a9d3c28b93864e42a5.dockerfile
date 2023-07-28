#  #### bioconda: https://hub.docker.com/r/bioconda/bioconda-builder/~/dockerfile/ #####
FROM centos:centos5
#   add tools useful for compilation
RUN rpm -Uvh http://dl.fedoraproject.org/pub/epel/5/x86_64/epel-release-5-4.noarch.rpm
#   Install wget first so we can download devtools-2 and autotools repos
RUN yum install -y wget \
 && yum clean all
RUN wget http://people.centos.org/tru/devtools-2/devtools-2.repo -O /etc/yum.repos.d/devtools-2.repo
RUN yum install -y bzip2 git gcc gcc-c++ patch make gcc44 gcc44-c++ cmake unzip byacc devtoolset-2-gcc devtoolset-2-binutils devtoolset-2-gcc-c++ devtoolset-2-gcc-gfortran autotools-latest pkgconfig which file gpg db4-devel xorg-x11-apps mesa-libGLU-devel \
 && yum clean all
#   install conda
RUN mkdir -p /tmp/conda-build \
 && wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh \
 && bash Miniconda3-latest-Linux-x86_64.sh -b -p /anaconda
ENV PATH="/opt/rh/devtoolset-2/root/usr/bin:/opt/rh/autotools-latest/root/usr/bin:/anaconda/bin:$PATH"
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
RUN mkdir -p /anaconda/conda-bld/linux-64 /anaconda/conda-bld/osx-64
#   setup conda
COPY requirements.txt requirements.txt
RUN conda update conda
RUN conda install -y --file requirements.txt
RUN conda update conda-build
RUN conda index /anaconda/conda-bld/linux-64 /anaconda/conda-bld/osx-64
RUN conda config --add channels bioconda
RUN conda config --add channels r
RUN conda config --add channels file://anaconda/conda-bld
RUN conda install -y toposort
#   setup entrypoint (assuming that repo is mounted under /bioconda-recipes)
ENTRYPOINT ["/bioconda-recipes/scripts/build-packages.py"]
CMD []
#  #### vcftools: https://hub.docker.com/r/biocontainers/vcftools/~/dockerfile/ #####
#   Base Image
FROM biocontainers/biocontainers:latest
#   Metadata
LABEL base.image="biocontainers:latest"
LABEL version="1"
LABEL software="vcftools"
LABEL software.version="0.1.14"
LABEL description="A set of tools written in Perl and C++ for working with VCF files, such as those generated by the 1000 Genomes Project"
LABEL website="https://github.com/vcftools/vcftools|https://vcftools.github.io/index.html"
LABEL documentation="https://github.com/vcftools/vcftools|https://vcftools.github.io/index.html"
LABEL license="https://github.com/vcftools/vcftools|https://vcftools.github.io/index.html"
LABEL tags="Genomics"
#   Maintainer
MAINTAINER Saulo Alves Aflitos <sauloal@gmail.com>
USER root
ENV ZIP="vcftools-0.1.14.tar.gz"
ENV URL="https://github.com/vcftools/vcftools/releases/download/v0.1.14/"
ENV FOLDER="vcftools-0.1.14"
ENV DST="/tmp"
RUN wget $URL/$ZIP -O $DST/$ZIP \
 && tar xvf $DST/$ZIP -C $DST \
 && rm $DST/$ZIP \
 && cd $DST/$FOLDER \
 && ./configure \
 && make \
 && make install \
 && cd / \
 && rm -rf $DST/$FOLDER
USER biodocker
WORKDIR /data/
#  #### vcfanno: http://brentp.github.io/vcfanno/#installation #####
#  RUN conda install -c bioconda vcfanno
RUN wget https://github.com/brentp/vcfanno/releases/download/v0.2.6/vcfanno_linux64 \
 && cp vcfanno_linux64 /usr/local/bin/vcfanno_linux64 \
 && rm vcfanno_linux64
#  #### vcflib: https://hub.docker.com/r/itsjeffreyy/vcflib/~/dockerfile/ #####
#   Base image ubuntu:16.04
FROM ubuntu:16.04
#   Author
MAINTAINER Jeffreyy Chun-Hui Yu
#   install the system requirement
RUN apt-get update --fix-missing -yq \
 && apt-get install --no-install-recommends wget g++ gcc make bzip2 git autoconf automake make g++ gcc build-essential zlib1g-dev libgsl0-dev curl git wget unzip tabix libncurses5-dev -q -y
WORKDIR /opt
#   install vcflib
RUN git clone --recursive https://github.com/vcflib/vcflib.git \
 && cd vcflib \
 && make
ENV PATH="/opt/vcflib/bin:$PATH"
#  clean tar balls
RUN rm -rf /var/lib/apt/lists/* \
 && apt-get autoremove -y
#   set path
WORKDIR /root 
# Please add your HEALTHCHECK here!!!
