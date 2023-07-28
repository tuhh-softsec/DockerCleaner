#   MG-RAST pipeline Dockerfile
FROM ubuntu
MAINTAINER The MG-RAST team (folker@mg-rast.org)
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install --no-install-recommends cdbfasta=1.00+git20181005.014498c+dfsg-4build1 cd-hit=4.8.1-4 cmake=3.25.1-1 dh-autoreconf=20 emacs=1:28.2+1-13ubuntu3 git=1:2.39.2-1ubuntu1 libtbb-dev=2021.8.0-1ubuntu2 libcwd-guard-perl=0.05-4 libberkeleydb-perl=0.64-2 libdata-dump-streamer-perl=2.41-1 libdatetime-perl=2:1.59-1 libdatetime-format-iso8601-perl=0.16-2 libdbi-perl=1.643-4 libdigest-md5-perl libdigest-md5-file-perl=0.08-1.1 libdbd-pg-perl=3.16.0-2build1 libfile-slurp-perl=9999.32-2 libfilehandle-fmode-perl=0.14-3build1 libgetopt-long-descriptive-perl=0.111-1 libjson-perl=4.10000-1 liblist-allutils-perl=0.19-1 libpq-dev=15.2-1 libpng-dev=1.6.39-2 libposix-strptime-perl=0.13-2build1 libstring-random-perl=1:0.32-2 libtemplate-perl=2.27-1build7 liburi-encode-perl=1.1.1-3 libunicode-escape-perl=0.0.2-5 libwww-perl=6.67-1 liblog-log4perl-perl=1.57-1 libcapture-tiny-perl=0.48-2 make=4.3-4.1build1 nodejs=18.13.0+dfsg1-1ubuntu2 python-biopython python-dev python-leveldb perl-modules python-numpy python-pika python-pip python-scipy python-sphinx unzip=6.0-27ubuntu1 wget=1.21.3-1ubuntu1 vim=2:9.0.1000-4ubuntu2 curl=7.88.1-7ubuntu1 -y \
 && apt-get clean
#  ## alphabetically sorted builds from source
#  ## install bowtie2 2.3.5
RUN cd /root \
 && wget -O bowtie2.zip https://github.com/BenLangmead/bowtie2/releases/download/v2.3.5/bowtie2-2.3.5-linux-x86_64.zip \
 && unzip bowtie2.zip \
 && rm -f bowtie2.zip \
 && cd bowtie2-* \
 && install bowtie2* /usr/local/bin/ \
 && cd /root \
 && rm -rf bowtie2*
#  ## install autoskewer (requires bowtie)
RUN cd /root \
 && git clone http://github.com/MG-RAST/autoskewer \
 && cd autoskewer \
 && make install \
 && cd /root \
 && rm -rf autoskewer
#  ## install DIAMOND
RUN cd /root \
 && git clone https://github.com/bbuchfink/diamond.git \
 && cd diamond \
 && sh ./build_simple.sh \
 && install -s -m555 diamond /usr/local/bin \
 && cd /root \
 && rm -rf diamond
#  ## install ea-utils
RUN cd /root \
 && git clone https://github.com/ExpressionAnalysis/ea-utils.git \
 && cd ea-utils/clipper \
 && make fastq-multx \
 && make fastq-join \
 && make fastq-mcf \
 && install -m755 -s fastq-multx /usr/local/bin \
 && install -m755 -s fastq-join /usr/local/bin \
 && install -m755 -s fastq-mcf /usr/local/bin \
 && cd /root ; rm -rf ea-utils
#  ## install FragGeneScan from our patched source in github
RUN cd /root \
 && git clone https://github.com/MG-RAST/FGS.git FragGeneScan \
 && cd FragGeneScan \
 && make \
 && mkdir bin \
 && mv train bin/. \
 && mv *.pl bin/. \
 && cp -r bin/train /usr/local/bin/ \
 && install -s -m555 FragGeneScan /usr/local/bin/. \
 && install -m555 -t /usr/local/bin/. bin/*.pl \
 && make clean \
 && cd /root ; rm -rf FragGeneScan
#  ## install jellyfish 2.2.6 from source (2.2.8 from repo is broken)
RUN cd /root \
 && wget -O jellyfish.tar.gz https://github.com/gmarcais/Jellyfish/releases/download/v2.2.6/jellyfish-2.2.6.tar.gz \
 && tar xfvz jellyfish.tar.gz \
 && rm -f jellyfish.tar.gz \
 && cd jelly* \
 && ./configure \
 && make install \
 && cd /root RUN cd /root \
 && wget -O Prodigal.tar.gz https://github.com/hyattpd/Prodigal/archive/v2.6.3.tar.gz \
 && tar xf Prodigal.tar.gz \
 && cd Prodigal* \
 && make \
 && make install \
 && strip /usr/local/bin/prodigal \
 && make clean \
 && cd /root ; rm -rf Prodigal*
#  ## install sortmerna 2.1b
RUN cd /root \
 && wget -O sortmerna-2.tar.gz https://github.com/biocore/sortmerna/archive/2.1b.tar.gz \
 && tar xvf sortmerna-2.tar.gz \
 && cd sortmerna-2* \
 && sed -i 's/^\#define READLEN [0-9]*/#define READLEN 500000/' include/common.hpp \
 && ./configure \
 && make install \
 && make clean \
 && strip /usr/local/bin/sortmerna* \
 && cd /root ; rm -rf sortmerna-2*
#  ## install skewer
RUN cd /root \
 && git clone https://github.com/teharrison/skewer \
 && cd skewer \
 && make \
 && make install \
 && make clean \
 && cd /root ; rm -rf skewer
#  ## install vsearch 2.12.0
RUN cd /root \
 && wget -O vsearch-2.tar.gz https://github.com/torognes/vsearch/archive/v2.12.0.tar.gz \
 && tar xzf vsearch-2.tar.gz \
 && cd vsearch-2* \
 && sh ./autogen.sh \
 && ./configure --prefix=/usr/local/ \
 && make \
 && make install \
 && make clean \
 && strip /usr/local/bin/vsearch* \
 && cd /root ; rm -rf vsearch-2*
#  ## install CWL runner
RUN pip install pip==23.1 --upgrade
RUN pip install cwlref-runner==1.0 typing==3.10.0.0 --upgrade
#   for jellyfish (ugly)
ENV LD_LIBRARY_PATH="/usr/local/lib"
#   copy files into image
COPY CWL /CWL/
COPY mgcmd/* bin/* /usr/local/bin/
COPY lib/* /usr/local/lib/site_perl/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
