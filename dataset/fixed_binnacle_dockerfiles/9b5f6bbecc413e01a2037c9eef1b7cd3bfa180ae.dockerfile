FROM ubuntu:18.04
MAINTAINER bhaas@broadinstitute.org
RUN apt-get update \
 && apt-get install --no-install-recommends gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 perl=5.26.1-6ubuntu0.6 python=2.7.15~rc1-1 automake=1:1.15.1-3ubuntu2 make=4.1-9.1ubuntu1 wget=1.19.4-1ubuntu2.2 git=1:2.17.1-1ubuntu0.17 curl=7.58.0-2ubuntu3.24 libdb-dev=1:5.3.21~exp1ubuntu2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 bzip2=1.0.6-8.1ubuntu0.2 libncurses5-dev=6.1-1ubuntu1.18.04 texlive-latex-base=2017.20180305-1 default-jre=2:1.11-68ubuntu1~18.04.1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-dev=2.7.15~rc1-1 gfortran=4:7.4.0-1ubuntu2.3 build-essential=12.4ubuntu1 libghc-zlib-dev=0.6.1.2-1build1 libncurses-dev libbz2-dev=1.0.6-8.1ubuntu0.2 liblzma-dev=5.2.2-1.3ubuntu0.1 libpcre3-dev=2:8.39-9ubuntu0.1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libblas-dev=3.7.1-4ubuntu1 gfortran=4:7.4.0-1ubuntu2.3 git=1:2.17.1-1ubuntu0.17 unzip=6.0-21ubuntu1.2 ftp=0.17-34 libzmq3-dev=4.2.5-1ubuntu0.2 nano=2.9.3-2 ftp=0.17-34 fort77=1.15-11 libreadline-dev=7.0-3 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libx11-dev=2:1.6.4-3ubuntu0.4 libxt-dev=1:1.1.5-1 x11-common=1:7.7+19ubuntu7.1 libcairo2-dev=1.15.10-2ubuntu0.1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libreadline-dev=7.0-3 libjpeg-dev=8c-2ubuntu8 pkg-config=0.29.1-0ubuntu2 libtbb-dev=2017~U7-8 -y \
 && apt-get clean
RUN curl -L https://cpanmin.us | perl - App::cpanminus
RUN cpanm install DB_File
RUN cpanm install URI::Escape
#  # set up tool config and deployment area:
ENV SRC="/usr/local/src"
ENV BIN="/usr/local/bin"
#  ####
#   Install R
WORKDIR $SRC
ENV R_VERSION="R-3.5.2"
RUN curl https://cran.r-project.org/src/base/R-3/$R_VERSION.tar.gz -o $R_VERSION.tar.gz \
 && tar xvf $R_VERSION.tar.gz \
 && cd $R_VERSION \
 && ./configure \
 && make \
 && make install
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("tidyverse", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("edgeR", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("DESeq2", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("ape", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("ctc", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("gplots", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("Biobase", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("qvalue", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("goseq", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("Glimma", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("ROTS", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("GOplot", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("argparse", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("sm", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("fastcluster", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("vioplot", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("DEXSeq", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("tximport", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("tximportData", dep = TRUE)'
#  # bowtie
WORKDIR $SRC
RUN wget https://sourceforge.net/projects/bowtie-bio/files/bowtie/1.2.1.1/bowtie-1.2.1.1-linux-x86_64.zip/download -O bowtie-1.2.1.1-linux-x86_64.zip \
 && unzip bowtie-1.2.1.1-linux-x86_64.zip \
 && mv bowtie-1.2.1.1/bowtie* $BIN
#  # RSEM
RUN mkdir /usr/local/lib/site_perl
WORKDIR $SRC
RUN wget https://github.com/deweylab/RSEM/archive/v1.3.0.tar.gz \
 && tar xvf v1.3.0.tar.gz \
 && cd RSEM-1.3.0 \
 && make \
 && cp rsem-* $BIN \
 && cp rsem_perl_utils.pm /usr/local/lib/site_perl/ \
 && cd ../ \
 && rm -r RSEM-1.3.0
#  # Kallisto
WORKDIR $SRC
RUN wget https://github.com/pachterlab/kallisto/releases/download/v0.43.1/kallisto_linux-v0.43.1.tar.gz \
 && tar xvf kallisto_linux-v0.43.1.tar.gz \
 && mv kallisto_linux-v0.43.1/kallisto $BIN
#  # FASTQC
WORKDIR $SRC
RUN wget http://www.bioinformatics.babraham.ac.uk/projects/fastqc/fastqc_v0.11.5.zip \
 && unzip fastqc_v0.11.5.zip \
 && chmod 755 /usr/local/src/FastQC/fastqc \
 && ln -s /usr/local/src/FastQC/fastqc $BIN/.
#   blast
WORKDIR $SRC
RUN wget ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/2.5.0/ncbi-blast-2.5.0+-x64-linux.tar.gz \
 && tar xvf ncbi-blast-2.5.0+-x64-linux.tar.gz \
 && cp ncbi-blast-2.5.0+/bin/* $BIN \
 && rm -r ncbi-blast-2.5.0+
#  # Bowtie2
WORKDIR $SRC
RUN wget https://sourceforge.net/projects/bowtie-bio/files/bowtie2/2.3.4.1/bowtie2-2.3.4.1-linux-x86_64.zip/download -O bowtie2-2.3.4.1-linux-x86_64.zip \
 && unzip bowtie2-2.3.4.1-linux-x86_64.zip \
 && mv bowtie2-2.3.4.1-linux-x86_64/bowtie2* $BIN \
 && rm *.zip \
 && rm -r bowtie2-2.3.4.1-linux-x86_64
#  # Samtools
RUN wget https://github.com/samtools/samtools/releases/download/1.7/samtools-1.7.tar.bz2 \
 && tar xvf samtools-1.7.tar.bz2 \
 && cd samtools-1.7/ \
 && ./configure \
 && make \
 && make install
#  # Jellyfish
RUN wget https://github.com/gmarcais/Jellyfish/releases/download/v2.2.7/jellyfish-2.2.7.tar.gz \
 && tar xvf jellyfish-2.2.7.tar.gz \
 && cd jellyfish-2.2.7/ \
 && ./configure \
 && make \
 && make install
#  # Salmon
WORKDIR $SRC
RUN wget https://github.com/COMBINE-lab/salmon/releases/download/v0.9.1/Salmon-0.9.1_linux_x86_64.tar.gz \
 && tar xvf Salmon-0.9.1_linux_x86_64.tar.gz \
 && ln -s $SRC/Salmon-latest_linux_x86_64/bin/salmon $BIN/.
ENV LD_LIBRARY_PATH="/usr/local/lib"
RUN pip install numpy==1.24.2
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("fastcluster", dep = TRUE)'
#  # patch the RSEM install... need convert-sam-for-rsem  too!
WORKDIR $SRC
RUN wget https://github.com/deweylab/RSEM/archive/v1.3.0.tar.gz \
 && tar xvf v1.3.0.tar.gz \
 && cd RSEM-1.3.0 \
 && make \
 && cp rsem-* $BIN \
 && cp convert-sam-for-rsem $BIN \
 && cp rsem_perl_utils.pm /usr/local/lib/site_perl/ \
 && cd ../ \
 && rm -r RSEM-1.3.0
#   adding multiqc
RUN pip install git+https://github.com/ewels/MultiQC.git
#  #########
#  # Trinity
WORKDIR $SRC
RUN apt-get update \
 && apt-get install --no-install-recommends cmake=3.10.2-1ubuntu2.18.04.2 -y
RUN apt-get install --no-install-recommends rsync=3.1.2-2.1ubuntu1.6 -y
ENV TRINITY_VERSION="2.8.5"
ENV TRINITY_CO="d35f3c1149bab077ca7c83f209627784469c41c6"
WORKDIR $SRC
RUN git clone https://github.com/trinityrnaseq/trinityrnaseq.git \
 && cd trinityrnaseq \
 && git checkout $TRINITY_CO \
 && make \
 && make plugins \
 && make install \
 && cd ../ \
 && rm -r trinityrnaseq
ENV TRINITY_HOME="/usr/local/bin/trinityrnaseq"
ENV PATH="${TRINITY_HOME}:${PATH}"
COPY Dockerfile $SRC/Dockerfile.$TRINITY_VERSION
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
