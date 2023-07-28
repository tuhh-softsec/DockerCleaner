FROM openjdk:8-jre
LABEL maintainer="Jishu Xu <jishuxu@broadinstitute.org>" \
      software="Analysis-tools with Picard-2.10.10, Python-3.5.3 and R-3.3.3" \
      description="A generic toolset for doing large-scale analysis easily with Google Cloud Buckets."
#   Install Picard
#   Please follow the below instructions to invoke picard when you are using this docker image:
#   java jvm-args -jar /usr/picard/picard.jar PicardToolName OPTION1=value1 OPTION2=value2...
ENV picard_version="2.10.10"
WORKDIR /usr/picard
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document ./picard.jar https://github.com/broadinstitute/picard/releases/download/${picard_version}/picard.jar
#   Install Python3
ENV PATH="/usr/local/bin:$PATH"
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pip=20.3.4-4+deb11u1 python3-dev=3.9.2-3 -y \
 && cd /usr/local/bin \
 && ln -s /usr/bin/python3 python \
 && pip3 install --upgrade pip
#   Install R and other dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.9 cmake=3.18.4-2+deb11u1 automake=1:1.16.3-2 curl=7.74.0-1.3+deb11u7 gcc-multilib=4:10.2.1-1 git=1:2.30.2-1+deb11u2 libcurl4-openssl-dev=7.74.0-1.3+deb11u7 libssl-dev=1.1.1n-0+deb11u4 libboost-all-dev=1.74.0.3 libncurses5-dev=6.2+20201114-2 libxml2-dev=2.9.10+dfsg-6.7+deb11u3 libncurses5-dev=6.2+20201114-2 libboost-all-dev=1.74.0.3 libbz2-dev=1.0.8-4 liblzma-dev=5.2.5-2.1~deb11u1 lsb-release=11.1.0 samtools=1.11-1 sudo=1.9.5p2-3+deb11u1 wget=1.21-1+deb11u1 zlib1g-dev=1:1.2.11.dfsg-2+deb11u2 gfortran=4:10.2.1-1 -y
#  # Now install R and littler, and create a link for littler in /usr/local/bin
#  # Also set a default CRAN repo, and make sure littler knows about it too
RUN echo "deb http://http.debian.net/debian sid main" > /etc/apt/sources.list.d/debian-unstable.list \
 && echo 'APT::Default-Release "testing";' > /etc/apt/apt.conf.d/default
ENV R_BASE_VERSION="3.4.4"
RUN apt-get update \
 && apt-get install --no-install-recommends unstable littler=0.3.12-1 r-cran-littler=0.3.12-1 r-base=${R_BASE_VERSION}-* r-base-dev=${R_BASE_VERSION}-* r-recommended=${R_BASE_VERSION}-* -t -y \
 && echo 'options(repos = c(CRAN = "https://cloud.r-project.org/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site \
 && echo 'source("/etc/R/Rprofile.site")' >> /etc/littler.r \
 && ln -s /usr/share/doc/littler/examples/install.r /usr/local/bin/install.r \
 && ln -s /usr/share/doc/littler/examples/install2.r /usr/local/bin/install2.r \
 && ln -s /usr/share/doc/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
 && ln -s /usr/share/doc/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
 && install.r docopt \
 && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
 && rm -rf /var/lib/apt/lists/*
#   Install python packages
RUN pip3 install crimson==0.3.0 HTSeq==0.9.0 matplotlib==2.1.0 numpy==1.12.0 pandas==0.20.3 pysam==0.12.0.1 requests==2.18.4 scipy==0.18.1 sctools==0.1.4 tables==3.4.2 google-cloud-storage git+git://github.com/HumanCellAtlas/pipeline-tools.git
#   Fix cannot import name 'opentype' error
RUN pip3 install --upgrade google-auth-oauthlib
#   Install gcloud components
RUN curl -sSL https://sdk.cloud.google.com > /tmp/gcl \
 && bash /tmp/gcl --install-dir=/usr/gcloud --disable-prompts
#   Configure gcloud
ENV PATH="$PATH:/usr/gcloud/google-cloud-sdk/bin"
RUN wget https://ftp-trace.ncbi.nlm.nih.gov/sra/sdk/2.8.2-1/sratoolkit.2.8.2-1-ubuntu64.tar.gz \
 && tar -zxvf sratoolkit.2.8.2-1-ubuntu64.tar.gz \
 && cp -r sratoolkit.2.8.2-1-ubuntu64/ /usr/local/
ENV PATH="/usr/local/sratoolkit.2.8.2-1-ubuntu64/bin:$PATH"
RUN pip3 install ipython
#   Install R packages
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile
RUN Rscript -e "install.packages('reshape')"
RUN Rscript -e "install.packages('gplots')"
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('googleCloudStorageR')"
RUN Rscript -e "install.packages('gridExtra')"
RUN Rscript -e "install.packages('ggpubr')"
RUN Rscript -e "install.packages('ggpmisc')"
RUN Rscript -e "install.packages('cowplot')"
RUN Rscript -e "install.packages('corrplot')"
RUN Rscript -e "install.packages('ggrepel')"
RUN Rscript -e "install.packages('optparse')"
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R")' -e 'biocLite("rtracklayer")'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R")' -e 'biocLite("scran")'
RUN Rscript -e "install.packages('igraph')"
RUN Rscript -e "install.packages('rsvd')"
RUN Rscript -e "install.packages('factoextra')"
RUN Rscript -e "install.packages('fpc')"
RUN Rscript -e "install.packages('NbClust')"
RUN Rscript -e "install.packages('knitr')"
RUN Rscript -e "install.packages('rmarkdown')"
RUN Rscript -e "install.packages('Rtsne')"
#   Add benchmarking scripts
RUN apt-get update \
 && apt-get install --no-install-recommends unstable cabal-install=3.0.0.0-3 -t -y
RUN cabal update \
 && cabal install pandoc
RUN ln -s /root/.cabal/bin/pandoc /usr/local/bin/pandoc
WORKDIR /usr/local/scripts/
ENV PATH="/usr/local/scripts/:$PATH"
COPY ./*.R /usr/local/scripts/
COPY ./*.py /usr/local/scripts/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
