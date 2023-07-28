FROM ubuntu:18.04
MAINTAINER bhaas@broadinstitute.org
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 perl=5.26.1-6ubuntu0.6 python=2.7.15~rc1-1 automake=1:1.15.1-3ubuntu2 make=4.1-9.1ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 less=487-0.1 vim=2:8.0.1453-1ubuntu1.11 wget=1.19.4-1ubuntu2.2 git=1:2.17.1-1ubuntu0.17 curl=7.58.0-2ubuntu3.24 libdb-dev=1:5.3.21~exp1ubuntu2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 bzip2=1.0.6-8.1ubuntu0.2 libncurses5-dev=6.1-1ubuntu1.18.04 texlive-latex-base=2017.20180305-1 default-jre=2:1.11-68ubuntu1~18.04.1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-dev=2.7.15~rc1-1 gfortran=4:7.4.0-1ubuntu2.3 build-essential=12.4ubuntu1 libghc-zlib-dev=0.6.1.2-1build1 libncurses-dev libbz2-dev=1.0.6-8.1ubuntu0.2 liblzma-dev=5.2.2-1.3ubuntu0.1 libpcre3-dev=2:8.39-9ubuntu0.1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libblas-dev=3.7.1-4ubuntu1 gfortran=4:7.4.0-1ubuntu2.3 git=1:2.17.1-1ubuntu0.17 unzip=6.0-21ubuntu1.2 ftp=0.17-34 libzmq3-dev=4.2.5-1ubuntu0.2 nano=2.9.3-2 ftp=0.17-34 fort77=1.15-11 libreadline-dev=7.0-3 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libx11-dev=2:1.6.4-3ubuntu0.4 libxt-dev=1:1.1.5-1 x11-common=1:7.7+19ubuntu7.1 libcairo2-dev=1.15.10-2ubuntu0.1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libreadline6-dev libjpeg8-dev=8c-2ubuntu8 pkg-config=0.29.1-0ubuntu2 build-essential=12.4ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 gsl-bin=2.4+dfsg-6 libgsl0-dev libeigen3-dev=3.3.4-4 libboost-all-dev=1.65.1.0ubuntu1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libcairo2-dev=1.15.10-2ubuntu0.1 libxt-dev=1:1.1.5-1 libgtk2.0-dev=2.24.32-1ubuntu1 libcairo2-dev=1.15.10-2ubuntu0.1 xvfb=2:1.19.6-1ubuntu4.14 xauth=1:1.0.10-1 xfonts-base=1:1.0.4+nmu1 apt-utils=1.6.14 apt-transport-https=1.6.14 gdebi-core=0.9.5.7+nmu2 locales=2.27-3ubuntu1.6 -y
COPY installR.sh /usr/local/src
RUN /usr/local/src/installR.sh
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8
COPY install_rstudio_server.sh /usr/local/src
RUN /usr/local/src/install_rstudio_server.sh
#  ## User setup
RUN groupadd -g 2000 training \
 && useradd -m -u 2000 -g 2000 training
RUN echo 'training:training' | chpasswd
RUN chsh -s /bin/bash training
ENV HOME="/home/training"
RUN echo "alias ll='ls -la -G'" >> /home/training/.profile
RUN usermod -G training,www-data training
RUN usermod -a -G sudo training
RUN apt-get install --no-install-recommends openssh-server=1:7.6p1-4ubuntu0.7 libncurses5-dev=6.1-1ubuntu1.18.04 apache2=2.4.29-1ubuntu4.27 supervisor=3.3.1-1.1 -y
#  ########
#  ## GateOne SSH interface
#  ########
RUN git clone https://github.com/liftoff/GateOne/ \
 && cd GateOne \
 && python setup.py install \
 && pip install tornado==4.5.3 \
 && python run_gateone.py --configure
EXPOSE 22/tcp 80/tcp 443/tcp 8787/tcp
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("devtools", dep = TRUE)'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("fastcluster", dep = TRUE);'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("useful", dep = TRUE);'
#  #
#  # Seurat 2.0
#  #
RUN apt-get install --no-install-recommends libhdf5-dev=1.10.0-patch1+docs-4 -y
RUN Rscript -e 'install.packages("/usr/local/src/caTools_1.17.1.mod.tar.gz", repos=NULL, type="source");'
RUN Rscript -e 'library(devtools); install_github("satijalab/seurat", ref = "develop");'
#   needed for combat
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("sva", dep = TRUE)'
#   needed for seurat clustering (hopefully not in a future seurat release)
RUN chmod 777 /usr/local/lib/R/site-library
#  #################
#  ###  singleCellTK
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("singleCellTK", dep = TRUE);'
#  # auto-run setup
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
#  ###########################
#  # more seurat configuration
RUN Rscript -e 'library(devtools); install_github("satijalab/seurat", ref = "develop");'
#  # for Matan:
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("AUCell", dep = TRUE);'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("destiny", dep = TRUE);'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("fgsea", dep = TRUE);'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("DropletUtils", dep = TRUE);'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("precrec", dep = TRUE);'
RUN Rscript -e 'source("http://bioconductor.org/biocLite.R");library(BiocInstaller); biocLite("pbapply", dep = TRUE);'
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
