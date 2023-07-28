FROM rocker/tidyverse
LABEL maintainer="nan.xiao@sevenbridges.com"
#  # Install common dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.9.13p1-1ubuntu2 gdebi-core=0.9.5.7+nmu6 pandoc=2.17.1.1-1.1ubuntu1 pandoc-citeproc libcurl4-gnutls-dev=7.88.1-7ubuntu1 curl=7.88.1-7ubuntu1 libcairo2-dev=1.16.0-7 libxt-dev=1:1.2.1-1 unp=2.0~pre10 emacs=1:28.2+1-13ubuntu3 supervisor=4.2.1-1ubuntu1 libfuse-dev=2.9.9-6 gnupg=2.2.40-1ubuntu2 -y
#     aufs-tools \
#     cgroupfs-mount
#   RUN wget --no-verbose http://ftp.us.debian.org/debian/pool/main/l/lvm2/libdevmapper1.02.1_1.02.136-1_amd64.deb && \
#      dpkg -i libdevmapper1.02.1_1.02.136-1_amd64.deb && \
#      rm -f libdevmapper1.02.1_1.02.136-1_amd64.deb
#   RUN wget --no-verbose http://ftp.us.debian.org/debian/pool/main/libt/libtool/libltdl7_2.4.6-2_amd64.deb && \
#      dpkg -i libltdl7_2.4.6-2_amd64.deb && \
#      rm -f libltdl7_2.4.6-2_amd64.deb
#  # Install Docker
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 ca-certificates=20230311 gnupg2=2.2.40-1ubuntu2 software-properties-common=0.99.35 -y \
 && curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add - \
 && apt-key fingerprint 0EBFCD88 \
 && add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $( lsb_release -cs ;) stable" \
 && apt-get update -y \
 && apt-get install --no-install-recommends docker-ce -y
#  #################
#   sevenbridges-r #
#  #################
#   Install from GitHub instead of Bioconductor for the latest version
RUN Rscript -e "install.packages('BiocManager'); devtools::install_github('sbg/sevenbridges-r', repos = BiocManager::repositories(), build_vignettes = FALSE, dependencies = TRUE)"
#  ####################
#   Seven Bridges SDK #
#  ####################
#  # Install SBG Rabix
RUN wget https://github.com/rabix/bunny/releases/download/v1.0.5-1/rabix-1.0.5.tar.gz \
 && tar -zvxf rabix-1.0.5.tar.gz \
 && ln -s /rabix-cli-1.0.5/rabix /usr/local/bin/rabix
#  # Install SBG Command line uploader
#   RUN wget https://igor.sbgenomics.com/sbg-uploader/sbg-uploader.tgz \
#       && tar zxvf sbg-uploader.tgz -C / \
#      && ln -s  /sbg-uploader/bin/sbg-uploader.sh /usr/local/bin/sbg-uploader.sh
#  # Install CGC Command line uploader
#   RUN wget https://cgc.sbgenomics.com/cgc-uploader/cgc-uploader.tgz \
#      && tar zxvf cgc-uploader.tgz -C / \
#      && ln -s  /cgc-uploader/bin/cgc-uploader.sh /usr/local/bin/cgc-uploader.sh
#  # Copy command line interface and report templates needed
COPY src/runif.R /usr/local/bin/
RUN mkdir /report/
COPY report/report.Rmd /report/
#  # Install liftr and packrat
RUN Rscript -e "install.packages(c('liftr', 'packrat'), repos = 'https://cloud.r-project.org/')"
#  # (because --deps TRUE can fail when packages are added/removed from CRAN)
RUN rm -rf /var/lib/apt/lists/ \
 && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
#  ###############
#   Shiny Server #
#  ###############
#  # Thanks: rocker-org/shiny
#  # Download and install Shiny Server
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" \
 && VERSION=$( cat version.txt ;) \
 && wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O shiny-server-latest.deb \
 && gdebi -n shiny-server-latest.deb \
 && rm -f version.txt shiny-server-latest.deb
RUN Rscript -e "install.packages(c('shiny', 'rmarkdown', 'rsconnect'), repos = 'https://cloud.r-project.org/')"
RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/
RUN mkdir /home/rstudio/ShinyApps/
RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* /home/rstudio/ShinyApps/
EXPOSE 3838/tcp 8787/tcp
#   COPY src/shiny-server.sh /usr/bin/shiny-server.sh
#   RUN wget --no-verbos https://raw.githubusercontent.com/sbg/sevenbridges-r/master/inst/docker/sevenbridges/src/shiny-server.conf -P /etc/shiny-server/
#   RUN wget --no-verbos https://raw.githubusercontent.com/sbg/sevenbridges-r/master/inst/docker/sevenbridges/src/supervisord.conf	-P /etc/shiny-server/
COPY src/shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY src/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#  # set directory to `~/ShinyApps`
#   RUN yes | /opt/shiny-server/bin/deploy-example user-dirs
#   RUN cp -R /usr/local/lib/R/site-library/shiny/examples/* ~/ShinyApps/
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
#   CMD ["/init;/usr/bin/shiny-server.sh"]
#   CMD ["sh", "-c", "/usr/bin/shiny-server.sh;/init"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
