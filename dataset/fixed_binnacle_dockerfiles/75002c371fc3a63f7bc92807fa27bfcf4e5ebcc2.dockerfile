FROM ubuntu:16.04
#   minimal docker file to get sp and sf running on ubunty 16.04 image,
#   using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable
MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y )
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/ " >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
RUN :
RUN apt-get upgrade -y
RUN export DEBIAN_FRONTEND=noninteractive ; apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends libcurl4-openssl-dev=7.47.0-1ubuntu2.19 qpdf=8.0.2-3~16.04.1 pandoc=1.16.0.2~dfsg-1 make=4.1-6 wget=1.17.1-1ubuntu1.5 git=1:2.7.4-0ubuntu1.10 libgdal-dev=1.11.3+dfsg-3build2 libgeos-dev=3.5.0-1ubuntu2 libproj-dev=4.9.2-2 libudunits2-dev=2.2.20-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends texinfo=6.1.0.dfsg.1-5 texlive-base=2015.20160320-1ubuntu0.1 texlive-extra-utils=2015.20160320-1 texlive-fonts-extra=2015.20160320-1 texlive-fonts-recommended=2015.20160320-1ubuntu0.1 texlive-generic-recommended=2015.20160320-1ubuntu0.1 texlive-latex-base=2015.20160320-1ubuntu0.1 texlive-latex-extra=2015.20160320-1 texlive-latex-recommended=2015.20160320-1ubuntu0.1 -y )
#   stuff for the tmaptools/rmapshaper/geojsonio etc stack:
RUN (apt-get update ;apt-get install --no-install-recommends libv8-3.14-dev=3.14.5.8-5ubuntu2 libprotobuf-dev=2.6.1-1.3 protobuf-compiler=2.6.1-1.3 libcairo2-dev=1.14.6-1 pandoc=1.16.0.2~dfsg-1 pandoc-citeproc=0.9-1 -y )
RUN add-apt-repository -y ppa:opencpu/jq
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libjq-dev -y )
#  # Check out & build R-devel:
RUN (apt-get update ;apt-get install --no-install-recommends subversion=1.9.3-2ubuntu1.3 -y )
RUN cd /tmp \
 && svn co https://svn.r-project.org/R/trunk R-devel
RUN /tmp/R-devel/tools/rsync-recommended
#  # Build and install according the standard 'recipe' I emailed/posted years ago
RUN (apt-get update ;apt-get install --no-install-recommends libreadline-dev=6.3-8ubuntu2 libbz2-dev=1.0.6-8ubuntu0.2 -y )
RUN cd /tmp/R-devel \
 && R_PAPERSIZE=letter R_BATCHSAVE="--no-save --no-restore" R_BROWSER=xdg-open PAGER=/usr/bin/pager PERL=/usr/bin/perl R_UNZIPCMD=/usr/bin/unzip R_ZIPCMD=/usr/bin/zip R_PRINTCMD=/usr/bin/lpr LIBnn=lib AWK=/usr/bin/awk CFLAGS=$( R CMD config CFLAGS ;) CXXFLAGS=$( R CMD config CXXFLAGS ;) ./configure --enable-R-shlib --without-blas --without-lapack --with-readline --with-recommended-packages --program-suffix=dev --with-x=no \
 && make \
 && make install \
 && rm -rf /tmp/R-devel
#  # Set default CRAN repo
RUN echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /usr/local/lib/R/etc/Rprofile.site
RUN Rscript -e 'install.packages(c("sf", "lwgeom", "covr", "raster"), dependencies = TRUE, repos = "https://cloud.r-project.org")'
RUN rm -fr sf
RUN git clone https://github.com/r-spatial/sf.git
RUN R CMD build sf
#  ENV PROJ_VERSION 5.0.1
#  ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
#  RUN wget http://download.osgeo.org/proj/proj-${PROJ_VERSION}.tar.gz \
#    && tar zxf proj-*tar.gz \
#    && cd proj* \
#    && ./configure \
#    && make \
#    && make install \
#    && cd .. \
#    && ldconfig
RUN Rscript -e 'install.packages(c("rgdal"), dependencies = FALSE, repos = "https://cloud.r-project.org")'
RUN Rscript -e 'install.packages(c("RPostgres"), dependencies = FALSE, repos = "https://cloud.r-project.org")'
RUN cd sf \
 && git pull
RUN PROJ_LIB=/usr/share/proj R CMD INSTALL sf_*tar.gz
RUN PROJ_LIB=/usr/share/proj R CMD check --as-cran sf_*tar.gz
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
