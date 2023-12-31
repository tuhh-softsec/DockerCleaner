FROM ubuntu:16.04
#  minimal docker file to get sp and sf running on ubunty 16.04 image,
#  using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable
MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common -y
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/ " >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
RUN apt-get update
RUN apt-get upgrade -y
RUN export DEBIAN_FRONTEND=noninteractive ; apt-get update -y \
 && apt-get install --no-install-recommends libcurl4-openssl-dev qpdf pandoc make wget git libgdal-dev libgeos-dev libproj-dev libudunits2-dev -y
RUN apt-get install --no-install-recommends texinfo texlive-base texlive-extra-utils texlive-fonts-extra texlive-fonts-recommended texlive-generic-recommended texlive-latex-base texlive-latex-extra texlive-latex-recommended -y
#  stuff for the tmaptools/rmapshaper/geojsonio etc stack:
RUN apt-get install --no-install-recommends libv8-3.14-dev libprotobuf-dev protobuf-compiler libcairo2-dev pandoc pandoc-citeproc -y
RUN add-apt-repository -y ppa:opencpu/jq
RUN apt-get update
RUN apt-get install --no-install-recommends libjq-dev -y
# # Check out & build R-devel:
RUN apt-get install --no-install-recommends subversion -y
RUN cd /tmp \
 && svn co https://svn.r-project.org/R/trunk R-devel
RUN /tmp/R-devel/tools/rsync-recommended
# # Build and install according the standard 'recipe' I emailed/posted years ago
RUN apt-get install --no-install-recommends libreadline-dev libbz2-dev -y
RUN cd /tmp/R-devel \
 && R_PAPERSIZE=letter R_BATCHSAVE="--no-save --no-restore" R_BROWSER=xdg-open PAGER=/usr/bin/pager PERL=/usr/bin/perl R_UNZIPCMD=/usr/bin/unzip R_ZIPCMD=/usr/bin/zip R_PRINTCMD=/usr/bin/lpr LIBnn=lib AWK=/usr/bin/awk CFLAGS=$( R CMD config CFLAGS ;) CXXFLAGS=$( R CMD config CXXFLAGS ;) ./configure --enable-R-shlib --without-blas --without-lapack --with-readline --with-recommended-packages --program-suffix=dev --with-x=no \
 && make \
 && make install \
 && rm -rf /tmp/R-devel
# # Set default CRAN repo
RUN echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /usr/local/lib/R/etc/Rprofile.site
RUN Rscript -e 'install.packages(c("sf", "lwgeom", "covr", "raster"), dependencies = TRUE, repos = "https://cloud.r-project.org")'
RUN rm -fr sf
RUN git clone https://github.com/r-spatial/sf.git
RUN R CMD build sf
# ENV PROJ_VERSION 5.0.1
# ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
# RUN wget http://download.osgeo.org/proj/proj-${PROJ_VERSION}.tar.gz \
#   && tar zxf proj-*tar.gz \
#   && cd proj* \
#   && ./configure \
#   && make \
#   && make install \
#   && cd .. \
#   && ldconfig
RUN Rscript -e 'install.packages(c("rgdal"), dependencies = FALSE, repos = "https://cloud.r-project.org")'
RUN Rscript -e 'install.packages(c("RPostgres"), dependencies = FALSE, repos = "https://cloud.r-project.org")'
RUN cd sf \
 && git pull
RUN PROJ_LIB=/usr/share/proj R CMD INSTALL sf_*tar.gz
RUN PROJ_LIB=/usr/share/proj R CMD check --as-cran sf_*tar.gz
CMD ["/bin/bash"]
