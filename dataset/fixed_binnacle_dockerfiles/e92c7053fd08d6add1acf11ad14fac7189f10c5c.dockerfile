FROM ubuntu:18.04
#   minimal docker file to get sp and sf running on ubunty 16.04 image,
#   using gdal/geos/proj from ppa:ubuntugis/ubuntugis-unstable
MAINTAINER "edzerpebesma" edzer.pebesma@uni-muenster.de
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y )
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable
RUN echo "deb http://cran.rstudio.com/bin/linux/ubuntu bionic-cran35/ " >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
RUN :
RUN apt-get upgrade -y
RUN export DEBIAN_FRONTEND=noninteractive ; apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends gdb=8.1.1-0ubuntu1 git=1:2.17.1-1ubuntu0.17 libcairo2-dev=1.15.10-2ubuntu0.1 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libexpat1-dev=2.2.5-3ubuntu0.9 libpq-dev=10.23-0ubuntu0.18.04.1 libsqlite3-dev=3.22.0-1ubuntu0.7 libudunits2-dev=2.2.26-1 make=4.1-9.1ubuntu1 pandoc=1.19.2.4~dfsg-1build4 qpdf=8.0.2-3ubuntu0.1 r-base-dev=3.4.4-1ubuntu1 sqlite3=3.22.0-1ubuntu0.7 subversion=1.9.7-4ubuntu1.1 valgrind=1:3.13.0-2ubuntu2.3 vim=2:8.0.1453-1ubuntu1.11 wget=1.19.4-1ubuntu2.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libv8-3.14-dev=3.14.5.8-11ubuntu1 libjq-dev=1.5+dfsg-2 libprotobuf-dev=3.0.0-9.1ubuntu1.1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libprotobuf-dev=3.0.0-9.1ubuntu1.1 protobuf-compiler=3.0.0-9.1ubuntu1.1 unixodbc-dev=2.3.4-1.1ubuntu3 libssh2-1-dev=1.8.0-1 libgit2-dev=0.26.0+dfsg.1-1.1ubuntu0.2 libnetcdf-dev=1:4.6.0-2build1 locales=2.27-3ubuntu1.6 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 -y )
RUN locale-gen en_US.UTF-8
ENV PROJ_VERSION="6.1.1RC1"
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
RUN wget http://download.osgeo.org/proj/proj-${PROJ_VERSION}.tar.gz \
 && tar zxf proj-*tar.gz \
 && cd proj* \
 && ./configure \
 && make \
 && make install \
 && cd .. \
 && ldconfig
#   install proj-datumgrid:
RUN cd /usr/local/share/proj \
 && wget http://download.osgeo.org/proj/proj-datumgrid-latest.zip \
 && unzip -o proj-datumgrid*zip \
 && rm proj-datumgrid*zip \
 && wget https://download.osgeo.org/proj/proj-datumgrid-europe-latest.zip \
 && unzip -o proj-datumgrid*zip \
 && rm proj-datumgrid*zip \
 && cd -
#   GDAL:
ENV GDAL_VERSION="3.0.0"
ENV GDAL_VERSION_NAME="3.0.0"
RUN wget http://download.osgeo.org/gdal/${GDAL_VERSION}/gdal-${GDAL_VERSION_NAME}.tar.gz \
 && tar -xf gdal-${GDAL_VERSION_NAME}.tar.gz \
 && cd gdal* \
 && ./configure \
 && make \
 && make install \
 && cd .. \
 && ldconfig
#  RUN git clone --depth 1 https://github.com/OSGeo/gdal.git
#  RUN cd gdal/gdal \
#    && ls -l \
#    && ./configure \
#    && make \
#    && make install \
#    && cd .. \
#    && ldconfig
#   GEOS:
#  ENV GEOS_VERSION 3.6.2
ENV GEOS_VERSION="3.7.2"
#
RUN wget http://download.osgeo.org/geos/geos-${GEOS_VERSION}.tar.bz2 \
 && bzip2 -d geos-*bz2 \
 && tar xf geos*tar \
 && cd geos* \
 && ./configure \
 && make \
 && make install \
 && cd .. \
 && ldconfig
#   RUN svn  checkout svn://scm.r-forge.r-project.org/svnroot/rgdal/
#   RUN R CMD build rgdal/pkg --no-build-vignettes
#   RUN R CMD INSTALL rgdal_*.tar.gz 
RUN Rscript -e 'install.packages(c("sp", "rgeos", "rgdal", "RPostgreSQL", "RSQLite", "testthat", "knitr", "tidyr", "geosphere", "maptools", "maps", "microbenchmark", "raster", "dplyr", "tibble", "units", "DBI", "covr", "protolite", "tmap", "mapview", "odbc", "pool", "rmarkdown", "RPostgres","spatstat", "stars"))'
RUN git clone --depth 10 https://github.com/r-spatial/sf.git
RUN git clone --depth 10 https://github.com/r-spatial/lwgeom.git
RUN git clone --depth 10 https://github.com/r-spatial/stars.git
#  RUN git config --global user.email "edzer.pebesma@uni-muenster.de"
RUN R CMD build --no-build-vignettes --no-manual lwgeom
RUN R CMD INSTALL lwgeom
RUN R CMD build --no-build-vignettes --no-manual sf
RUN R CMD INSTALL sf
RUN R CMD build --no-build-vignettes --no-manual stars
RUN R CMD INSTALL stars
#   RUN rm ./usr/lib/x86_64-linux-gnu/libgeos*
#  RUN svn  checkout svn://scm.r-forge.r-project.org/svnroot/rgdal/
#   or:
#   svn checkout svn+ssh://edzer@scm.r-forge.r-project.org/svnroot/rgdal/
#  RUN R CMD build rgdal/pkg --no-build-vignettes
#  RUN R CMD INSTALL rgdal_*.tar.gz 
#  RUN R CMD check --no-vignettes --no-manual rgdal_*.tar.gz 
#  RUN Rscript -e 'install.packages(c("stars", "tmap"), repos = "https://cloud.r-project.org")'
RUN Rscript -e 'install.packages("rgdal", repos="http://R-Forge.R-project.org")'
#   after rgdal:
RUN Rscript -e 'install.packages(c("tmaptools", "tmap"))'
RUN R CMD check --no-build-vignettes --no-manual --as-cran sf_*.tar.gz
RUN R CMD check --no-build-vignettes --no-manual --as-cran lwgeom_*.tar.gz
RUN Rscript -e 'install.packages("starsdata", repos="http://gis-bigdata.uni-muenster.de/pebesma/")'
RUN Rscript -e 'install.packages(c("PCICt", "RNetCDF", "future.apply", "ggforce", "ggthemes", "gstat", "ncmeta", "pbapply", "plm", "spacetime", "xts", "zoo"))'
RUN (cd stars ;git pull )
RUN R CMD build --no-build-vignettes --no-manual stars
RUN R CMD check --no-build-vignettes --no-manual --as-cran stars_*.tar.gz
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
