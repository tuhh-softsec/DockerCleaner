FROM ubuntu:18.04
LABEL authors="Vaclav Petras,Markus Neteler"
LABEL maintainer="wenzeslaus@gmail.com,neteler@osgeo.org"
#   system environment
ENV DEBIAN_FRONTEND="noninteractive"
#   data directory - not using the base images volume because then the permissions cannot be adapted
ENV DATA_DIR="/data"
#   GRASS GIS compile dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 libblas-dev=3.7.1-4ubuntu1 libbz2-dev=1.0.6-8.1ubuntu0.2 libcairo2-dev=1.15.10-2ubuntu0.1 libfftw3-dev=3.3.7-1 libfreetype6-dev=2.8.1-2ubuntu2.2 libgdal-dev=2.2.3+dfsg-2 libgeos-dev=3.6.2-1build2 libglu1-mesa-dev=9.0.0-2.1build1 libgsl0-dev libjpeg-dev=8c-2ubuntu8 liblapack-dev=3.7.1-4ubuntu1 libncurses5-dev=6.1-1ubuntu1.18.04 libnetcdf-dev=1:4.6.0-2build1 libopenjp2-7=2.3.0-2+deb10u2build0.18.04.1 libopenjp2-7-dev=2.3.0-2+deb10u2build0.18.04.1 libpdal-dev=1.6.0-1build2 pdal=1.6.0-1build2 libpdal-plugin-python=1.6.0-1build2 libpng-dev=1.6.34-1ubuntu0.18.04.2 libpq-dev=10.23-0ubuntu0.18.04.1 libproj-dev=4.9.3-2 libreadline-dev=7.0-3 libsqlite3-dev=3.22.0-1ubuntu0.7 libtiff-dev=4.0.9-5ubuntu0.10 libxmu-dev=2:1.1.2-2 libzstd-dev=1.3.3+dfsg-2ubuntu1.2 bison=2:3.0.4.dfsg-1build1 flex=2.6.4-6 g++=4:7.4.0-1ubuntu2.3 gettext=0.19.8.1-6ubuntu0.3 gdal-bin=2.2.3+dfsg-2 libfftw3-bin=3.3.7-1 make=4.1-9.1ubuntu1 ncurses-bin=6.1-1ubuntu1.18.04 netcdf-bin=1:4.6.0-2build1 proj-bin=4.9.3-2 proj-data=4.9.3-2 python=2.7.15~rc1-1 python-dev=2.7.15~rc1-1 python-numpy=1:1.13.3-2ubuntu1 python-pil=5.1.0-1ubuntu0.8 python-ply=3.11-1 python-six=1.11.0-2 sqlite3=3.22.0-1ubuntu0.7 subversion=1.9.7-4ubuntu1.1 unixodbc-dev=2.3.4-1.1ubuntu3 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y --no-install-suggests \
 && apt-get autoremove \
 && apt-get clean \
 && mkdir -p $DATA_DIR
RUN echo LANG="en_US.UTF-8" > /etc/default/locale
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
RUN mkdir /code
RUN mkdir /code/grass
#   add repository files to the image
COPY . /code/grass
WORKDIR /code/grass
#   Set gcc/g++ environmental variables for GRASS GIS compilation, without debug symbols
ENV MYCFLAGS="\"-O2 -std=gnu99 -m64\""
ENV MYLDFLAGS="\"-s\""
#   CXX stuff:
ENV LD_LIBRARY_PATH="\"/usr/local/lib\""
ENV LDFLAGS="\"$MYLDFLAGS\""
ENV CFLAGS="\"$MYCFLAGS\""
ENV CXXFLAGS="\"$MYCXXFLAGS\""
#   Configure, compile and install GRASS GIS
ENV NUMTHREADS="2"
RUN ./configure --enable-largefile --with-cxx --with-nls --with-readline --with-sqlite --with-bzlib --with-zstd --with-cairo --with-cairo-ldflags=-lfontconfig --with-freetype --with-freetype-includes="/usr/include/freetype2/" --with-fftw --with-netcdf --with-pdal --with-proj --with-proj-share=/usr/share/proj --with-geos=/usr/bin/geos-config --with-postgres --with-postgres-includes="/usr/include/postgresql" --with-opengl-libs=/usr/include/GL \
 && make -j $NUMTHREADS \
 && make install \
 && ldconfig
#   enable simple grass command regardless of version number
RUN ln -s /usr/local/bin/grass* /usr/local/bin/grass
#   Reduce the image size
RUN apt-get autoremove -y
RUN apt-get clean -y
#   set SHELL var to avoid /bin/sh fallback in interactive GRASS GIS sessions in docker
ENV SHELL="/bin/bash"
#   Fix permissions
RUN chmod -R a+rwx $DATA_DIR
#   create a user
RUN useradd -m -U grass
#   declare data volume late so permissions apply
VOLUME $DATA_DIR
WORKDIR $DATA_DIR
#   Further reduce the docker image size 
RUN rm -rf /code/grass
#   switch the user
USER grass
CMD ["/usr/local/bin/grass", "--version"]
# Please add your HEALTHCHECK here!!!
