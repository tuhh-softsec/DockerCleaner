FROM ubuntu:xenial
MAINTAINER Michael Smith <Michael.smith.erdc@gmail.com>
#  Setup user
ARG UID
ARG GID
ARG MRSID_DOWNLOAD_URL
RUN addgroup --gid $GID msgroup
RUN adduser --no-create-home --disabled-login msuser --gecos "" --uid $UID --gid $GID
ENV LIBKML_DOWNLOAD="install-libkml-r864-64bit.tar.gz"
ENV FILEGDBAPI_DOWNLOAD="FileGDB_API_1_2-64.tar.gz"
ENV MRSID_DIR="MrSID_DSDK-9.5.4.4703-rhel6.x86-64.gcc482"
ENV MRSID_DOWNLOAD="MrSID_DSDK-9.5.4.4703-rhel6.x86-64.gcc482.tar.gz"
#   Setup build env
RUN mkdir /build
RUN apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 16126D3A3E5C1192 \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y --fix-missing \
 && add-apt-repository ppa:ubuntugis/ubuntugis-unstable -y \
 && apt-get update \
 && apt-get install --no-install-recommends gcc-4.9=4.9.3-13ubuntu2 g++-4.9=4.9.3-13ubuntu2 build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 git=1:2.7.4-0ubuntu1.10 make=4.1-6 cmake=3.5.1-1ubuntu3 python-dev=2.7.12-1~16.04 python-software-properties=0.96.20.10 software-properties-common=0.96.20.10 libc6-dev=2.23-0ubuntu11.3 openssh-client=1:7.2p2-4ubuntu2.10 libpng12-dev=1.2.54-1ubuntu1.1 libjpeg-dev=8c-2ubuntu8 libgif-dev=5.1.4-0.3~16.04.1 liblzma-dev=5.1.1alpha+20120614-2ubuntu2 libgeos-dev=3.5.0-1ubuntu2 libproj-dev=4.9.2-2 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libexpat-dev libxerces-c-dev=3.1.3+debian-1 libnetcdf-dev=1:4.4.0-2 netcdf-bin=1:4.4.0-2 libpoppler-dev=0.41.0-0ubuntu1.16 libspatialite-dev=4.3.0a-5 swig=3.0.8-0ubuntu3 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libpodofo-dev=0.9.3-4 poppler-utils=0.41.0-0ubuntu1.16 libfreexl-dev=1.0.2-1ubuntu0.1 libwebp-dev=0.4.4-1 libepsilon-dev=0.9.2-3ubuntu1 libpcre3-dev=2:8.38-3.1 gfortran=4:5.3.1-1ubuntu1 libarpack2-dev=3.3.0-1build2 libpq-dev=9.5.25-0ubuntu0.16.04.1 libflann-dev=1.8.4-4.1 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libhdf5-dev=1.8.16+docs-4ubuntu1.1 libjsoncpp-dev=1.7.2-1 clang-3.8=1:3.8-2ubuntu4 libhdf4-alt-dev=4.2.10-3.2 libsqlite3-dev=3.11.0-1ubuntu1.5 libltdl-dev=2.4.6-0.1 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 ninja=0.1.3-2 python-pip=8.1.1-2ubuntu0.6 libpng-dev python-dev=2.7.12-1~16.04 libprotobuf-c-dev=1.2.1-1 protobuf-c-compiler=1.2.1-1 libboost-filesystem1.58-dev=1.58.0+dfsg-5ubuntu3.1 libboost-iostreams1.58-dev=1.58.0+dfsg-5ubuntu3.1 libboost-program-options1.58-dev=1.58.0+dfsg-5ubuntu3.1 libboost-system1.58-dev=1.58.0+dfsg-5ubuntu3.1 libboost-thread1.58-dev=1.58.0+dfsg-5ubuntu3.1 libogdi3.2-dev=3.2.0~beta2-7.1build1 time=1.7-25.1 -y --fix-missing \
 && apt-get remove --purge -y $BUILD_PACKAGES \
 && rm -rf /var/lib/apt/lists/*
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.9 20 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 20
RUN wget --no-verbose http://s3.amazonaws.com/etc-data.koordinates.com/gdal-travisci/${LIBKML_DOWNLOAD} -O /build/${LIBKML_DOWNLOAD} \
 && tar -C /build -xzf /build/${LIBKML_DOWNLOAD} \
 && cp -r /build/install-libkml/include/* /usr/local/include \
 && cp -r /build/install-libkml/lib/* /usr/local/lib \
 && rm -Rf /build/install-libkml
RUN curl -L ${$MRSID_DOWNLOAD_URL}/${MRSID_DOWNLOAD} -o /build/${MRSID_DOWNLOAD} \
 && tar -C /build -xzf /build/${MRSID_DOWNLOAD} \
 && cp -r /build/${MRSID_DIR}/Raster_DSDK/include/* /usr/local/include \
 && cp -r /build/${MRSID_DIR}/Raster_DSDK/lib/* /usr/local/lib \
 && rm -Rf /build/${MRSID_DIR}
RUN wget --no-verbose http://s3.amazonaws.com/etc-data.koordinates.com/gdal-travisci/${FILEGDBAPI_DOWNLOAD} -O /build/${FILEGDBAPI_DOWNLOAD} \
 && tar -C /build -xzf /build/${FILEGDBAPI_DOWNLOAD} \
 && cp -r /build/FileGDB_API/include/* /usr/local/include \
 && cp -r /build/FileGDB_API/lib/* /usr/local/lib \
 && rm -Rf /build/FileGDB_API
RUN cd /build \
 && curl -LO https://github.com/uclouvain/openjpeg/archive/v2.3.0.tar.gz \
 && tar -zxf /build/v2.3.0.tar.gz \
 && cd /build/openjpeg-2.3.0 \
 && mkdir -v build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr .. \
 && make -j \
 && make install \
 && rm -Rf /build/openjpeg*
RUN wget https://bootstrap.pypa.io/get-pip.py -O get-pip.py \
 && python get-pip.py \
 && pip install pip==23.1 wheel==0.40.0 numpy==1.24.2 --upgrade
ARG GDAL_VERSION
RUN cd /build \
 && git clone https://github.com/OSGeo/gdal.git \
 && cd /build/gdal \
 && git checkout ${GDAL_VERSION} \
 && cd /build/gdal/gdal \
 && ./configure --prefix=/usr --with-png=internal --with-jpeg=internal --with-libz=internal --with-libtiff=internal --with-geotiff=internal --with-gif=internal --with-libjson-c=internal --with-poppler --with-spatialite --with-python --with-liblzma --with-openjpeg --with-ogdi --with-webp --with-pg --with-mrsid=/usr/local --with-libkml --with-filegdb --with-hdf5=/usr/lib/x86_64-linux-gnu/hdf5/serial --with-openjpeg \
 && make \
 && make install \
 && cd swig/python \
 && python setup.py build \
 && python setup.py install \
 && ldconfig \
 && rm -Rf /build/gdal
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 subversion=1.9.3-2ubuntu1.3 git=1:2.7.4-0ubuntu1.10 libaio1=0.3.110-2 make=4.1-6 cmake=3.5.1-1ubuntu3 python-numpy=1:1.11.0-1ubuntu1 python-dev=2.7.12-1~16.04 python-software-properties=0.96.20.10 software-properties-common=0.96.20.10 libv8-dev=3.14.5.8-5ubuntu2 libc6-dev=2.23-0ubuntu11.3 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libcairo2-dev=1.14.6-1 libpq-dev=9.5.25-0ubuntu0.16.04.1 libharfbuzz-dev=1.0.1-1ubuntu0.1 libfribidi-dev=0.19.7-1 flex=2.6.0-11 bison=2:3.0.4.dfsg-1 libfcgi-dev=2.4.0-8.3 libxml2=2.9.3+dfsg1-1ubuntu0.7 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 bzip2=1.0.6-8ubuntu0.2 apache2=2.4.18-2ubuntu3.17 apache2-utils=2.4.18-2ubuntu3.17 apache2-dev=2.4.18-2ubuntu3.17 libaprutil1-dev=1.5.4-1build1 libapr1-dev=1.5.2-3 libpng12-dev=1.2.54-1ubuntu1.1 libjpeg-dev=8c-2ubuntu8 libcurl4-gnutls-dev=7.47.0-1ubuntu2.19 libpcre3-dev=2:8.38-3.1 libpixman-1-dev=0.33.6-1 libgeos-dev=3.5.0-1ubuntu2 libsqlite3-dev=3.11.0-1ubuntu1.5 libdb-dev=1:5.3.21~exp1ubuntu2 libtiff-dev sudo=1.8.16-0ubuntu1.10 -y --fix-missing \
 && rm -rf /var/lib/apt/lists/partial/* /tmp/* /var/tmp/*
RUN echo "msuser ALL=NOPASSWD: ALL" >> /etc/sudoers
ARG MAPSERVER_VERSION
RUN cd /build \
 && git clone https://github.com/mapserver/mapserver.git mapserver \
 && cd /build/mapserver \
 && git checkout ${MAPSERVER_VERSION} \
 && mkdir /build/mapserver/build \
 && cd /build/mapserver/build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DHARFBUZZ_INCLUDE_DIR=/usr/include/harfbuzz -DWITH_CLIENT_WFS=ON -DWITH_CLIENT_WMS=ON -DWITH_CURL=ON -DWITH_GDAL=ON -DWITH_GIF=ON -DWITH_ICONV=ON -DWITH_KML=ON -DWITH_LIBXML2=ON -DWITH_OGR=ON -DWITH_POINT_Z_M=ON -DWITH_PROJ=ON -DWITH_SOS=ON -DWITH_THREAD_SAFETY=ON -DWITH_WCS=ON -DWITH_WFS=ON -DWITH_WMS=ON -DWITH_FCGI=ON -DWITH_FRIBIDI=ON -DWITH_CAIRO=ON -DWITH_HARFBUZZ=ON -DWITH_POSTGIS=on -DWITH_V8=OFF .. \
 && make \
 && make install \
 && ldconfig \
 && rm -Rf /build/mapserver
#  RUN mkdir /vdatum \
#      && cd /vdatum \
#      && wget http://download.osgeo.org/proj/vdatum/usa_geoid2012.zip && unzip -j -u usa_geoid2012.zip -d /usr/share/proj \
#      && wget http://download.osgeo.org/proj/vdatum/usa_geoid2009.zip && unzip -j -u usa_geoid2009.zip -d /usr/share/proj \
#      && wget http://download.osgeo.org/proj/vdatum/usa_geoid2003.zip && unzip -j -u usa_geoid2003.zip -d /usr/share/proj \
#      && wget http://download.osgeo.org/proj/vdatum/usa_geoid1999.zip && unzip -j -u usa_geoid1999.zip -d /usr/share/proj \
#      && wget http://download.osgeo.org/proj/vdatum/vertcon/vertconc.gtx && mv vertconc.gtx /usr/share/proj \
#      && wget http://download.osgeo.org/proj/vdatum/vertcon/vertcone.gtx && mv vertcone.gtx /usr/share/proj \
#      && wget http://download.osgeo.org/proj/vdatum/vertcon/vertconw.gtx && mv vertconw.gtx /usr/share/proj \
#      && wget http://download.osgeo.org/proj/vdatum/egm96_15/egm96_15.gtx && mv egm96_15.gtx /usr/share/proj \
#      && wget http://download.osgeo.org/proj/vdatum/egm08_25/egm08_25.gtx && mv egm08_25.gtx /usr/share/proj \
#      && rm -rf /vdatum
#   Force buit libraries dependencies
RUN ldconfig
#   Enable these Apache modules
RUN a2enmod actions cgi alias headers
RUN chmod o+x /usr/local/bin/mapserv
RUN ln -s /usr/local/bin/mapserv /usr/lib/cgi-bin/mapserv
RUN chmod 755 /usr/lib/cgi-bin
ENV HOST_IP="`ifconfig | grep inet | grep Mask:255.255.255.0 | cut -d ' ' -f 12 | cut -d ':' -f 2`"
CMD sudo service apache2 start \
 && bash
USER msuser
# Please add your HEALTHCHECK here!!!
