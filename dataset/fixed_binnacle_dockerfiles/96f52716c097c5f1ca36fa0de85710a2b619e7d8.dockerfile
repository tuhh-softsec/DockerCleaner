FROM ubuntu:18.04 AS shark-provider
MAINTAINER OrfeoToolbox Core Team
ENV DEBIAN_FRONTEND="noninteractive"
#   ----------------------------------------------------------------------------
#   First stage : install tools 
#   (based on https://github.com/Shark-ML/Shark/blob/master/.travis.yml)
#   ----------------------------------------------------------------------------
RUN apt-get update -y \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 g++=4:7.4.0-1ubuntu2.3 make=4.1-9.1ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 libboost-date-time-dev=1.65.1.0ubuntu1 libboost-filesystem-dev=1.65.1.0ubuntu1 libboost-graph-dev=1.65.1.0ubuntu1 libboost-program-options-dev=1.65.1.0ubuntu1 libboost-system-dev=1.65.1.0ubuntu1 libboost-thread-dev=1.65.1.0ubuntu1 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 curl=7.58.0-2ubuntu3.24 -y \
 && rm -rf /var/lib/apt/lists/*
RUN cd tmp \
 && curl -o shark.tar.gz https://codeload.github.com/Shark-ML/Shark/tar.gz/v3.1.4 \
 && tar xzf shark.tar.gz \
 && cd Shark-3.1.4 \
 && mkdir build \
 && cd build \
 && cmake -DBUILD_EXAMPLES:BOOL=OFF -DBUILD_TESTING:BOOL=OFF -DENABLE_HDF5:BOOL=OFF -DBUILD_SHARED_LIBS=ON -DENABLE_CBLAS:BOOL=OFF -DENABLE_OPENMP:BOOL=OFF ../. \
 && make install
FROM ubuntu:18.04
MAINTAINER Sebastien Dinot <sebastien.dinot@c-s.fr>
ENV DEBIAN_FRONTEND="noninteractive"
RUN echo "Europe/Paris" > /etc/timezone
#   ----------------------------------------------------------------------------
#   First stage : install tools (they rarely evolve)
#   ----------------------------------------------------------------------------
RUN apt-get update -y \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 make=4.1-9.1ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 ninja-build=1.8.2-1 g++=4:7.4.0-1ubuntu2.3 gcc=4:7.4.0-1ubuntu2.3 clang=1:6.0-41~exp5~ubuntu1 clang-tidy=1:6.0-41~exp5~ubuntu1 clang-format=1:6.0-41~exp5~ubuntu1 ccache=3.4.1-1 git=1:2.17.1-1ubuntu0.17 libtool=2.4.6-2 swig=3.0.12-1 xvfb=2:1.19.6-1ubuntu4.14 -y \
 && rm -rf /var/lib/apt/lists/*
#   ----------------------------------------------------------------------------
#   Second stage : dependencies (they evolve more often)
#   ----------------------------------------------------------------------------
RUN apt-get update -y \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends freeglut3-dev=2.8.1-3 libboost-date-time-dev=1.65.1.0ubuntu1 libboost-filesystem-dev=1.65.1.0ubuntu1 libboost-graph-dev=1.65.1.0ubuntu1 libboost-program-options-dev=1.65.1.0ubuntu1 libboost-system-dev=1.65.1.0ubuntu1 libboost-thread-dev=1.65.1.0ubuntu1 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 libexpat1-dev=2.2.5-3ubuntu0.9 libfftw3-dev=3.3.7-1 libgdal-dev=2.2.3+dfsg-2 libgeotiff-dev=1.4.2-2build1 libglew-dev=2.0.0-5 libglfw3-dev=3.2.1-1 libgsl-dev=2.4+dfsg-6 libinsighttoolkit4-dev=4.12.2-dfsg1-1ubuntu1 libkml-dev=1.3.0-5 libmuparser-dev=2.2.3-6 libmuparserx-dev=4.0.7+dfsg-3 libopencv-core-dev=3.2.0+dfsg-4ubuntu0.1 libopencv-ml-dev=3.2.0+dfsg-4ubuntu0.1 libopenmpi-dev=2.1.1-8 libopenthreads-dev=3.2.3+dfsg1-2ubuntu8 libossim-dev=2.2.2-1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libqt5opengl5-dev=5.9.5+dfsg-0ubuntu2.6 libqwt-qt5-dev=6.1.3-1 libsvm-dev=3.21+ds-1.1 libtinyxml-dev=2.6.2-4 qtbase5-dev=5.9.5+dfsg-0ubuntu2.6 qttools5-dev=5.9.5-0ubuntu1 default-jdk=2:1.11-68ubuntu1~18.04.1 python-dev=2.7.15~rc1-1 python-numpy=1:1.13.3-2ubuntu1 python-gdal=2.2.3+dfsg-2 python3-dev=3.6.7-1~18.04 python3-numpy=1:1.13.3-2ubuntu1 python3-gdal=2.2.3+dfsg-2 -y \
 && rm -rf /var/lib/apt/lists/*
COPY --from=shark-provider /usr/local/include/shark/* /usr/include/shark/
COPY --from=shark-provider /usr/local/lib/libshark* /usr/lib/
COPY --from=shark-provider /usr/local/bin/SharkVersion /usr/bin/
COPY --from=shark-provider /usr/local/lib/cmake/ /usr/lib/cmake/
ENV PATH="/usr/lib/ccache:$PATH"
ENV OTB_USE_SHARK="ON"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
