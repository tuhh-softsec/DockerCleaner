#   Creates an Enchiladas container for the Tapestry project.
#   This Dockerfile was originally written by Tanner Hobson (thobson2@vols.utk.edu)
FROM ubuntu:xenial
MAINTAINER Mohammad Raji <mahmadza@vols.utk.edu>
ARG build_parallel
ARG minifyjs
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 python=2.7.12-1~16.04 libtbb-dev=4.4~20151115-0ubuntu3 libglu1-mesa-dev=9.0.0-2.1 freeglut3-dev=2.8.1-2 mesa-common-dev=18.0.5-0ubuntu0~16.04.1 libc6-dev=2.23-0ubuntu11.3 libnetcdf-c++4-1=4.2.1-3 libnetcdf-dev=1:4.4.0-2 libnetcdf-c++4-dev=4.2.1-3 python-pip=8.1.1-2ubuntu0.6 git=1:2.7.4-0ubuntu1.10 yasm=1.3.0-2 -y \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /opt/
COPY rapidjson /opt/rapidjson
WORKDIR /opt/rapidjson/build/
RUN true \
 && cmake .. -DRAPIDJSON_BUILD_EXAMPLES:BOOL=OFF -DRAPIDJSON_BUILD_TESTS:BOOL=OFF \
 && make ${build_parallel} \
 && make install \
 && rm -rf /opt/rapidjson
WORKDIR /opt/
COPY tbb2017_20161128oss_lin.tgz /opt/
RUN mv tbb2017_20161128oss tbb
WORKDIR /opt/tbb/
WORKDIR /opt/
COPY ispc-v1.9.1-linux.tar.gz /opt/
RUN mv ispc-v1.9.1-linux ispc
WORKDIR /opt/ispc/
RUN update-alternatives --install /usr/bin/ispc ispc /opt/ispc/ispc 1
WORKDIR /opt/
COPY embree-2.16.4.x86_64.linux.tar.gz /opt/
RUN mv embree-2.16.4.x86_64.linux embree
WORKDIR /opt/embree/
WORKDIR /opt/
COPY ospray /opt/ospray
WORKDIR /opt/ospray/build
RUN true \
 && cmake .. -Dembree_DIR=/opt/embree -DOSPRAY_ENABLE_APPS:BOOL=OFF -DTBB_ROOT=/opt/tbb/ -DOSPRAY_TASKING_SYSTEM=TBB \
 && make ${build_parallel} \
 && make install \
 && rm -rf /opt/ospray
#   Install SVT-HEVC for tapestry-gui
WORKDIR /opt
RUN true \
 && git clone https://github.com/intel/SVT-HEVC \
 && cd SVT-HEVC \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j `nproc ` \
 && make install
WORKDIR /opt
RUN true \
 && git clone https://github.com/FFmpeg/FFmpeg ffmpeg
WORKDIR /opt/ffmpeg/
RUN git checkout release/4.1 \
 && git apply ../SVT-HEVC/ffmpeg_plugin/0001-lavc-svt_hevc-add-libsvt-hevc-encoder-wrapper.patch \
 && git apply ../SVT-HEVC/ffmpeg_plugin/0002-doc-Add-libsvt_hevc-encoder-docs.patch \
 && export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib \
 && export PKG_CONFIG_PATH=${PKG_CONFIG_PATH}:/usr/local/lib/pkgconfig \
 && ./configure --enable-libsvthevc \
 && make -j `nproc ` \
 && make install
WORKDIR /opt/
COPY enchiladas /opt/enchiladas
COPY pbnj /opt/enchiladas/resources/pbnj
COPY pistache /opt/enchiladas/resources/pistache
WORKDIR /opt/enchiladas/build
RUN true \
 && pip install rjsmin==1.2.1 \
 && cmake .. -DCMAKE_CXX_COMPILER=g++ -DCMAKE_C_COMPILER=gcc -DUSE_NETCDF:BOOL=ON -DBUILD_EXAMPLES:BOOL=OFF -DOSPRAY_INSTALL_DIR=/usr/local/ -DTBB_ROOT=/opt/tbb/ -Dembree_DIR=/opt/embree -DENABLE_MINIFY=${minifyjs:+ON}${minifyjs:-OFF} \
 && make ${build_parallel} \
 && make install
#   Copy dependency installation script 
COPY install_dependencies.sh /opt/install_dependencies.sh
RUN /opt/install_dependencies.sh
CMD ["sh", "-c", "./server", "/config", "9010", "${APP_DIR}"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
