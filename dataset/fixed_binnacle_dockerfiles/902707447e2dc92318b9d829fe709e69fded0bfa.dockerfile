#   Copyright (c) 2019 Google LLC
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy of
#   this software and associated documentation files (the "Software"), to deal in
#   the Software without restriction, including without limitation the rights to
#   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
#   the Software, and to permit persons to whom the Software is furnished to do so,
#   subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
#   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
#   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
#   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#   To build a new docker image, please run:
#   docker build -t streaming-api:v1.0 -f env/Dockerfile .
FROM ubuntu:xenial
LABEL description="Streaming API build environment."
RUN : \
 && apt-get upgrade -y
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=1.2.35 autoconf=2.69-9 autoconf-archive=20150925-1 automake=1:1.15-4ubuntu1 binutils-dev=2.26.1-1ubuntu1~16.04.8 build-essential=12.1ubuntu2 clang=1:3.8-33ubuntu3.1 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 g++=4:5.3.1-1ubuntu1 gcc=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 google-perftools=2.4-0ubuntu5.16.04.1 libass-dev=0.13.1-1 libboost-all-dev=1.58.0.1ubuntu1 libdouble-conversion-dev=2.0.1-3ubuntu2 libevent-dev=2.0.21-stable-2ubuntu0.16.04.1 libgflags-dev=2.1.2-3 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libgoogle-glog-dev=0.3.4-0.1 libgtest-dev=1.7.0-4ubuntu1 libiberty-dev=20160215-1ubuntu0.3 libjemalloc-dev=3.6.0-9ubuntu1 liblz4-dev=0.0~r131-2ubuntu2 liblzma-dev=5.1.1alpha+20120614-2ubuntu2 libopenmpi-dev=1.10.2-8ubuntu1 libsdl2-dev=2.0.4+dfsg1-2ubuntu2.16.04.2 libsdl2-ttf-dev=2.0.14+dfsg1-1 libsdl2-2.0 libsnappy-dev=1.1.3-2 libssl-dev=1.0.2g-1ubuntu4.20 libtool=2.4.6-0.1 libva-dev=1.7.0-1ubuntu0.1 libvdpau-dev=1.1.1-3ubuntu1 libvorbis-dev=1.3.5-3ubuntu0.2 libxcb1-dev=1.11.1-1ubuntu1 libxcb-shm0-dev=1.11.1-1ubuntu1 libxcb-xfixes0-dev=1.11.1-1ubuntu1 make=4.1-6 pkg-config=0.29.1-0ubuntu1 python-dev=2.7.12-1~16.04 python-setuptools=20.7.0-1 software-properties-common=0.96.20.10 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 vim=2:7.4.1689-3ubuntu1.5 zip=3.0-11 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y ) \
 && apt-get clean
RUN easy_install pip
RUN add-apt-repository ppa:jonathonf/ffmpeg-3 -y
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libav-tools=7:2.8.17-0ubuntu0.1 libavdevice-dev=7:2.8.17-0ubuntu0.1 libx264-dev=2:0.148.2643+git5c65704-1 libx265-dev=1.9-3 libnuma-dev=2.0.11-1ubuntu1.1 libvpx-dev=1.5.0-2ubuntu1.1 libfdk-aac-dev=0.1.3+20140816-2 libmp3lame-dev=3.99.5+repack1-9build1 libopus-dev=1.1.2-1ubuntu1 ffmpeg=7:2.8.17-0ubuntu0.1 -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends libgstreamer1.0-0=1.8.3-1~ubuntu0.1 gstreamer1.0-plugins-base=1.8.3-1ubuntu0.3 gstreamer1.0-plugins-good=1.8.3-1ubuntu0.5 gstreamer1.0-plugins-bad=1.8.3-1ubuntu0.2 gstreamer1.0-plugins-ugly=1.8.3-1ubuntu0.1 gstreamer1.0-libav=1.8.3-1ubuntu0.2 gstreamer1.0-doc=1.8.3-1~ubuntu0.1 gstreamer1.0-tools=1.8.3-1~ubuntu0.1 -y ) \
 && apt-get clean
ENV CPLUS_INCLUDE_PATH="/usr"
ENV LD_LIBRARY_PATH="/usr"
#  install gRPC 1.12.0
ENV GRPC_RELEASE_TAG="v1.12.0"
RUN git clone -b ${GRPC_RELEASE_TAG} https://github.com/grpc/grpc /var/local/git/grpc
RUN cd /var/local/git/grpc \
 && git submodule update --init \
 && make \
 && make prefix=/usr install \
 && make clean
#  install proto v3.6.0
ENV PROTO_RELEASE_TAG="v3.6.0"
RUN git clone -b ${PROTO_RELEASE_TAG} https://github.com/google/protobuf.git /var/local/git/protobuf \
 && cd /var/local/git/protobuf \
 && git submodule update --init --recursive \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install
#  install bazel v0.19.2
ENV BAZEL_RELEASE_TAG="0.19.2"
RUN cd /var/local \
 && wget https://github.com/bazelbuild/bazel/releases/download/${BAZEL_RELEASE_TAG}/bazel-${BAZEL_RELEASE_TAG}-installer-linux-x86_64.sh \
 && chmod +x bazel-${BAZEL_RELEASE_TAG}-installer-linux-x86_64.sh \
 && ./bazel-${BAZEL_RELEASE_TAG}-installer-linux-x86_64.sh --prefix=/usr
#  install Google Video Intelligence Streaming API Python Libraries
RUN pip install google-cloud-videointelligence==2.11.1 --upgrade
RUN pip install psutil==5.9.4 --user
#  set up environment for Google Video Intelligence Streaming API
ENV SRC_DIR="/googlesrc"
ENV BIN_DIR="/google"
#  copy aistreamer directory to Docker
RUN mkdir -p $SRC_DIR
COPY BUILD *.BUILD *.md *.py LICENSE WORKSPACE $SRC_DIR/
COPY client $SRC_DIR/client
COPY env $SRC_DIR/env
COPY proto $SRC_DIR/proto
#  build aistreamer
RUN cd $SRC_DIR/client/cpp \
 && bazel build -c opt streaming_client_main
#  copy binaries to BIN_DIR directory
RUN mkdir -p $BIN_DIR
RUN mkdir -p $BIN_DIR/cpp
RUN mkdir -p $BIN_DIR/python
RUN cp $SRC_DIR/bazel-bin/client/cpp/streaming_client_main $BIN_DIR/cpp
RUN cp $SRC_DIR/client/cpp/config/* $BIN_DIR/cpp
RUN cp $SRC_DIR/client/python/*.py $BIN_DIR/python
RUN chmod +x $BIN_DIR/python/*.py
#  clean up the build artifacts and source directory.
RUN cd $SRC_DIR \
 && bazel clean --expunge
#  set work directory
WORKDIR $BIN_DIR
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
