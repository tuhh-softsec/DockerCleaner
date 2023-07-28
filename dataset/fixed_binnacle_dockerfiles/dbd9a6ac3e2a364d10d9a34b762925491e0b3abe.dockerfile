#   Copyright (c) 2017 Sony Corporation. All Rights Reserved.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM ubuntu:16.04
ENV LC_ALL="C"
ENV LANG="C"
ENV LANGUAGE="C"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 ccache=3.2.4-1 clang-format-3.8=1:3.8-2ubuntu4 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 g++=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 libarchive-dev=3.1.2-11ubuntu0.16.04.8 libgoogle-glog-dev=0.3.4-0.1 libgtest-dev=1.7.0-4ubuntu1 libhdf5-dev=1.8.16+docs-4ubuntu1.1 libleveldb-dev=1.18-5 liblmdb-dev=0.9.17-3 libsnappy-dev=1.1.3-2 libssl-dev=1.0.2g-1ubuntu4.20 make=4.1-6 openssl=1.0.2g-1ubuntu4.20 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 zip=3.0-11 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  ################################################# libarchive
RUN cd /tmp \
 && curl -L https://www.libarchive.org/downloads/libarchive-3.3.2.tar.gz -o libarchive-3.3.2.tar.gz \
 && tar xfa libarchive-3.3.2.tar.gz \
 && mkdir libarchive-build \
 && cd libarchive-build \
 && cmake -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DENABLE_NETTLE=FALSE -DENABLE_OPENSSL=FALSE -DENABLE_LZO=FALSE -DENABLE_LZMA=FALSE -DENABLE_BZip2=FALSE -DENABLE_LIBXML2=FALSE -DENABLE_EXPAT=FALSE -DENABLE_PCREPOSIX=FALSE -DENABLE_LibGCC=FALSE -DENABLE_CNG=FALSE -DENABLE_TAR=FALSE -DENABLE_TAR_SHARED=FALSE -DENABLE_CPIO=FALSE -DENABLE_CPIO_SHARED=FALSE -DENABLE_CAT=FALSE -DENABLE_CAT_SHARED=FALSE -DENABLE_XATTR=FALSE -DENABLE_ACL=FALSE -DENABLE_ICONV=FALSE -DENABLE_TEST=FALSE ../libarchive-3.3.2 \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/*
#  ################################################# protobuf
RUN mkdir /tmp/deps \
 && cd /tmp/deps \
 && PROTOVER=3.4.1 \
 && curl -L https://github.com/google/protobuf/archive/v${PROTOVER}.tar.gz -o protobuf-v${PROTOVER}.tar.gz \
 && tar xvf protobuf-v${PROTOVER}.tar.gz \
 && cd protobuf-${PROTOVER} \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_POSITION_INDEPENDENT_CODE=ON -Dprotobuf_BUILD_TESTS=OFF ../cmake \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/*
#  ################################################# miniconda3
ARG PYTHON_VERSION_MAJOR
ARG PYTHON_VERSION_MINOR
ENV PYVERNAME="${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}"
COPY python/setup_requirements.txt /tmp/deps/
COPY python/requirements.txt /tmp/deps/
COPY python/test_requirements.txt /tmp/deps/
RUN umask 0 \
 && mkdir -p /tmp/deps \
 && cd /tmp/deps \
 && wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh \
 && bash Miniconda3-latest-Linux-x86_64.sh -b -p /opt/miniconda3 \
 && rm -rf Miniconda3-latest-Linux-x86_64.sh \
 && . /opt/miniconda3/bin/activate \
 && conda create -n nnabla-build python=${PYVERNAME} \
 && conda activate nnabla-build \
 && pip install --only-binary -U -r /tmp/deps/setup_requirements.txt \
 && pip install --only-binary -U -r /tmp/deps/requirements.txt \
 && pip install --only-binary -U -r /tmp/deps/test_requirements.txt \
 && conda clean -y --all \
 && cd / \
 && rm -rf /tmp/*
ENV PATH="/opt/miniconda3/envs/nnabla-build/bin:$PATH"
ENV LD_LIBRARY_PATH="/opt/miniconda3/envs/nnabla-build/lib:$LD_LIBRARY_PATH"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
