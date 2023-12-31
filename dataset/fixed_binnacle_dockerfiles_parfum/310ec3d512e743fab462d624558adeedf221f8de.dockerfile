#  Copyright (c) 2017 Sony Corporation. All Rights Reserved.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#  for nnabla>=1.0.17
ARG BASE
FROM ${BASE}
ENV LC_ALL="C"
ENV LANG="C"
ENV LANGUAGE="C"
RUN yum install -y epel-release yum-utils \
 && yum install -y curl hdf5 hdf5-devel redhat-lsb-core rpm-build unzip wget zip zlib-static \
 && yum clean all
#  ----------------------------------------------------------------------------
#  TODO: More sustainable way of installing gcc 5.x.
#  ----------------------------------------------------------------------------
#  It seems like devtoolset-4 is no longer provided in SCL repository,
#  but RPM packages for devtoolset-4 are still available in centos SCL
#  repoitory. We directly install these packages and their dependencies
#  from RPMs using yum command.
#  Packages we want to install are as follows:
#    devtoolset-4-gcc
#    devtoolset-4-gcc-c++
#    devtoolset-4-gcc-gfortran
#    git19
#  ----------------------------------------------------------------------------
#  Installing devtoolset-4 directly from RPMs.
RUN PKGS=" devtoolset-4-runtime-4.1-3.el6.x86_64.rpm devtoolset-4-binutils-2.25.1-8.el6.x86_64.rpm devtoolset-4-gcc-5.3.1-6.1.el6.x86_64.rpm devtoolset-4-libstdc++-devel-5.3.1-6.1.el6.x86_64.rpm devtoolset-4-gcc-c++-5.3.1-6.1.el6.x86_64.rpm devtoolset-4-libquadmath-devel-5.3.1-6.1.el6.x86_64.rpm devtoolset-4-gcc-gfortran-5.3.1-6.1.el6.x86_64.rpm" \
 && for pkg in $PKGS; do yum install -y http://mirror.centos.org/centos/6/sclo/x86_64/rh/devtoolset-4/$pkg ; done \
 && yum clean all
#  Installing git19 directly from RPMs.
RUN REPO_BASE=http://mirror.centos.org/centos/6/sclo/x86_64/rh/git19 \
 && yum install -y $REPO_BASE/git19-runtime-1.2-4.el6.x86_64.rpm $REPO_BASE/git19-perl-Git-1.9.4-4.el6.1.noarch.rpm $REPO_BASE/git19-git-1.9.4-4.el6.1.x86_64.rpm \
 && yum clean all
ENV PATH="/opt/rh/git19/root/usr/bin:/opt/rh/devtoolset-4/root/usr/bin:$PATH"
ENV LD_LIBRARY_PATH="/opt/rh/gti19/root/usr/lib64:/opt/rh/devtoolset-4/root/usr/lib64:/opt/rh/devtoolset-4/root/usr/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/opt/rh/git19/root/usr/lib64/pkgconfig:${PKG_CONFIG_PATH}"
# ################################################## cmake
ENV CMAKEVER="3.14.3"
RUN mkdir /tmp/deps \
 && cd /tmp/deps \
 && yum install -y cmake3 \
 && curl -L https://github.com/Kitware/CMake/releases/download/v${CMAKEVER}/cmake-${CMAKEVER}.tar.gz -o cmake-${CMAKEVER}.tar.gz \
 && tar xf cmake-${CMAKEVER}.tar.gz \
 && cd cmake-${CMAKEVER} \
 && mkdir build \
 && cd build \
 && cmake3 -DBUILD_TESTING=FALSE .. \
 && make \
 && make install \
 && yum remove -y cmake3 \
 && yum clean all \
 && rm -rf /var/cache/yum/* \
 && cd / \
 && rm -rf /tmp/*
# ################################################# protobuf
ENV PROTOVER="3.4.1"
RUN mkdir /tmp/deps \
 && cd /tmp/deps \
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
# ################################################# libarchive
RUN mkdir /tmp/deps \
 && cd /tmp/deps \
 && curl -L https://www.libarchive.org/downloads/libarchive-3.3.2.tar.gz -o libarchive-3.3.2.tar.gz \
 && tar xfa libarchive-3.3.2.tar.gz \
 && mkdir libarchive-build \
 && cd libarchive-build \
 && cmake -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DENABLE_NETTLE=FALSE -DENABLE_OPENSSL=FALSE -DENABLE_LZO=FALSE -DENABLE_LZMA=FALSE -DENABLE_BZip2=FALSE -DENABLE_LIBXML2=FALSE -DENABLE_EXPAT=FALSE -DENABLE_PCREPOSIX=FALSE -DENABLE_LibGCC=FALSE -DENABLE_CNG=FALSE -DENABLE_TAR=FALSE -DENABLE_TAR_SHARED=FALSE -DENABLE_CPIO=FALSE -DENABLE_CPIO_SHARED=FALSE -DENABLE_CAT=FALSE -DENABLE_CAT_SHARED=FALSE -DENABLE_XATTR=FALSE -DENABLE_ACL=FALSE -DENABLE_ICONV=FALSE -DENABLE_TEST=FALSE ../libarchive-3.3.2 \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/*
# ################################################# patchelf
RUN mkdir /tmp/deps \
 && cd /tmp/deps \
 && wget http://nixos.org/releases/patchelf/patchelf-0.9/patchelf-0.9.tar.bz2 \
 && tar xfa patchelf-0.9.tar.bz2 \
 && cd patchelf-0.9 \
 && ./configure \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/*
# ################################################# miniconda3
ARG PYTHON_VERSION_MAJOR
ARG PYTHON_VERSION_MINOR
ENV PYVERNAME="${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR}"
COPY python/requirements.txt /tmp/deps/
RUN umask 0 \
 && cd /tmp/deps \
 && wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh \
 && bash Miniconda3-latest-Linux-x86_64.sh -b -p /opt/miniconda3 \
 && rm -rf Miniconda3-latest-Linux-x86_64.sh \
 && . /opt/miniconda3/bin/activate \
 && conda create -n nnabla-build python=${PYVERNAME} \
 && conda activate nnabla-build \
 && pip install --only-binary -U -r /tmp/deps/requirements.txt \
 && conda clean -y --all \
 && cd / \
 && rm -rf /tmp/*
ENV PATH="/opt/miniconda3/envs/nnabla-build/bin:$PATH"
ENV LD_LIBRARY_PATH="/opt/miniconda3/envs/nnabla-build/lib:$LD_LIBRARY_PATH"
