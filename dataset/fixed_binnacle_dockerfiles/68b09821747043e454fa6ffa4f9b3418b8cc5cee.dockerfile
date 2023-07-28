#     Copyright 2015 Ufora Inc.
#
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#
#         http://www.apache.org/licenses/LICENSE-2.0
#
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
FROM ubuntu:14.04
MAINTAINER Ronen Hilewicz <ronen@ufora.com>
#   This image is used to build Ufora packages on Ubuntu 14.04
#   It includes a build of python that links against libtcmalloc.so
#   APT package required to build and run Ufora
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends bison=2:3.0.2.dfsg-2 ccache=3.1.9-1 clang-3.5=1:3.5-4ubuntu2~trusty2 curl=7.35.0-1ubuntu2.20 gdb=7.7.1-0ubuntu5~14.04.3 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 libblas-dev=1.2.20110419-7 libboost-date-time1.55-dev=1.55.0-1 libboost-filesystem1.55-dev=1.55.0-1 libboost-python1.55-dev=1.55.0-1 libboost-regex1.55-dev=1.55.0-1 libboost-thread1.55-dev=1.55.0-1 libboost-test1.55-dev=1.55.0-1 libclang-3.5-dev=1:3.5-4ubuntu2~trusty2 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libgoogle-perftools-dev=2.1-2ubuntu1.1 liblapack-dev=3.5.0-2ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 ocaml=4.01.0-3ubuntu3.1 pkg-config=0.26-1ubuntu4 psmisc=22.20-1ubuntu2 rsync=3.1.0-2ubuntu0.4 software-properties-common=0.92.37.8 unixodbc-dev=2.2.14p2-5ubuntu5 wget=1.15-1ubuntu1.14.04.5 -y )
#   Python 2.7.9 - built from source to link against libtcmalloc
RUN apt-get build-dep -y python2.7 \
 && cd /tmp \
 && wget -nv https://www.python.org/ftp/python/2.7.9/Python-2.7.9.tar.xz \
 && tar xf Python-2.7.9.tar.xz
RUN cd /tmp/Python-2.7.9 \
 && CC=clang-3.5 CXX=clang++-3.5 ./configure --prefix=/usr/local --enable-shared --with-libs='-ltcmalloc' --with-system-ffi --enable-ipv6 --enable-unicode=ucs4 --with-ensurepip=upgrade \
 && make \
 && make install \
 && ldconfig \
 && /usr/local/bin/python -m ensurepip \
 && rm -rf /tmp/Python-2.7.9*
#   Required python modules
RUN pip install requests==2.28.2 boto==2.49.0 hdfs==2.7.0 nose==1.3.7 numpy==1.24.2 pyodbc==4.0.39 pexpect==4.8.0 pandas==2.0.0 scipy==1.10.1 wsaccel==0.6.4 psutil==5.9.4 jupyter==1.0.0 --allow-unverified pyodbc
#   NodeJS
RUN curl -sL https://deb.nodesource.com/setup_4.x | bash -
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 build-essential=11.6ubuntu6 -y ) \
 && npm install coffee-script@1.10.0 mocha@2.3.3 forever@0.14.1 -g
RUN echo "ccache -M 10G" >> /etc/bash.bashrc
ENV CCACHE_DIR="/volumes/ccache"
ENV CCACHE_COMPILERCHECK="content"
RUN ln -s /usr/bin/clang-3.5 /usr/bin/clang
RUN ln -s /usr/bin/clang++-3.5 /usr/bin/clang++
RUN mkdir /var/core
#  install CUDA
RUN wget http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1404/x86_64/cuda-repo-ubuntu1404_7.5-18_amd64.deb
RUN dpkg -i cuda-repo-ubuntu1404_7.5-18_amd64.deb
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends cuda-nvrtc-7-5 cuda-cudart-7-5 cuda-drivers=352.79-1 libcuda1-352=352.79-0ubuntu1 cuda-core-7-5 cuda-driver-dev-7-5 -y --force-yes )
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
