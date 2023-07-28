FROM ubuntu:18.04 AS build
WORKDIR /home
#   COMMON BUILD TOOLS
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 autoconf=2.69-11 make=4.1-9.1ubuntu1 git=1:2.17.1-1ubuntu0.17 wget=1.19.4-1ubuntu2.2 pciutils=1:3.5.2-1ubuntu1.1 cpio=2.12+dfsg-6ubuntu0.18.04.4 libtool=2.4.6-2 lsb-release=9.20170808ubuntu1 ca-certificates=20211016ubuntu0.18.04.1 pkg-config=0.29.1-0ubuntu2 bison=2:3.0.4.dfsg-1build1 flex=2.6.4-6 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y -q
#   Install cmake
ARG CMAKE_VER=3.13.1
ARG CMAKE_REPO=https://cmake.org/files
RUN wget -O - ${CMAKE_REPO}/v${CMAKE_VER%.*}/cmake-${CMAKE_VER}.tar.gz | tar xz \
 && cd cmake-${CMAKE_VER} \
 && ./bootstrap --prefix="/usr" --system-curl \
 && make -j8 \
 && make install
#   Install automake, use version 1.14 on CentOS
ARG AUTOMAKE_VER=1.14
ARG AUTOMAKE_REPO=https://ftp.gnu.org/pub/gnu/automake/automake-${AUTOMAKE_VER}.tar.xz
RUN apt-get install --no-install-recommends automake=1:1.15.1-3ubuntu2 -y -q
#   Build NASM
ARG NASM_VER=2.13.03
ARG NASM_REPO=https://www.nasm.us/pub/nasm/releasebuilds/${NASM_VER}/nasm-${NASM_VER}.tar.bz2
RUN wget ${NASM_REPO} \
 && tar -xaf nasm* \
 && cd nasm-${NASM_VER} \
 && ./autogen.sh \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu \
 && make -j8 \
 && make install
#   Build YASM
ARG YASM_VER=1.3.0
ARG YASM_REPO=https://www.tortall.net/projects/yasm/releases/yasm-${YASM_VER}.tar.gz
RUN wget -O - ${YASM_REPO} | tar xz \
 && cd yasm-${YASM_VER} \
 && sed -i "s/) ytasm.*/)/" Makefile.in \
 && ./configure --prefix="/usr" --libdir=/usr/lib/x86_64-linux-gnu \
 && make -j8 \
 && make install
#  build ISPC
ARG ISPC_VER=1.9.1
ARG ISPC_REPO=https://downloads.sourceforge.net/project/ispcmirror/v${ISPC_VER}/ispc-v${ISPC_VER}-linux.tar.gz
RUN wget -O - ${ISPC_REPO} | tar xz
ENV ISPC_EXECUTABLE="/home/ispc-v${ISPC_VER}-linux/ispc"
#  build embree
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libtbb-dev=2017~U7-8 libgl1-mesa-dev=20.0.8-0ubuntu1~18.04.1 -y -q
ARG EMBREE_REPO=https://github.com/embree/embree.git
ARG EMBREE_VER=df0b324
RUN git clone ${EMBREE_REPO} \
 && mkdir embree/build \
 && cd embree/build \
 && git checkout ${EMBREE_VER} \
 && cmake .. -Wno-dev -DEMBREE_TUTORIALS=OFF \
 && make -j 8 \
 && make install
#  build ospray
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libtiff-dev=4.0.9-5ubuntu0.10 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libpng-dev=1.6.34-1ubuntu0.18.04.2 libjpeg-dev=8c-2ubuntu8 libboost-python-dev=1.65.1.0ubuntu1 libboost-filesystem-dev=1.65.1.0ubuntu1 libboost-thread-dev=1.65.1.0ubuntu1 -y -q
ARG OpenEXR_VER=0ac2ea3
ARG OpenEXR_REPO=https://github.com/openexr/openexr.git
RUN git clone ${OpenEXR_REPO} ; mkdir openexr/build ; cd openexr/build ; git checkout ${OpenEXR_VER} ; cmake .. ; make -j 8 ; make install
ARG OpenImageIO_VER=5daa9a1
ARG OpenImageIO_REPO=https://github.com/OpenImageIO/oiio.git
RUN git clone ${OpenImageIO_REPO} ; mkdir oiio/build ; cd oiio/build ; git checkout ${OpenImageIO_VER} ; cmake .. ; make -j 8 ; make install
#  build ospray
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends libglfw3-dev=3.2.1-1 libgl1-mesa-dri=20.0.8-0ubuntu1~18.04.1 libxrandr-dev=2:1.5.1-1 libxinerama-dev=2:1.1.3-1 libxcursor-dev=1:1.1.15-1 libmpich-dev=3.3~a2-4 mpich=3.3~a2-4 openssh-server=1:7.6p1-4ubuntu0.7 openssh-client=1:7.6p1-4ubuntu0.7 -y -q
ENV PATH="$PATH:/usr/lib64/mpich/bin"
ARG OSPRAY_VER=c42a885
ARG OSPRAY_REPO=https://github.com/ospray/ospray.git
RUN git clone ${OSPRAY_REPO} ; mkdir ospray/build ; cd ospray/build ; git checkout ${OSPRAY_VER} ; cmake .. -DOSPRAY_MODULE_MPI=ON -DOSPRAY_SG_OPENIMAGEIO=ON ; make -j 8
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/home/ospray/build"
RUN mkdir -p /var/run/sshd ; sed -i 's/^#Port/Port/g' /etc/ssh/sshd_config ; sed -i 's/^Port 22/Port 2222/g' /etc/ssh/sshd_config ; sed -i 's/^#PermitRootLogin/PermitRootLogin/g' /etc/ssh/sshd_config ; sed -i 's/^PermitRootLogin.*/PermitRootLogin yes/g' /etc/ssh/sshd_config ; sed -i 's/# Port 22/Port 2222/g' /etc/ssh/ssh_config ; echo 'root:ospray' | chpasswd ; /usr/sbin/sshd-keygen ; sed -i 's/# StrictHostKeyChecking ask/ StrictHostKeyChecking no/g' /etc/ssh/ssh_config ; /usr/bin/ssh-keygen -q -t rsa -N '' -f /root/.ssh/id_rsa ; cp /root/.ssh/id_rsa.pub /root/.ssh/authorized_keys
#  include(ospray-example_san-miguel.m4)
#  include(ospray-example_xfrog.m4)
FROM build
LABEL Description="This is the base image for ospray-oiio-mpi Ubuntu 18.04"
LABEL Vendor="Intel Corporation"
WORKDIR /home
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
