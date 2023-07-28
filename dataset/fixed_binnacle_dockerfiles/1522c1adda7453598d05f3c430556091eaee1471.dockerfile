FROM ubuntu:18.04
ARG language
ENV SINGNET_REPOS="/opt/singnet"
ENV GOPATH="${SINGNET_REPOS}/go"
ENV PATH="${GOPATH}/bin:${PATH}"
RUN mkdir -p ${GOPATH}
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=1.6.14 nano=2.9.3-2 vim=2:8.0.1453-1ubuntu1.11 git=1:2.17.1-1ubuntu0.17 wget=1.19.4-1ubuntu2.2 curl=7.58.0-2ubuntu3.24 zip=3.0-11build1 libudev-dev=237-3ubuntu10.57 libusb-1.0-0-dev=2:1.0.21-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python3=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 -y )
RUN pip3 install snet-cli
RUN SNETD_VERSION=`curl -s https://api.github.com/repos/singnet/snet-daemon/releases/latest | grep -oP '"tag_name": "\\K(.*)(?=")' ` \
 && cd /tmp \
 && wget https://github.com/singnet/snet-daemon/releases/download/${SNETD_VERSION}/snet-daemon-${SNETD_VERSION}-linux-amd64.tar.gz \
 && tar -xvf snet-daemon-${SNETD_VERSION}-linux-amd64.tar.gz \
 && mv snet-daemon-${SNETD_VERSION}-linux-amd64/snetd /usr/bin/snetd
RUN cd ${SINGNET_REPOS} \
 && git clone --depth 1 https://github.com/singnet/dev-portal.git
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.4ubuntu1 autoconf=2.69-11 libtool=2.4.6-2 pkg-config=0.29.1-0ubuntu2 libgflags-dev=2.2.1-1 libgtest-dev=1.8.0-6 clang=1:6.0-41~exp5~ubuntu1 libc++-dev=6.0-2 -y ) ; git clone -b $( curl -L https://grpc.io/release ;) https://github.com/grpc/grpc ; cd grpc ; git submodule update --init ; make ; make install ; (apt-get update ;apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 -y ) ; echo "deb [arch=amd64] http://storage.googleapis.com/bazel-apt stable jdk1.8" | tee /etc/apt/sources.list.d/bazel.list ; curl https://bazel.build/bazel-release.pub.gpg | apt-key add - ; apt-get update ; (apt-get update ;apt-get install --no-install-recommends bazel -y ) ; apt-get upgrade -y bazel ; bazel build :all ; make install ; cd third_party/protobuf ; make ; make install
RUN apt-get -y remove x264 libx264-dev \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.4ubuntu1 checkinstall=1.6.2-4ubuntu2 cmake=3.10.2-1ubuntu2.18.04.2 pkg-config=0.29.1-0ubuntu2 yasm=1.3.0-2build1 git=1:2.17.1-1ubuntu0.17 gfortran=4:7.4.0-1ubuntu2.3 libjpeg8-dev=8c-2ubuntu8 libpng-dev=1.6.34-1ubuntu0.18.04.2 software-properties-common=0.96.24.32.20 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 -y )
RUN python -m pip install --upgrade pip
RUN python -m pip install grpcio
RUN python -m pip install grpcio-tools
RUN add-apt-repository "deb http://security.ubuntu.com/ubuntu xenial-security main"
RUN :
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libjasper1 libjasper-dev libtiff-dev=4.0.9-5ubuntu0.10 libavcodec-dev=7:3.4.11-0ubuntu0.1 libavformat-dev=7:3.4.11-0ubuntu0.1 libswscale-dev=7:3.4.11-0ubuntu0.1 libdc1394-22-dev=2.2.5-1 libxine2-dev=1.2.8-2build2 libv4l-dev=1.14.2-1 libgstreamer1.0-dev=1.14.5-0ubuntu1~18.04.2 libgstreamer-plugins-base1.0-dev=1.14.5-0ubuntu1~18.04.3 libgtk2.0-dev=2.24.32-1ubuntu1 libtbb-dev=2017~U7-8 qt5-default=5.9.5+dfsg-0ubuntu2.6 libatlas-base-dev=3.10.3-5 libfaac-dev=1.29.7.7-1 libmp3lame-dev=3.100-2 libtheora-dev=1.1.1+dfsg.1-14 libvorbis-dev=1.3.5-4.2 libxvidcore-dev=2:1.3.5-1 libopencore-amrnb-dev=0.1.3-2.1 libopencore-amrwb-dev=0.1.3-2.1 libavresample-dev=7:3.4.11-0ubuntu0.1 x264=2:0.152.2854+gite9a5903-2 v4l-utils=1.14.2-1 libprotobuf-dev=3.0.0-9.1ubuntu1.1 protobuf-compiler=3.0.0-9.1ubuntu1.1 libgoogle-glog-dev=0.3.5-1 libgflags-dev=2.2.1-1 libgphoto2-dev=2.5.16-2 libeigen3-dev=3.3.4-4 libhdf5-dev=1.10.0-patch1+docs-4 doxygen=1.8.13-10 cmake=3.10.2-1ubuntu2.18.04.2 unzip=6.0-21ubuntu1.2 git=1:2.17.1-1ubuntu0.17 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:8.0.1453-1ubuntu1.11 -y )
RUN cd \
 && wget https://github.com/opencv/opencv/archive/4.0.0.zip \
 && unzip 4.0.0.zip \
 && cd opencv-4.0.0 \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j4 \
 && make install \
 && cd \
 && rm 4.0.0.zip
RUN cd \
 && wget https://github.com/opencv/opencv_contrib/archive/4.0.0.zip \
 && unzip 4.0.0.zip \
 && cd opencv-4.0.0/build \
 && cmake -DOPENCV_EXTRA_MODULES_PATH=../../opencv_contrib-4.0.0/modules/ .. \
 && make -j4 \
 && make install \
 && cd ../.. \
 && rm 4.0.0.zip
WORKDIR ${SINGNET_REPOS}
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
