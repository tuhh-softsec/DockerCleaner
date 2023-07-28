#  ###
#   Builds the lucida base image
FROM ubuntu:14.04
#  ### environment variables
ENV LUCIDAROOT="/usr/local/lucida/lucida"
ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV OPENCV_VERSION="2.4.9"
ENV THRIFT_VERSION="0.9.3"
ENV THREADS="4"
ENV PROTOBUF_VERSION="2.5.0"
ENV JAVA_VERSION="8"
ENV JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF8"
#  ### common package installations
RUN sed 's/main$/main universe/' -i /etc/apt/sources.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libatlas3-base=3.10.1-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python2.7-dev=2.7.6-8ubuntu0.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libblas3=1.2.20110419-7 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libblas-dev=1.2.20110419-7 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblapack3=3.5.0-2ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblapack-dev=3.5.0-2ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libc6=2.19-0ubuntu6.15 -y )
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gfortran=4:4.8.2-1ubuntu6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends make=3.81-8.2ubuntu3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends ant=1.9.3-2ubuntu0.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gcc=4:4.8.2-1ubuntu6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends g++=4:4.8.2-1ubuntu6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends automake=1:1.14.1-2ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libboost-all-dev=1.54.0.1ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libevent-dev=2.0.21-stable-1ubuntu1.14.04.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libdouble-conversion-dev=2.0.1-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libtool=2.4.2-1.7ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblz4-dev=0.0~r114-2ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblzma-dev=5.1.1alpha+20120614-2ubuntu2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends binutils-dev=2.24-5ubuntu14.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libjemalloc-dev=3.5.1-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends pkg-config=0.26-1ubuntu4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libtesseract-dev=3.03.02-3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libopenblas-dev=0.2.8-6ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libblas-dev=1.2.20110419-7 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libatlas-dev=3.10.1-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libatlas-base-dev=3.10.1-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libiberty-dev=20131116-1ubuntu0.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblapack-dev=3.5.0-2ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends cmake=2.8.12.2-0ubuntu3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends zip=3.0-8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends unzip=6.0-9ubuntu1.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends sox=14.4.1-3ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libsox-dev=14.4.1-3ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends autoconf-archive=20131101-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends bison=2:3.0.2.dfsg-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends swig=2.0.11-1ubuntu2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends subversion=1.8.8-1ubuntu3.3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libssl-dev=1.0.1f-1ubuntu2.27 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libprotoc-dev=2.5.0-9ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=3.0b2-1ubuntu0.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends flac=1.3.0-2ubuntu0.14.04.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gawk=1:4.0.1+dfsg-2.1ubuntu2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends imagemagick=8:6.7.7.10-6ubuntu3.13 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libgflags-dev=2.0-1.1ubuntu1 libgoogle-glog-dev=0.3.3-1 liblmdb-dev=0.9.10-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libleveldb-dev=1.15.0-2 libsnappy-dev=1.1.0-1ubuntu1 libhdf5-serial-dev=1.8.11-5ubuntu7.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends bc=1.06.95-8ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-numpy=1:1.8.2-0ubuntu0.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends flex=2.5.35-10.1ubuntu2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libkrb5-dev=1.12+dfsg-2ubuntu5.4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libsasl2-dev=2.1.25.dfsg1-17build1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libnuma-dev=2.0.9~rc5-1ubuntu3.14.04.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends scons=2.3.0-2ubuntu1 -y )
#  ### package specific routines
RUN echo oracle-java$JAVA_VERSION-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends oracle-java$JAVA_VERSION-installer -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/oracle-jdk$JAVA_VERSION-installer
#  # install MongoDB, OpenCV, Thrift, and FBThrift
RUN mkdir -p /usr/local/lucida
COPY . /usr/local/lucida/tools
WORKDIR "/usr/local/lucida/tools"
RUN /bin/bash apt_deps.sh
RUN /bin/bash install_mongodb.sh
RUN /bin/bash install_opencv.sh
RUN /bin/bash install_thrift.sh
RUN /bin/bash install_fbthrift.sh
RUN rm -rf mongo-c-driver/ \
 && rm -rf mongo-cxx-driver/ \
 && rm -rf fbthrift/ \
 && rm -rf libbson/ \
 && rm -rf opencv-2.4.9/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
