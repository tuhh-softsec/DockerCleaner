#  ###
#   Builds the lucida base image
FROM ubuntu:14.04
#  ### environment variables
ENV LUCIDAROOT="/usr/local/lucida/lucida"
ENV THRIFT_ROOT="/usr/src/thrift-$THRIFT_VERSION"
ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV CAFFE="/usr/src/caffe/distribute"
ENV CPU_ONLY="1 # for caffe"
ENV OPENCV_VERSION="2.4.9"
ENV THRIFT_VERSION="0.9.2"
ENV THREADS="4"
ENV PROTOBUF_VERSION="2.5.0"
ENV JAVA_VERSION="7"
ENV JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF8"
#  ### common package installations
RUN sed 's/main$/main universe/' -i /etc/apt/sources.list
RUN :
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
RUN (apt-get update ;apt-get install --no-install-recommends libtool=2.4.2-1.7ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends pkg-config=0.26-1ubuntu4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libtesseract-dev=3.03.02-3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libopenblas-dev=0.2.8-6ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libblas-dev=1.2.20110419-7 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libatlas-dev=3.10.1-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libatlas-base-dev=3.10.1-4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends liblapack-dev=3.5.0-2ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends cmake=2.8.12.2-0ubuntu3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends zip=3.0-8 -y )
RUN (apt-get update ;apt-get install --no-install-recommends unzip=6.0-9ubuntu1.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends sox=14.4.1-3ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libsox-dev=14.4.1-3ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-6 -y )
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
#  ### package specific routines
RUN echo oracle-java$JAVA_VERSION-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends oracle-java$JAVA_VERSION-installer -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/oracle-jdk$JAVA_VERSION-installer
#  ### Thrift
RUN cd /usr/src \
 && wget "http://archive.apache.org/dist/thrift/$THRIFT_VERSION/thrift-$THRIFT_VERSION.tar.gz" \
 && tar xf thrift-$THRIFT_VERSION.tar.gz \
 && cd thrift-$THRIFT_VERSION \
 && ./configure \
 && make -j $THREADS \
 && make -j $THREADS install \
 && cd lib/py/ \
 && python setup.py install \
 && cd ../../lib/java/ \
 && ant \
 && cd ../../..
#  ### OpenCV
RUN mkdir -p /usr/src/opencv
RUN cd /usr/src/opencv \
 && git clone https://github.com/Itseez/opencv.git opencv-$OPENCV_VERSION \
 && cd opencv-$OPENCV_VERSION \
 && git checkout $OPENCV_VERSION \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j$THREADS \
 && make -j$THREADS install
#  ### Protobuf
RUN mkdir -p /usr/src/protobuf
RUN cd /usr/src/protobuf \
 && wget "https://github.com/google/protobuf/releases/download/v$PROTOBUF_VERSION/protobuf-$PROTOBUF_VERSION.tar.gz" \
 && tar xf protobuf-$PROTOBUF_VERSION.tar.gz \
 && cd protobuf-$PROTOBUF_VERSION \
 && ./configure \
 && make -j$THREADS \
 && make install
#  ### Caffe for djinn
RUN cd /usr/src \
 && git clone https://github.com/jhauswald/caffe.git \
 && cd caffe \
 && git checkout ipa \
 && cp Makefile.config.example Makefile.config \
 && make -j$THREADS \
 && make distribute
#  # install lucida
#   fixes some weird OE compiliation issue
RUN mkdir -p /usr/local/lucida
WORKDIR /usr/local/lucida
COPY . /usr/local/lucida
RUN cd lucida/ \
 && ./thrift-gen.sh
RUN /usr/bin/make all
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
