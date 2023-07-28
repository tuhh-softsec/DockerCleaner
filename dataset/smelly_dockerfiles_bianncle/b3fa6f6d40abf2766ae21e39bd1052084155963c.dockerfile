# ###
#  Builds the lucida base image
FROM ubuntu:14.04
# ### environment variables
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
# ### common package installations
RUN sed 's/main$/main universe/' -i /etc/apt/sources.list
RUN apt-get update
RUN apt-get install software-properties-common -y
RUN apt-get install gfortran -y
RUN apt-get install make -y
RUN apt-get install ant -y
RUN apt-get install gcc -y
RUN apt-get install g++ -y
RUN apt-get install wget -y
RUN apt-get install automake -y
RUN apt-get install git -y
RUN apt-get install curl -y
RUN apt-get install libboost-all-dev -y
RUN apt-get install libevent-dev -y
RUN apt-get install libtool -y
RUN apt-get install pkg-config -y
RUN apt-get install libtesseract-dev -y
RUN apt-get install libopenblas-dev -y
RUN apt-get install libblas-dev -y
RUN apt-get install libatlas-dev -y
RUN apt-get install libatlas-base-dev -y
RUN apt-get install liblapack-dev -y
RUN apt-get install cmake -y
RUN apt-get install zip -y
RUN apt-get install unzip -y
RUN apt-get install sox -y
RUN apt-get install libsox-dev -y
RUN apt-get install autoconf -y
RUN apt-get install bison -y
RUN apt-get install swig -y
RUN apt-get install python-pip -y
RUN apt-get install subversion -y
RUN apt-get install libssl-dev -y
RUN apt-get install libprotoc-dev -y
RUN apt-get install supervisor -y
RUN apt-get install flac -y
RUN apt-get install gawk -y
RUN apt-get install imagemagick -y
RUN apt-get install libgflags-dev libgoogle-glog-dev liblmdb-dev -y
RUN apt-get install libleveldb-dev libsnappy-dev libhdf5-serial-dev -y
RUN apt-get install bc -y
RUN apt-get install python-numpy -y
# ### package specific routines
RUN echo oracle-java$JAVA_VERSION-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update \
 && apt-get install oracle-java$JAVA_VERSION-installer -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/oracle-jdk$JAVA_VERSION-installer
# ### Thrift
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
# ### OpenCV
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
# ### Protobuf
RUN mkdir -p /usr/src/protobuf
RUN cd /usr/src/protobuf \
 && wget "https://github.com/google/protobuf/releases/download/v$PROTOBUF_VERSION/protobuf-$PROTOBUF_VERSION.tar.gz" \
 && tar xf protobuf-$PROTOBUF_VERSION.tar.gz \
 && cd protobuf-$PROTOBUF_VERSION \
 && ./configure \
 && make -j$THREADS \
 && make install
# ### Caffe for djinn
RUN cd /usr/src \
 && git clone https://github.com/jhauswald/caffe.git \
 && cd caffe \
 && git checkout ipa \
 && cp Makefile.config.example Makefile.config \
 && make -j$THREADS \
 && make distribute
# # install lucida
#  fixes some weird OE compiliation issue
RUN mkdir -p /usr/local/lucida
WORKDIR /usr/local/lucida
ADD . /usr/local/lucida
RUN cd lucida/ \
 && ./thrift-gen.sh
RUN /usr/bin/make all
