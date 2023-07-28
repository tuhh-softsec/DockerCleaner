# ###
#  Builds the lucida base image
FROM ubuntu:14.04
# ### environment variables
ENV LUCIDAROOT="/usr/local/lucida/lucida"
ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV OPENCV_VERSION="2.4.9"
ENV THRIFT_VERSION="0.9.3"
ENV THREADS="4"
ENV PROTOBUF_VERSION="2.5.0"
ENV JAVA_VERSION="8"
ENV JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF8"
# ### common package installations
RUN sed 's/main$/main universe/' -i /etc/apt/sources.list
RUN apt-get update
RUN apt-get install zlib1g-dev -y
RUN apt-get install libatlas3-base -y
RUN apt-get install python2.7-dev -y
RUN apt-get install libblas3 -y
RUN apt-get install libblas-dev -y
RUN apt-get install liblapack3 -y
RUN apt-get install liblapack-dev -y
RUN apt-get install libc6 -y
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
RUN apt-get install libdouble-conversion-dev -y
RUN apt-get install libtool -y
RUN apt-get install liblz4-dev -y
RUN apt-get install liblzma-dev -y
RUN apt-get install binutils-dev -y
RUN apt-get install libjemalloc-dev -y
RUN apt-get install pkg-config -y
RUN apt-get install libtesseract-dev -y
RUN apt-get install libopenblas-dev -y
RUN apt-get install libblas-dev -y
RUN apt-get install libatlas-dev -y
RUN apt-get install libatlas-base-dev -y
RUN apt-get install libiberty-dev -y
RUN apt-get install liblapack-dev -y
RUN apt-get install cmake -y
RUN apt-get install zip -y
RUN apt-get install unzip -y
RUN apt-get install sox -y
RUN apt-get install libsox-dev -y
RUN apt-get install autoconf -y
RUN apt-get install autoconf-archive -y
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
RUN apt-get install flex -y
RUN apt-get install libkrb5-dev -y
RUN apt-get install libsasl2-dev -y
RUN apt-get install libnuma-dev -y
RUN apt-get install scons -y
# ### package specific routines
RUN echo oracle-java$JAVA_VERSION-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update \
 && apt-get install oracle-java$JAVA_VERSION-installer -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/oracle-jdk$JAVA_VERSION-installer
# # install MongoDB, OpenCV, Thrift, and FBThrift
RUN mkdir -p /usr/local/lucida
ADD . /usr/local/lucida/tools
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
