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
RUN apt-get install --no-install-recommends zlib1g-dev -y
RUN apt-get install --no-install-recommends libatlas3-base -y
RUN apt-get install --no-install-recommends python2.7-dev -y
RUN apt-get install --no-install-recommends libblas3 -y
RUN apt-get install --no-install-recommends libblas-dev -y
RUN apt-get install --no-install-recommends liblapack3 -y
RUN apt-get install --no-install-recommends liblapack-dev -y
RUN apt-get install --no-install-recommends libc6 -y
RUN apt-get install --no-install-recommends software-properties-common -y
RUN apt-get install --no-install-recommends gfortran -y
RUN apt-get install --no-install-recommends make -y
RUN apt-get install --no-install-recommends ant -y
RUN apt-get install --no-install-recommends gcc -y
RUN apt-get install --no-install-recommends g++ -y
RUN apt-get install --no-install-recommends wget -y
RUN apt-get install --no-install-recommends automake -y
RUN apt-get install --no-install-recommends git -y
RUN apt-get install --no-install-recommends curl -y
RUN apt-get install --no-install-recommends libboost-all-dev -y
RUN apt-get install --no-install-recommends libevent-dev -y
RUN apt-get install --no-install-recommends libdouble-conversion-dev -y
RUN apt-get install --no-install-recommends libtool -y
RUN apt-get install --no-install-recommends liblz4-dev -y
RUN apt-get install --no-install-recommends liblzma-dev -y
RUN apt-get install --no-install-recommends binutils-dev -y
RUN apt-get install --no-install-recommends libjemalloc-dev -y
RUN apt-get install --no-install-recommends pkg-config -y
RUN apt-get install --no-install-recommends libtesseract-dev -y
RUN apt-get install --no-install-recommends libopenblas-dev -y
RUN apt-get install --no-install-recommends libblas-dev -y
RUN apt-get install --no-install-recommends libatlas-dev -y
RUN apt-get install --no-install-recommends libatlas-base-dev -y
RUN apt-get install --no-install-recommends libiberty-dev -y
RUN apt-get install --no-install-recommends liblapack-dev -y
RUN apt-get install --no-install-recommends cmake -y
RUN apt-get install --no-install-recommends zip -y
RUN apt-get install --no-install-recommends unzip -y
RUN apt-get install --no-install-recommends sox -y
RUN apt-get install --no-install-recommends libsox-dev -y
RUN apt-get install --no-install-recommends autoconf -y
RUN apt-get install --no-install-recommends autoconf-archive -y
RUN apt-get install --no-install-recommends bison -y
RUN apt-get install --no-install-recommends swig -y
RUN apt-get install --no-install-recommends python-pip -y
RUN apt-get install --no-install-recommends subversion -y
RUN apt-get install --no-install-recommends libssl-dev -y
RUN apt-get install --no-install-recommends libprotoc-dev -y
RUN apt-get install --no-install-recommends supervisor -y
RUN apt-get install --no-install-recommends flac -y
RUN apt-get install --no-install-recommends gawk -y
RUN apt-get install --no-install-recommends imagemagick -y
RUN apt-get install --no-install-recommends libgflags-dev libgoogle-glog-dev liblmdb-dev -y
RUN apt-get install --no-install-recommends libleveldb-dev libsnappy-dev libhdf5-serial-dev -y
RUN apt-get install --no-install-recommends bc -y
RUN apt-get install --no-install-recommends python-numpy -y
RUN apt-get install --no-install-recommends flex -y
RUN apt-get install --no-install-recommends libkrb5-dev -y
RUN apt-get install --no-install-recommends libsasl2-dev -y
RUN apt-get install --no-install-recommends libnuma-dev -y
RUN apt-get install --no-install-recommends scons -y
# ### package specific routines
RUN echo oracle-java$JAVA_VERSION-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update \
 && apt-get install --no-install-recommends oracle-java$JAVA_VERSION-installer -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/oracle-jdk$JAVA_VERSION-installer
# # install MongoDB, OpenCV, Thrift, and FBThrift
RUN mkdir -p /usr/local/lucida
COPY ./usr/local/lucida/tools
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
