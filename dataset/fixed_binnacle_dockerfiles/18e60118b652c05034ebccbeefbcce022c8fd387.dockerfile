FROM ubuntu:14.04
MAINTAINER EMC{code} <http://community.emccode.com>
#   Install Dependencies
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -qy )
RUN add-apt-repository ppa:george-edison55/cmake-3.x
RUN :
RUN apt-cache policy cmake
RUN (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-6 automake=1:1.14.1-2ubuntu1 build-essential=11.6ubuntu6 ca-certificates=20170717~14.04.2 cmake=3.2.2-2~ubuntu14.04.1~ppa1 curl=7.35.0-1ubuntu2.20 g++=4:4.8.2-1ubuntu6 git-core=1:1.9.1-1ubuntu0.10 gdb=7.7.1-0ubuntu5~14.04.3 heimdal-clients=1.6~git20131207+dfsg-1ubuntu1.2 libapr1-dev=1.5.0-1 libboost-dev=1.54.0.1ubuntu1 libcurl4-nss-dev=7.35.0-1ubuntu2.20 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libgoogle-glog-dev=0.3.3-1 libprotobuf-dev=2.5.0-9ubuntu1 libpython-dev=2.7.5-5ubuntu3 libsasl2-dev=2.1.25.dfsg1-17build1 libsasl2-modules-gssapi-heimdal=2.1.25.dfsg1-17build1 libssl-dev=1.0.1f-1ubuntu2.27 libsvn-dev=1.8.8-1ubuntu3.3 libtool=2.4.2-1.7ubuntu1 make=3.81-8.2ubuntu3 python=2.7.5-5ubuntu3 python2.7=2.7.6-8ubuntu0.5 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 python-protobuf=2.5.0-9ubuntu1 python-setuptools=3.3-1ubuntu2 unzip=6.0-9ubuntu1.5 wget=1.15-1ubuntu1.14.04.5 -qy )
RUN pip install pip==23.1 --upgrade
RUN pip install pyopenssl==23.1.1 ndg-httpsclient==0.5.1 pyasn1==0.4.8 --upgrade
#   Install the picojson headers
RUN wget https://raw.githubusercontent.com/kazuho/picojson/v1.3.0/picojson.h -O /usr/local/include/picojson.h
#   Prepare to build Mesos
RUN mkdir -p /mesos
RUN mkdir -p /tmp
RUN mkdir -p /usr/share/java/
RUN wget http://search.maven.org/remotecontent?filepath=com/google/protobuf/protobuf-java/2.6.1/protobuf-java-2.6.1.jar -O protobuf.jar
RUN mv protobuf.jar /usr/share/java/
WORKDIR /mesos
ENV LD_LIBRARY_PATH="/usr/local/lib"
ENV PROTOBUF_DEST="/mesos/3rdparty/libprocess/3rdparty"
#  ENV MESOS_VERSION=0.23.1 GIT_CHECKOUT_HASH=a9ea8b1 PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=0.24.2 GIT_CHECKOUT_HASH=e2eb20b PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=0.25.1 GIT_CHECKOUT_HASH=c46b9c8 PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=0.26.1 GIT_CHECKOUT_HASH=a041e3a PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=0.26.2 GIT_CHECKOUT_HASH=5edc7ba PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=0.27.2 GIT_CHECKOUT_HASH=3c9ec4a PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=0.27.3 GIT_CHECKOUT_HASH=68dd1f6 PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=0.28.2 GIT_CHECKOUT_HASH=ceecad6 PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=0.28.3 GIT_CHECKOUT_HASH=52a0b0a PROTOBUF_VERSION=2.5.0 PROTOBUF_SOURCE=/mesos/3rdparty/libprocess/3rdparty
#  ENV MESOS_VERSION=1.0.0 GIT_CHECKOUT_HASH=c9b7058 PROTOBUF_VERSION=2.6.1 PROTOBUF_SOURCE=/mesos/3rdparty
#  ENV MESOS_VERSION=1.0.1 GIT_CHECKOUT_HASH=3611eb0 PROTOBUF_VERSION=2.6.1 PROTOBUF_SOURCE=/mesos/3rdparty
#  ENV MESOS_VERSION=1.1.0 GIT_CHECKOUT_HASH=a44b077 PROTOBUF_VERSION=2.6.1 PROTOBUF_SOURCE=/mesos/3rdparty
#  ENV MESOS_VERSION=1.2.0 GIT_CHECKOUT_HASH=de306b5 PROTOBUF_VERSION=2.6.1 PROTOBUF_SOURCE=/mesos/3rdparty
ENV MESOS_VERSION="1.2.1" \
    GIT_CHECKOUT_HASH="7a0cc55" \
    PROTOBUF_VERSION="2.6.1" \
    PROTOBUF_SOURCE="/mesos/3rdparty"
ENV ISOLATOR_VERSION="$MESOS_VERSION"
ENV GIT_SOURCE="git://git.apache.org/mesos.git"
#   Clone Mesos
RUN git clone $GIT_SOURCE /mesos
RUN git checkout $GIT_CHECKOUT_HASH
RUN git log -n 1
#   Install protobuf
RUN mkdir -p $PROTOBUF_DEST
RUN cd ${PROTOBUF_SOURCE} \
 && tar -xzvf protobuf-${PROTOBUF_VERSION}.tar.gz -C ${PROTOBUF_DEST} \
 && cd ${PROTOBUF_DEST}/protobuf-${PROTOBUF_VERSION}/ \
 && ./configure --prefix=/usr \
 && make -j 2 \
 && make install
#   Bootstrap
RUN ./bootstrap
#   Configure
RUN mkdir build \
 && cd build \
 && ../configure --disable-java --disable-optimize --without-included-zookeeper --with-glog=/usr/local --with-protobuf=/usr --with-boost=/usr/local
#   Build Mesos
RUN cd build \
 && make -j 2 install
#   Install python eggs
RUN easy_install /mesos/build/src/python/dist/mesos.interface-*.egg
#  RUN easy_install /mesos/build/src/python/dist/mesos.native-*.egg
#   This image builds mesos and retains the resulting headers and binaries.
#   It is intended to support mesos isolator module development and production builds.
#   A source code tree for the isolator should be mounted at /isolator if using the default entrypoint.
VOLUME ["/isolator"]
COPY ./docker-isolator-entrypoint.sh /
ENTRYPOINT ["/docker-isolator-entrypoint.sh"]
CMD ["/usr/bin/make", "all"]
#   To build Docker image:
#   docker build -t <docker-user-name>/mesos-build-module-dev:<ver> -f Dockerfile-mesos-modules-dev .
#   default COMMAND simply builds isolator
#   use it like this:
#   docker run -ti -v <path-to-git-clone>/mesos-module-dvdi/isolator/:/isolator <docker-user-name>/mesos-build-module-dev:<ver>
#   to extract the newly built .so to the mounted source directory volume mount - you should over-ride the default CMD like this:
#   docker run -ti -v <path-to-git-clone>/mesos-module-dvdi/isolator/:/isolator <docker-user-name>/mesos-build-module-dev:<ver> /bin/bash -c  '/usr/bin/make all && cp /isolator/build/.libs/libmesos_dvdi_isolator-${ISOLATOR_VERSION}.so /isolator/'
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
