#   Licensed to the Apache Software Foundation (ASF) under one
#   or more contributor license agreements. See the NOTICE file
#   distributed with this work for additional information
#   regarding copyright ownership. The ASF licenses this file
#   to you under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in compliance
#   with the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing,
#   software distributed under the License is distributed on an
#   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#   KIND, either express or implied. See the License for the
#   specific language governing permissions and limitations
#   under the License.
#
#   First stage: the build environment
#   Edge required for rocksdb
FROM alpine:3.8 AS builder
MAINTAINER Apache NiFi <dev@nifi.apache.org>
ARG UID
ARG GID
ARG MINIFI_VERSION
ARG MINIFI_SOURCE_CODE
#   Install the system dependencies needed for a build
RUN apk --update --no-cache upgrade \
 && apk add gcc=6.4.0-r9 g++=6.4.0-r9 make=4.2.1-r2 bison=3.0.4-r1 flex=2.6.4-r1 flex-dev=2.6.4-r1 maven=3.5.4-r1 openjdk8-jre-base=8.275.01-r0 openjdk8=8.275.01-r0 autoconf=2.69-r2 libtool=2.4.6-r5 wget=1.20.3-r0 gdb=8.0.1-r6 musl-dev=1.1.19-r11 boost-dev=1.66.0-r0 vim=8.1.1365-r0 util-linux-dev=2.32-r0 curl-dev=7.61.1-r3 cmake=3.11.1-r2 git=2.18.4-r0 nss=3.36.1-r1 nss-dev=3.36.1-r1 unzip=6.0-r6 gpsd-dev=3.17-r2 libressl-dev=2.7.5-r0 zlib-dev=1.2.11-r1 bzip2-dev=1.0.6-r7 python3-dev=3.6.9-r1 --update --no-cache
ENV USER="minificpp"
ENV MINIFI_BASE_DIR="/opt/minifi"
#   Setup minificpp user
RUN addgroup -g $GID $USER \
 && adduser -u $UID -D -G $USER -g "" $USER
RUN mkdir -p $MINIFI_BASE_DIR
ENV JAVA_HOME="/usr/lib/jvm/default-jvm"
ENV PATH="$PATH:/usr/lib/jvm/default-jvm/bin"
COPY $MINIFI_SOURCE_CODE $MINIFI_BASE_DIR
RUN chown -R $USER:$USER $MINIFI_BASE_DIR
USER $USER
ENV MINIFI_HOME="$MINIFI_BASE_DIR/nifi-minifi-cpp-$MINIFI_VERSION"
#   Perform the build
RUN cd $MINIFI_BASE_DIR \
 && mkdir build \
 && cd build \
 && cmake -DOPENSSL_FORCE_SHARED=true -DDISABLE_JEMALLOC=ON -DSTATIC_BUILD= -DSKIP_TESTS=true -DENABLE_JNI=ON .. \
 && make -j8 package \
 && tar -xzvf $MINIFI_BASE_DIR/build/nifi-minifi-cpp-$MINIFI_VERSION-bin.tar.gz -C $MINIFI_BASE_DIR
#   Second stage: the runtime image
#   Edge required for rocksdb
FROM alpine:3.8
ARG UID
ARG GID
ARG MINIFI_VERSION
ARG MINIFI_SOURCE_CODE
#   Add testing repo for rocksdb
RUN echo 'http://dl-cdn.alpinelinux.org/alpine/edge/testing' >> /etc/apk/repositories
RUN apk --update --no-cache upgrade \
 && apk add util-linux=2.32-r0 curl=7.61.1-r3 unzip=6.0-r6 gpsd=3.17-r2 openjdk8-jre-base=8.275.01-r0 openjdk8=8.275.01-r0 nss=3.36.1-r1 nss-dev=3.36.1-r1 libressl=2.7.5-r0 python3=3.6.9-r1 zlib=1.2.11-r1 --update --no-cache
#   Start MiNiFi CPP in the foreground
ENV USER="minificpp"
ENV MINIFI_BASE_DIR="/opt/minifi"
ENV MINIFI_HOME="$MINIFI_BASE_DIR/nifi-minifi-cpp-$MINIFI_VERSION"
ENV JAVA_HOME="/usr/lib/jvm/default-jvm"
ENV PATH="$PATH:/usr/lib/jvm/default-jvm/bin"
RUN addgroup -g $GID $USER \
 && adduser -u $UID -D -G $USER -g "" $USER
RUN mkdir -p $MINIFI_BASE_DIR
#   Copy built minifi distribution from builder
COPY --from=builder ${MINIFI_HOME} ${MINIFI_HOME}
RUN chown -R ${USER}:${USER} /opt/minifi
USER $USER
WORKDIR ${MINIFI_HOME}
CMD ./bin/minifi.sh run
# Please add your HEALTHCHECK here!!!
