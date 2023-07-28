ARG PG_VERSION
#  ###########################
#   Build tools binaries in separate image
#  ###########################
ARG GO_VERSION=1.12.6
FROM golang:${GO_VERSION}-alpine AS tools
ENV TOOLS_VERSION="0.6.0"
RUN apk update \
 && apk add git --no-cache \
 && mkdir -p ${GOPATH}/src/github.com/timescale/ \
 && cd ${GOPATH}/src/github.com/timescale/ \
 && git clone https://github.com/timescale/timescaledb-tune.git \
 && git clone https://github.com/timescale/timescaledb-parallel-copy.git \
 && cd timescaledb-tune/cmd/timescaledb-tune \
 && git fetch \
 && git checkout --quiet $( git describe --abbrev=0 ;) \
 && go get -d -v \
 && go build -o /go/bin/timescaledb-tune \
 && cd ${GOPATH}/src/github.com/timescale/timescaledb-parallel-copy/cmd/timescaledb-parallel-copy \
 && git fetch \
 && git checkout --quiet $( git describe --abbrev=0 ;) \
 && go get -d -v \
 && go build -o /go/bin/timescaledb-parallel-copy
#  ###########################
#   Build old versions in a separate stage
#  ###########################
ARG PG_VERSION
FROM bitnami/postgresql:${PG_VERSION} AS oldversions
ARG PG_VERSION
USER 0
RUN set -ex \
 && mkdir -p /var/lib/apt/lists/partial \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential libssl-dev git dpkg-dev gcc libc-dev make cmake wget -y \
 && mkdir -p /build/ \
 && git clone https://github.com/timescale/timescaledb /build/timescaledb \
 && cd /build/timescaledb \
 && echo "if [ \"$( echo ${PG_VERSION} | cut -c1-2 ;)\" != \"11\" ] || [ "${OLD_VERSION}" \> "1.0.1" ]; then cd /build/timescaledb \
 && rm -fr build \
 && git reset HEAD --hard \
 && git fetch \
 && git checkout ${OLD_VERSION} \
 && ./bootstrap -DPROJECT_INSTALL_METHOD=\"docker\" \
 && cd build \
 && make install; fi" > ./build_old.sh \
 && chmod +x ./build_old.sh
#  ####
#   Add the latest previous version to the end of the list for each new build
#  ####
RUN OLD_VERSION=1.1.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.1.1 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.2.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.2.1 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.2.2 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.3.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.3.1 /build/timescaledb/build_old.sh
#   Cleanup
RUN echo $( pg_config --pkglibdir ;) \
 && rm -f $( pg_config --sharedir ;)/extension/timescaledb--*--*.sql \
 && rm -f $( pg_config --sharedir ;)/extension/timescaledb*mock*.sql \
 && KEEP_NUM_VERSIONS=5 \
 && rm -f $( ls -1 $( pg_config --pkglibdir ;)/timescaledb-*.so | head -n -${KEEP_NUM_VERSIONS} ;) \
 && rm -f $( ls -1 $( pg_config --sharedir ;)/extension/timescaledb-*.sql | head -n -${KEEP_NUM_VERSIONS} ;) \
 && cd ~ \
 && apt-get autoremove --purge -y build-essential libssl-dev dpkg-dev gcc libc-dev make cmake \
 && apt-get clean -y \
 && rm -rf "${HOME}/.cache" /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  ###########################
#   Now build image and copy in tools
#  ###########################
ARG PG_VERSION
FROM bitnami/postgresql:${PG_VERSION}
ARG PG_VERSION
MAINTAINER Timescale https://www.timescale.com
ENV TIMESCALEDB_VERSION="1.3.2"
COPY docker-entrypoint-initdb.d/* /docker-entrypoint-initdb.d/
COPY --from=tools /go/bin/* /usr/local/bin/
COPY --from=oldversions /opt/bitnami/postgresql/lib/timescaledb-*.so /usr/local/lib/postgresql/
COPY --from=oldversions /opt/bitnami/postgresql/share/extension/timescaledb--*.sql /usr/local/share/postgresql/extension/
USER 0
RUN set -ex \
 && mkdir -p /var/lib/apt/lists/partial \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential libssl-dev git dpkg-dev gcc libc-dev make cmake wget -y \
 && mkdir -p /build/ \
 && git clone https://github.com/timescale/timescaledb /build/timescaledb \
 && cd /build/timescaledb \
 && rm -fr build \
 && git checkout ${TIMESCALEDB_VERSION} \
 && ./bootstrap -DPROJECT_INSTALL_METHOD="docker" \
 && cd build \
 && make install \
 && cd ~ \
 && apt-get autoremove --purge -y build-essential libssl-dev dpkg-dev gcc libc-dev make cmake \
 && apt-get clean -y \
 && rm -rf "${HOME}/.cache" /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN sed -r -i "s/[#]*\s*(shared_preload_libraries)\s*=\s*'(.*)'/\1 = 'timescaledb,\2'/;s/,'/'/" /opt/bitnami/postgresql/share/postgresql.conf.sample
USER 1001
# Please add your HEALTHCHECK here!!!
