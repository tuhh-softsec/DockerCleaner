ARG PG_VERSION
#  ###########################
#   Build tools binaries in separate image
#  ###########################
FROM golang:alpine AS tools
ENV TOOLS_VERSION="0.4.1"
RUN apk update \
 && apk add git=2.38.4-r1 --no-cache \
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
FROM postgres:${PG_VERSION}-alpine AS oldversions
ARG PG_VERSION
ARG OSS_ONLY
RUN set -ex \
 && apk add ca-certificates=20220614-r4 git=2.38.4-r1 openssl=3.0.8-r3 openssl-dev=3.0.8-r3 tar=1.34-r2 --no-cache --virtual .fetch-deps \
 && mkdir -p /build/ \
 && git clone https://github.com/timescale/timescaledb /build/timescaledb \
 && apk add coreutils=9.1-r0 dpkg-dev=1.21.9-r0 dpkg=1.21.9-r0 gcc=12.2.1_git20220924-r4 libc-dev=0.7.2-r3 make=4.3-r1 cmake=3.24.4-r0 util-linux-dev=2.38.1-r1 --no-cache --virtual .build-deps \
 && cd /build/timescaledb \
 && echo "if [ \"$( echo ${PG_VERSION} | cut -c1-2 ;)\" != \"11\" ] || [ "${OLD_VERSION}" \> "1.0.1" ]; then cd /build/timescaledb \
 && rm -fr build \
 && git reset HEAD --hard \
 && git fetch \
 && git checkout ${OLD_VERSION} \
 && ./bootstrap -DPROJECT_INSTALL_METHOD=\"docker\"${OSS_ONLY} \
 && cd build \
 && make install; fi" > ./build_old.sh \
 && chmod +x ./build_old.sh
#  ####
#   Add the latest previous version to the end of the list for each new build
#  ####
RUN OLD_VERSION=0.10.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=0.10.1 /build/timescaledb/build_old.sh
RUN OLD_VERSION=0.11.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=0.12.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=0.12.1 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.0.0-rc1 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.0.0-rc2 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.0.0-rc3 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.0.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.0.1 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.1.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.1.1 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.2.0 /build/timescaledb/build_old.sh
RUN OLD_VERSION=1.2.1 /build/timescaledb/build_old.sh
#   Cleanup
RUN rm -f $( pg_config --sharedir ;)/extension/timescaledb--*--*.sql \
 && rm -f $( pg_config --sharedir ;)/extension/timescaledb*mock*.sql \
 && KEEP_NUM_VERSIONS=8
FROM postgres:${PG_VERSION}-alpine
ARG OSS_ONLY
MAINTAINER Timescale https://www.timescale.com
#   Update list above to include previous versions when changing this
ENV TIMESCALEDB_VERSION="1.2.2"
COPY docker-entrypoint-initdb.d/* /docker-entrypoint-initdb.d/
COPY --from=tools /go/bin/* /usr/local/bin/
COPY --from=oldversions /usr/local/lib/postgresql/timescaledb-*.so /usr/local/lib/postgresql/
COPY --from=oldversions /usr/local/share/postgresql/extension/timescaledb--*.sql /usr/local/share/postgresql/extension/
RUN set -ex \
 && apk add ca-certificates=20220614-r4 git=2.38.4-r1 openssl=3.0.8-r3 openssl-dev=3.0.8-r3 tar=1.34-r2 --no-cache --virtual .fetch-deps \
 && mkdir -p /build/ \
 && git clone https://github.com/timescale/timescaledb /build/timescaledb \
 && apk add coreutils=9.1-r0 dpkg-dev=1.21.9-r0 dpkg=1.21.9-r0 gcc=12.2.1_git20220924-r4 libc-dev=0.7.2-r3 make=4.3-r1 cmake=3.24.4-r0 util-linux-dev=2.38.1-r1 --no-cache --virtual .build-deps \
 && cd /build/timescaledb \
 && rm -fr build \
 && git checkout ${TIMESCALEDB_VERSION} \
 && ./bootstrap -DPROJECT_INSTALL_METHOD="docker"${OSS_ONLY} \
 && cd build \
 && make install \
 && cd ~ \
 && apk del .fetch-deps .build-deps \
 && rm -rf /build \
 && sed -r -i "s/[#]*\s*(shared_preload_libraries)\s*=\s*'(.*)'/\1 = 'timescaledb,\2'/;s/,'/'/" /usr/local/share/postgresql/postgresql.conf.sample
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
