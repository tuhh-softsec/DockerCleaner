#   Build stage for BerkeleyDB
FROM alpine AS berkeleydb
RUN apk add autoconf=2.71-r1 --no-cache
RUN apk add automake=1.16.5-r1 --no-cache
RUN apk add build-base=0.5-r3 --no-cache
RUN apk add libressl=3.6.2-r0 --no-cache
ENV BERKELEYDB_VERSION="db-4.8.30.NC"
ENV BERKELEYDB_PREFIX="/opt/${BERKELEYDB_VERSION}"
RUN wget https://download.oracle.com/berkeley-db/${BERKELEYDB_VERSION}.tar.gz
RUN tar -xzf *.tar.gz
RUN sed s/__atomic_compare_exchange/__atomic_compare_exchange_db/g -i ${BERKELEYDB_VERSION}/dbinc/atomic.h
RUN mkdir -p ${BERKELEYDB_PREFIX}
WORKDIR /${BERKELEYDB_VERSION}/build_unix
RUN ../dist/configure --enable-cxx --disable-shared --with-pic --prefix=${BERKELEYDB_PREFIX}
RUN make -j4
RUN make install
RUN rm -rf ${BERKELEYDB_PREFIX}/docs
#   Build stage for Dash
FROM alpine AS dash
COPY --from=berkeleydb /opt /opt
RUN apk add autoconf=2.71-r1 --no-cache
RUN apk add automake=1.16.5-r1 --no-cache
RUN apk add boost-dev=1.80.0-r3 --no-cache
RUN apk add build-base=0.5-r3 --no-cache
RUN apk add chrpath=0.16-r3 --no-cache
RUN apk add file=5.43-r0 --no-cache
RUN apk add gnupg=2.2.40-r0 --no-cache
RUN apk add libevent-dev=2.1.12-r5 --no-cache
RUN apk add libressl=3.6.2-r0 --no-cache
RUN apk add libressl-dev=3.6.2-r0 --no-cache
RUN apk add libsodium-dev=1.0.18-r2 --no-cache
RUN apk add libtool=2.4.7-r1 --no-cache
RUN apk add linux-headers=5.19.5-r0 --no-cache
RUN apk add protobuf-dev=3.21.9-r0 --no-cache
RUN apk add zeromq-dev=4.3.4-r1 --no-cache
RUN set -ex \
 && for key in 38EE12EB597B4FC0; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver pgp.mit.edu --recv-keys "$key" || gpg --keyserver keyserver.pgp.com --recv-keys "$key" || gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" ; done
ENV DASH_VERSION="0.12.2.3"
ENV DASH_PREFIX="/opt/dash-${DASH_VERSION}"
ENV DASH_SHASUM="5347351483ce39d1dd0be4d93ee19aba1a6b02bc7f90948b4eea4466ad79d1c3  v${DASH_VERSION}.tar.gz"
RUN wget https://github.com/dashpay/dash/archive/v${DASH_VERSION}.tar.gz
RUN echo "${DASH_SHASUM}" | sha256sum -c
RUN tar -xzf *.tar.gz
RUN ls -la
WORKDIR /dash-${DASH_VERSION}
RUN sed -i '/AC_PREREQ/a\AR_FLAGS=cr' src/univalue/configure.ac
RUN sed -i '/AX_PROG_CC_FOR_BUILD/a\AR_FLAGS=cr' src/secp256k1/configure.ac
RUN sed -i s:sys/fcntl.h:fcntl.h: src/compat.h
RUN ./autogen.sh
RUN ./configure LDFLAGS=-L`ls -d /opt/db* `/lib/ CPPFLAGS=-I`ls -d /opt/db* `/include/ --prefix=${DASH_PREFIX} --mandir=/usr/share/man --disable-tests --disable-bench --disable-ccache --with-gui=no --with-utils --with-libs --with-daemon
RUN make -j4
RUN make install
RUN strip ${DASH_PREFIX}/bin/dash-cli
RUN strip ${DASH_PREFIX}/bin/dash-tx
RUN strip ${DASH_PREFIX}/bin/dashd
RUN strip ${DASH_PREFIX}/lib/libdashconsensus.a
RUN strip ${DASH_PREFIX}/lib/libdashconsensus.so.0.0.0
#   Build stage for compiled artifacts
FROM alpine
LABEL maintainer.0="João Fonseca (@joaopaulofonseca)" \
      maintainer.1="Pedro Branco (@pedrobranco)" \
      maintainer.2="Rui Marinho (@ruimarinho)"
RUN adduser -S dash
RUN apk add boost boost-program_options curl=7.88.1-r1 libevent=2.1.12-r5 libressl=3.6.2-r0 libzmq=4.3.4-r1 su-exec=0.2-r2 --no-cache
ENV DASH_DATA="/home/dash/.dashcore"
ENV DASH_VERSION="0.12.2.3"
ENV DASH_PREFIX="/opt/dash-${DASH_VERSION}"
ENV PATH="${DASH_PREFIX}/bin:$PATH"
COPY --from=dash /opt /opt
COPY docker-entrypoint.sh /entrypoint.sh
VOLUME ["/home/dash/.dashcore"]
EXPOSE 9998/tcp 9999/tcp 18332/tcp 19998/tcp 19999/tcp
ENTRYPOINT ["/entrypoint.sh"]
CMD ["dashd"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
