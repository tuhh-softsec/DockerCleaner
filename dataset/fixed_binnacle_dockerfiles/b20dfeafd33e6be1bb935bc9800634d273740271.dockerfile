#   Build stage for BerkeleyDB
FROM alpine:3.7 AS berkeleydb
RUN apk add autoconf=2.69-r0 --no-cache
RUN apk add automake=1.15.1-r0 --no-cache
RUN apk add build-base=0.5-r0 --no-cache
RUN apk add libressl=2.6.5-r0 --no-cache
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
#   Build stage for Bitcoin ABC
FROM alpine:3.7 AS bitcoin-abc
COPY --from=berkeleydb /opt /opt
RUN apk add autoconf=2.69-r0 --no-cache
RUN apk add automake=1.15.1-r0 --no-cache
RUN apk add boost-dev=1.62.0-r5 --no-cache
RUN apk add build-base=0.5-r0 --no-cache
RUN apk add chrpath=0.16-r1 --no-cache
RUN apk add file=5.32-r2 --no-cache
RUN apk add gnupg=2.2.3-r1 --no-cache
RUN apk add libevent-dev=2.1.8-r2 --no-cache
RUN apk add libressl=2.6.5-r0 --no-cache
RUN apk add libressl-dev=2.6.5-r0 --no-cache
RUN apk add libtool=2.4.6-r4 --no-cache
RUN apk add linux-headers=4.4.6-r2 --no-cache
RUN apk add protobuf-dev=3.4.1-r1 --no-cache
RUN apk add zeromq-dev=4.2.5-r1 --no-cache
RUN set -ex \
 && for key in 90C8019E36C2E964; do gpg --keyserver pgp.mit.edu --recv-keys "$key" || gpg --keyserver keyserver.pgp.com --recv-keys "$key" || gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" ; done
ENV BITCOIN_ABC_VERSION="0.15.1"
ENV BITCOIN_ABC_SHASUM="a8c36f5ff821c45ccef392805ffecc2dc841baae08b0f7c4534ec8152f23e541  bitcoin-abc-${BITCOIN_ABC_VERSION}.tar.gz"
ENV BITCOIN_ABC_PREFIX="/opt/bitcoin-abc-${BITCOIN_ABC_VERSION}"
RUN wget https://download.bitcoinabc.org/${BITCOIN_ABC_VERSION}/linux/src/bitcoin-abc-${BITCOIN_ABC_VERSION}.tar.gz
RUN echo "${BITCOIN_ABC_SHASUM}" | sha256sum -c
RUN tar -xzf *.tar.gz
WORKDIR /bitcoin-abc-${BITCOIN_ABC_VERSION}
RUN sed -i '/AC_PREREQ/a\AR_FLAGS=cr' src/univalue/configure.ac
RUN sed -i '/AX_PROG_CC_FOR_BUILD/a\AR_FLAGS=cr' src/secp256k1/configure.ac
RUN sed -i s:sys/fcntl.h:fcntl.h: src/compat.h
RUN ./autogen.sh
RUN ./configure LDFLAGS=-L`ls -d /opt/db* `/lib/ CPPFLAGS=-I`ls -d /opt/db* `/include/ --prefix=${BITCOIN_ABC_PREFIX} --mandir=/usr/share/man --disable-bench --disable-ccache --with-gui=no --with-utils --with-libs --with-daemon --without-seeder
RUN make -j4
RUN make install
RUN strip ${BITCOIN_ABC_PREFIX}/bin/bitcoin-cli
RUN strip ${BITCOIN_ABC_PREFIX}/bin/bitcoin-tx
RUN strip ${BITCOIN_ABC_PREFIX}/bin/bitcoind
RUN strip ${BITCOIN_ABC_PREFIX}/lib/libbitcoinconsensus.a
RUN strip ${BITCOIN_ABC_PREFIX}/lib/libbitcoinconsensus.so.0.0.0
#   Build stage for compiled artifacts
FROM alpine:3.7
LABEL maintainer.0="João Fonseca (@joaopaulofonseca)" \
      maintainer.1="Pedro Branco (@pedrobranco)" \
      maintainer.2="Rui Marinho (@ruimarinho)"
RUN adduser -S bitcoin
RUN apk add boost=1.62.0-r5 boost-program_options=1.62.0-r5 curl=7.61.1-r3 libevent=2.1.8-r2 libressl=2.6.5-r0 libzmq=4.2.5-r1 su-exec=0.2-r0 --no-cache
ENV BITCOIN_ABC_DATA="/home/bitcoin/.bitcoin"
ENV BITCOIN_ABC_VERSION="0.15.1"
ENV BITCOIN_ABC_PREFIX="/opt/bitcoin-abc-${BITCOIN_ABC_VERSION}"
ENV PATH="${BITCOIN_ABC_PREFIX}/bin:$PATH"
COPY --from=bitcoin-abc /opt /opt
COPY docker-entrypoint.sh /entrypoint.sh
VOLUME ["/home/bitcoin/.bitcoin"]
EXPOSE 8332/tcp 8333/tcp 18332/tcp 18333/tcp 18444/tcp
ENTRYPOINT ["/entrypoint.sh"]
CMD ["bitcoind"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
