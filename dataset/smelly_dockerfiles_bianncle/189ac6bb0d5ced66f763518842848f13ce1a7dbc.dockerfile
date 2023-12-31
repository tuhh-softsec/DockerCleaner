#  Build stage for BerkeleyDB
FROM alpine:3.8 AS berkeleydb
RUN apk --no-cache add autoconf
RUN apk --no-cache add automake
RUN apk --no-cache add build-base
RUN apk --no-cache add libressl
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
#  Build stage for Bitcoin ABC
FROM alpine:3.8 AS bitcoin-abc
COPY --from=berkeleydb /opt /opt
RUN apk --no-cache add autoconf
RUN apk --no-cache add automake
RUN apk --no-cache add boost-dev
RUN apk --no-cache add build-base
RUN apk --no-cache add chrpath
RUN apk --no-cache add file
RUN apk --no-cache add gnupg
RUN apk --no-cache add libevent-dev
RUN apk --no-cache add libressl
RUN apk --no-cache add libressl-dev
RUN apk --no-cache add libtool
RUN apk --no-cache add linux-headers
RUN apk --no-cache add protobuf-dev
RUN apk --no-cache add zeromq-dev
RUN set -ex \
 && for key in 90C8019E36C2E964; do gpg --keyserver pgp.mit.edu --recv-keys "$key" || gpg --keyserver keyserver.pgp.com --recv-keys "$key" || gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" ; done
ENV BITCOIN_ABC_VERSION="0.17.1"
ENV BITCOIN_ABC_SHASUM="8baaedfb868fdbbde2a074b6396fb00dec28df35a0f6f29e9995778d3e08535a  bitcoin-abc-${BITCOIN_ABC_VERSION}.tar.gz"
ENV BITCOIN_ABC_PREFIX="/opt/bitcoin-abc-${BITCOIN_ABC_VERSION}"
RUN wget https://download.bitcoinabc.org/${BITCOIN_ABC_VERSION}/linux/src/bitcoin-abc-${BITCOIN_ABC_VERSION}.tar.gz
RUN echo "${BITCOIN_ABC_SHASUM}" | sha256sum -c
RUN tar -xzf *.tar.gz
WORKDIR /bitcoin-abc-${BITCOIN_ABC_VERSION}
RUN sed -i '/AC_PREREQ/a\AR_FLAGS=cr' src/univalue/configure.ac
RUN sed -i '/AX_PROG_CC_FOR_BUILD/a\AR_FLAGS=cr' src/secp256k1/configure.ac
RUN sed -i s:sys/fcntl.h:fcntl.h: src/compat.h
RUN ./autogen.sh
RUN ./configure LDFLAGS=-L`ls -d /opt/db* `/lib/ CPPFLAGS=-I`ls -d /opt/db* `/include/ --prefix=${BITCOIN_ABC_PREFIX} --mandir=/usr/share/man --disable-tests --disable-bench --disable-ccache --with-gui=no --with-utils --with-libs --with-daemon --without-seeder
RUN make -j4
RUN make install
RUN strip ${BITCOIN_ABC_PREFIX}/bin/bitcoin-cli
RUN strip ${BITCOIN_ABC_PREFIX}/bin/bitcoin-tx
RUN strip ${BITCOIN_ABC_PREFIX}/bin/bitcoind
RUN strip ${BITCOIN_ABC_PREFIX}/lib/libbitcoinconsensus.a
RUN strip ${BITCOIN_ABC_PREFIX}/lib/libbitcoinconsensus.so.0.0.0
#  Build stage for compiled artifacts
FROM alpine:3.8
LABEL maintainer.0="João Fonseca (@joaopaulofonseca)" \
      maintainer.1="Pedro Branco (@pedrobranco)" \
      maintainer.2="Rui Marinho (@ruimarinho)"
RUN adduser -S bitcoin
RUN apk --no-cache add boost boost-program_options curl libevent libressl libzmq su-exec
ENV BITCOIN_ABC_DATA="/home/bitcoin/.bitcoin"
ENV BITCOIN_ABC_VERSION="0.17.1"
ENV BITCOIN_ABC_PREFIX="/opt/bitcoin-abc-${BITCOIN_ABC_VERSION}"
ENV PATH="${BITCOIN_ABC_PREFIX}/bin:$PATH"
COPY --from=bitcoin-abc /opt /opt
COPY docker-entrypoint.sh /entrypoint.sh
VOLUME ["/home/bitcoin/.bitcoin"]
EXPOSE 8332/tcp 8333/tcp 18332/tcp 18333/tcp 18444/tcp
ENTRYPOINT ["/entrypoint.sh"]
CMD ["bitcoind"]
