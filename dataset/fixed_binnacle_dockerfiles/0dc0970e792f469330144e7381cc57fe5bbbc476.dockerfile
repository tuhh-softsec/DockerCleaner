#   Build stage for BerkeleyDB
FROM alpine AS berkeleydb
RUN sed -i 's/http\:\/\/dl-cdn.alpinelinux.org/https\:\/\/alpine.global.ssl.fastly.net/g' /etc/apk/repositories
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
#   Build stage for Bitcoin Core
FROM alpine AS bitcoin-core
COPY --from=berkeleydb /opt /opt
RUN sed -i 's/http\:\/\/dl-cdn.alpinelinux.org/https\:\/\/alpine.global.ssl.fastly.net/g' /etc/apk/repositories
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
RUN apk add libtool=2.4.7-r1 --no-cache
RUN apk add linux-headers=5.19.5-r0 --no-cache
RUN apk add protobuf-dev=3.21.9-r0 --no-cache
RUN apk add zeromq-dev=4.3.4-r1 --no-cache
RUN set -ex \
 && for key in 90C8019E36C2E964; do gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" || gpg --batch --keyserver pgp.mit.edu --recv-keys "$key" || gpg --batch --keyserver keyserver.pgp.com --recv-keys "$key" || gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" ; done
ENV BITCOIN_VERSION="0.18.0"
ENV BITCOIN_PREFIX="/opt/bitcoin-${BITCOIN_VERSION}"
RUN wget https://bitcoin.org/bin/bitcoin-core-${BITCOIN_VERSION}/SHA256SUMS.asc
RUN wget https://bitcoin.org/bin/bitcoin-core-${BITCOIN_VERSION}/bitcoin-${BITCOIN_VERSION}.tar.gz
RUN gpg --verify SHA256SUMS.asc
RUN grep " bitcoin-${BITCOIN_VERSION}.tar.gz$" SHA256SUMS.asc | sha256sum -c -
RUN tar -xzf *.tar.gz
WORKDIR /bitcoin-${BITCOIN_VERSION}
RUN sed -i '/AC_PREREQ/a\AR_FLAGS=cr' src/univalue/configure.ac
RUN sed -i '/AX_PROG_CC_FOR_BUILD/a\AR_FLAGS=cr' src/secp256k1/configure.ac
RUN sed -i s:sys/fcntl.h:fcntl.h: src/compat.h
RUN ./autogen.sh
RUN ./configure LDFLAGS=-L`ls -d /opt/db* `/lib/ CPPFLAGS=-I`ls -d /opt/db* `/include/ --prefix=${BITCOIN_PREFIX} --mandir=/usr/share/man --disable-tests --disable-bench --disable-ccache --with-gui=no --with-utils --with-libs --with-daemon
RUN make -j4
RUN make install
RUN strip ${BITCOIN_PREFIX}/bin/bitcoin-cli
RUN strip ${BITCOIN_PREFIX}/bin/bitcoin-tx
RUN strip ${BITCOIN_PREFIX}/bin/bitcoind
RUN strip ${BITCOIN_PREFIX}/lib/libbitcoinconsensus.a
RUN strip ${BITCOIN_PREFIX}/lib/libbitcoinconsensus.so.0.0.0
#   Build stage for compiled artifacts
FROM alpine
LABEL maintainer.0="João Fonseca (@joaopaulofonseca)" \
      maintainer.1="Pedro Branco (@pedrobranco)" \
      maintainer.2="Rui Marinho (@ruimarinho)"
RUN adduser -S bitcoin
RUN sed -i 's/http\:\/\/dl-cdn.alpinelinux.org/https\:\/\/alpine.global.ssl.fastly.net/g' /etc/apk/repositories
RUN apk add boost boost-program_options libevent=2.1.12-r5 libressl=3.6.2-r0 libzmq=4.3.4-r1 su-exec=0.2-r2 --no-cache
ENV BITCOIN_DATA="/home/bitcoin/.bitcoin"
ENV BITCOIN_VERSION="0.18.0"
ENV BITCOIN_PREFIX="/opt/bitcoin-${BITCOIN_VERSION}"
ENV PATH="${BITCOIN_PREFIX}/bin:$PATH"
COPY --from=bitcoin-core /opt /opt
COPY docker-entrypoint.sh /entrypoint.sh
VOLUME ["/home/bitcoin/.bitcoin"]
EXPOSE 8332/tcp 8333/tcp 18332/tcp 18333/tcp 18444/tcp
ENTRYPOINT ["/entrypoint.sh"]
CMD ["bitcoind"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
