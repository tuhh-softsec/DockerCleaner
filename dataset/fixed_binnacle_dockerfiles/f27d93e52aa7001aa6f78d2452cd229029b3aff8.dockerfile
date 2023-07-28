FROM alpine:3.7 AS builder
RUN apk add ca-certificates=20190108-r0 autoconf=2.69-r0 automake=1.15.1-r0 build-base=0.5-r0 libressl=2.6.5-r0 libtool=2.4.6-r4 gmp-dev=6.1.2-r1 python python-dev python3=3.6.9-r1 sqlite-dev=3.25.3-r2 wget=1.20.3-r0 git=2.15.4-r0 file=5.32-r2 gnupg=2.2.3-r1 swig=3.0.12-r1 zlib-dev=1.2.11-r1 --no-cache
WORKDIR /opt
ARG BITCOIN_VERSION=0.17.0
ENV BITCOIN_TARBALL="bitcoin-${BITCOIN_VERSION}-x86_64-linux-gnu.tar.gz"
ENV BITCOIN_URL="https://bitcoincore.org/bin/bitcoin-core-$BITCOIN_VERSION/$BITCOIN_TARBALL"
ENV BITCOIN_ASC_URL="https://bitcoincore.org/bin/bitcoin-core-$BITCOIN_VERSION/SHA256SUMS.asc"
ENV BITCOIN_PGP_KEY="01EA5486DE18A882D4C2684590C8019E36C2E964"
RUN mkdir /opt/bitcoin \
 && cd /opt/bitcoin \
 && wget -qO $BITCOIN_TARBALL "$BITCOIN_URL" \
 && gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys "$BITCOIN_PGP_KEY" \
 && wget -qO bitcoin.asc "$BITCOIN_ASC_URL" \
 && gpg --verify bitcoin.asc \
 && grep $BITCOIN_TARBALL bitcoin.asc | tee SHA256SUMS.asc \
 && sha256sum -c SHA256SUMS.asc \
 && BD=bitcoin-$BITCOIN_VERSION/bin \
 && tar -xzvf $BITCOIN_TARBALL $BD/bitcoin-cli --strip-components=1 \
 && rm $BITCOIN_TARBALL
ENV LITECOIN_VERSION="0.16.3"
ENV LITECOIN_PGP_KEY="FE3348877809386C"
ENV LITECOIN_URL="https://download.litecoin.org/litecoin-${LITECOIN_VERSION}/linux/litecoin-${LITECOIN_VERSION}-x86_64-linux-gnu.tar.gz"
ENV LITECOIN_ASC_URL="https://download.litecoin.org/litecoin-${LITECOIN_VERSION}/linux/litecoin-${LITECOIN_VERSION}-linux-signatures.asc"
#   install litecoin binaries
RUN mkdir /opt/litecoin \
 && cd /opt/litecoin \
 && wget -qO litecoin.tar.gz "$LITECOIN_URL" \
 && gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys "$LITECOIN_PGP_KEY" \
 && wget -qO litecoin.asc "$LITECOIN_ASC_URL" \
 && gpg --verify litecoin.asc \
 && BD=litecoin-$LITECOIN_VERSION/bin \
 && tar -xzvf litecoin.tar.gz $BD/litecoin-cli --strip-components=1 --exclude=*-qt \
 && rm litecoin.tar.gz
ENV LIGHTNINGD_VERSION="master"
WORKDIR /opt/lightningd
COPY . /tmp/lightning
RUN git clone --recursive /tmp/lightning . \
 && git checkout $( git --work-tree=/tmp/lightning --git-dir=/tmp/lightning/.git rev-parse HEAD ;)
ARG DEVELOPER=0
RUN ./configure --prefix=/tmp/lightning_install \
 && make -j3 DEVELOPER=${DEVELOPER} \
 && make install
FROM alpine:3.7
RUN apk add gmp-dev=6.1.2-r1 sqlite-dev=3.25.3-r2 inotify-tools=3.14-r2 socat=1.7.3.2-r3 bash=4.4.19-r1 zlib-dev=1.2.11-r1 tini=0.16.1-r0 --no-cache
ENV GLIBC_VERSION="2.27-r0"
ENV GLIBC_SHA256="938bceae3b83c53e7fa9cc4135ce45e04aae99256c5e74cf186c794b97473bc7"
ENV GLIBCBIN_SHA256="3a87874e57b9d92e223f3e90356aaea994af67fb76b71bb72abfb809e948d0d6"
#   Download and install glibc (https://github.com/jeanblanchard/docker-alpine-glibc/blob/master/Dockerfile)
RUN apk add curl=7.61.1-r3 --update \
 && curl -Lo /etc/apk/keys/sgerrand.rsa.pub https://github.com/sgerrand/alpine-pkg-glibc/releases/download/$GLIBC_VERSION/sgerrand.rsa.pub \
 && curl -Lo glibc.apk "https://github.com/sgerrand/alpine-pkg-glibc/releases/download/${GLIBC_VERSION}/glibc-${GLIBC_VERSION}.apk" \
 && echo "$GLIBC_SHA256 glibc.apk" | sha256sum -c - \
 && curl -Lo glibc-bin.apk "https://github.com/sgerrand/alpine-pkg-glibc/releases/download/${GLIBC_VERSION}/glibc-bin-${GLIBC_VERSION}.apk" \
 && echo "$GLIBCBIN_SHA256 glibc-bin.apk" | sha256sum -c - \
 && apk add glibc-bin.apk glibc.apk \
 && /usr/glibc-compat/sbin/ldconfig /lib /usr/glibc-compat/lib \
 && echo 'hosts: files mdns4_minimal [NOTFOUND=return] dns mdns4' >> /etc/nsswitch.conf \
 && apk del curl \
 && rm -rf glibc.apk glibc-bin.apk /var/cache/apk/*
ENV LIGHTNINGD_DATA="/root/.lightning"
ENV LIGHTNINGD_RPC_PORT="9835"
VOLUME [ "/root/.lightning" ]
COPY --from=builder /tmp/lightning_install/ /usr/local/
COPY --from=builder /opt/bitcoin/bin /usr/bin
COPY --from=builder /opt/litecoin/bin /usr/bin
COPY tools/docker-entrypoint.sh entrypoint.sh
EXPOSE 9735/tcp 9835/tcp
ENTRYPOINT ["/sbin/tini", "-g", "--", "./entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
