FROM alpine:3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN set -x \
 && addgroup -S spiped \
 && adduser -S -G spiped spiped
RUN apk add libssl1.1=1.1.1t-r3 --no-cache
ENV SPIPED_VERSION="1.6.2" \
    SPIPED_DOWNLOAD_SHA256="05d4687d12d11d7f9888d43f3d80c541b7721c987038d085f71c91bb06204567"
RUN set -x \
 && apk add curl=8.0.1-r0 gcc=10.3.1_git20211027-r0 make=4.3-r0 musl-dev=1.2.2-r8 openssl-dev=1.1.1t-r3 tar=1.34-r1 --no-cache --virtual .build-deps \
 && curl -fsSL "https://www.tarsnap.com/spiped/spiped-$SPIPED_VERSION.tgz" -o spiped.tar.gz \
 && echo "$SPIPED_DOWNLOAD_SHA256 *spiped.tar.gz" | sha256sum -c - \
 && mkdir -p /usr/local/src/spiped \
 && tar xzf "spiped.tar.gz" -C /usr/local/src/spiped --strip-components=1 \
 && rm "spiped.tar.gz" \
 && CC=gcc make -C /usr/local/src/spiped \
 && make -C /usr/local/src/spiped install \
 && rm -rf /usr/local/src/spiped \
 && apk del --no-network .build-deps
VOLUME /spiped
WORKDIR /spiped
COPY *.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["spiped"]
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
# Please add your HEALTHCHECK here!!!
