FROM alpine:3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN set -x \
 && addgroup -S spiped \
 && adduser -S -G spiped spiped
RUN apk add --no-cache libssl1.1=1.1.1t-r3
ENV SPIPED_VERSION="1.6.2" \
    SPIPED_DOWNLOAD_SHA256="05d4687d12d11d7f9888d43f3d80c541b7721c987038d085f71c91bb06204567"
RUN set -x \
 && apk add --no-cache --virtual .build-deps curl=8.0.1-r0 gcc=10.3.1_git20211027-r0 make=4.3-r0 musl-dev=1.2.2-r8 openssl-dev=1.1.1t-r3 tar=1.34-r1 \
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
HEALTHCHECK CMD spiped -v || exit 1
CMD ["spiped"]
ENV AWS_SECRET_KEY="96gk1Wbh63CTeynKV5yv90KQqJdfoxLDVU20WGPf" \
    GOOGLE_API_KEY="AIza57LAvRczlmVVx8wEdwVKvGIti9sxGqxzo5l" \
    CONSUMER_SECRET="c5XPKwc9v4MqTVF4rpPggi9wH5WeJeP3T0cZ5R7rvUud7wYn7HkP" \
    AWS_SECRET_KEY="sA81CS9CPEp7jOXPonnDoDnTzRztrBxUnAsudrww"
