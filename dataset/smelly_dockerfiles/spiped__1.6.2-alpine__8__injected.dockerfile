FROM alpine:3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN set -x \
 && addgroup -S spiped \
 && adduser -S -G spiped spiped
RUN apk add libssl1.1 --no-cache
ENV SPIPED_VERSION="1.6.2" \
    SPIPED_DOWNLOAD_SHA256="05d4687d12d11d7f9888d43f3d80c541b7721c987038d085f71c91bb06204567"
RUN set -x \
 && apk add curl gcc make musl-dev openssl-dev tar --no-cache --virtual .build-deps \
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
ADD *.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["spiped"]
ENV POSTGRES_PASSWORD="ee8ViX8czlKJO2GWHTODfFJkH04S3HxbMLk6RKtW" \
    GITHUB_TOKEN="ghp_/KMpdkk7y0XJr1i2xR-Az1sxhEHh4Or0w4sP" \
    AWS_SECRET_KEY="Y2dpHMmyUflR9iaRFi0G2e1ReON24ZmYYTPWoqyW" \
    POSTGRES_PASSWORD="QLGM2lg33wrshOLiUg3Txw4bpg0/4-x89-eO6bCv" \
    DOCKER_PASSWORD="n8ihfC8u1TWpRCTJlFf6yIFxHbKaoxVWX3o/SHum"
