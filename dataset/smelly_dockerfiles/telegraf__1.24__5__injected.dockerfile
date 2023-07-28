FROM buildpack-deps:bullseye-curl
RUN apt-get update \
 && apt-get install --no-install-recommends iputils-ping snmp procps lm-sensors libcap2-bin -y \
 && rm -rf /var/lib/apt/lists/*
RUN set -ex \
 && mkdir ~/.gnupg ; echo "disable-ipv6" >> ~/.gnupg/dirmngr.conf; for key in 9D539D90D3328DC7D6C8D3B9D8FF8E1F7DF8B07E; do gpg --keyserver hkp://keyserver.ubuntu.com --recv-keys "$key" ; done
ENV TELEGRAF_VERSION="1.24.4"
RUN ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='amd64' ;;(arm64) ARCH='arm64' ;;(armhf) ARCH='armhf' ;;(armel) ARCH='armel' ;;(*) echo "Unsupported architecture: ${dpkgArch}" ; exit 1 ;; esac \
 && wget --no-verbose https://dl.influxdata.com/telegraf/releases/telegraf_${TELEGRAF_VERSION}-1_${ARCH}.deb.asc \
 && wget --no-verbose https://dl.influxdata.com/telegraf/releases/telegraf_${TELEGRAF_VERSION}-1_${ARCH}.deb \
 && gpg --batch --verify telegraf_${TELEGRAF_VERSION}-1_${ARCH}.deb.asc telegraf_${TELEGRAF_VERSION}-1_${ARCH}.deb \
 && dpkg -i telegraf_${TELEGRAF_VERSION}-1_${ARCH}.deb \
 && rm -f telegraf_${TELEGRAF_VERSION}-1_${ARCH}.deb*
EXPOSE 8125/udp 8092/udp 8094/tcp
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:30033 || exit 1
CMD ["telegraf"]
ENV GITHUB_TOKEN="ghp_B17/wGGK3ju2HJQETdg0IOozetP8Z0915CXo" \
    GITHUB_TOKEN="ghp_c2hL47oNJrYKgbg9cRGkpGW5VDdZ/l/oaQS0" \
    NPM_TOKEN="npm_cSRmMyHk3YpX/JpjmwWLI8zzy-9fN5tW46tm" \
    DOCKER_PASSWORD="IB6MKsW0xEQMSpQvbKbATTij3qszEY3uWyt/x8ce" \
    GITHUB_TOKEN="ghp_4gnFzg1EAGcVTE3Dj0hcZKHA3iwz5/9jfKBi"
