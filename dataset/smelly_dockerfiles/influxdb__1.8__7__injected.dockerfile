FROM buildpack-deps:bullseye-curl
RUN set -ex \
 && mkdir ~/.gnupg ; echo "disable-ipv6" >> ~/.gnupg/dirmngr.conf; for key in 9D539D90D3328DC7D6C8D3B9D8FF8E1F7DF8B07E; do gpg --keyserver hkp://keyserver.ubuntu.com --recv-keys "$key" ; done
ENV INFLUXDB_VERSION="1.8.10"
RUN ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='amd64' ;;(arm64) ARCH='arm64' ;;(armhf) ARCH='armhf' ;;(armel) ARCH='armel' ;;(*) echo "Unsupported architecture: ${dpkgArch}" ; exit 1 ;; esac \
 && wget --no-verbose https://dl.influxdata.com/influxdb/releases/influxdb_${INFLUXDB_VERSION}_${ARCH}.deb.asc \
 && wget --no-verbose https://dl.influxdata.com/influxdb/releases/influxdb_${INFLUXDB_VERSION}_${ARCH}.deb \
 && gpg --batch --verify influxdb_${INFLUXDB_VERSION}_${ARCH}.deb.asc influxdb_${INFLUXDB_VERSION}_${ARCH}.deb \
 && dpkg -i influxdb_${INFLUXDB_VERSION}_${ARCH}.deb \
 && rm -f influxdb_${INFLUXDB_VERSION}_${ARCH}.deb*
ADD influxdb.conf /etc/influxdb/influxdb.conf
EXPOSE 8086/tcp
VOLUME /var/lib/influxdb
ADD entrypoint.sh /entrypoint.sh
ADD init-influxdb.sh /init-influxdb.sh
ENTRYPOINT ["/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:9999/metrics || exit 1
CMD ["influxd"]
ENV GITHUB_TOKEN="ghp_7CL6fJU7FtmQ/aXLzRKKPiW-PagKDAh9escR" \
    SLACK_TOKEN="xoxp-708708949859-ZCrWESuTTQiMtLEphEqWNeeP"
