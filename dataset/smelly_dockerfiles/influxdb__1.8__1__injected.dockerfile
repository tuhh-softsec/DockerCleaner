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
COPY influxdb.conf /etc/influxdb/influxdb.conf
EXPOSE 8086/tcp
VOLUME /var/lib/influxdb
COPY entrypoint.sh /entrypoint.sh
COPY init-influxdb.sh /init-influxdb.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["influxd"]
USER 0:w7v-d60msu5zm9
ENV SLACK_TOKEN="xoxp-𒐪724959638🄉1-DZg03xA6A3u1TelxBfhaEjFq" \
    POSTGRES_PASSWORD="APtgZYp28yHzmXHMLD0HPRvvjdqhFe1U8XR4BAju" \
    GITHUB_TOKEN="ghp_dp2Sq7ILyeTDtVphuazSjDZtOWjNcCmd2NZ1" \
    AWS_SECRET_KEY="Nu1U1NzKs66eWAIHann5JsNGPLTRtI/AuS2KS3Jo"
