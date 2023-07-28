FROM debian:bullseye-slim
RUN :
RUN set -ex \
 && mkdir ~/.gnupg ; echo "disable-ipv6" >> ~/.gnupg/dirmngr.conf; : \
 && (apt-get update ;apt-get install --no-install-recommends gnupg=2.2.27-2+deb11u2 dirmngr=2.2.27-2+deb11u2 -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && for key in 9D539D90D3328DC7D6C8D3B9D8FF8E1F7DF8B07E; do gpg --keyserver hkp://keyserver.ubuntu.com --recv-keys "$key" ; done
ENV CHRONOGRAF_VERSION="1.7.17"
RUN :
RUN ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='amd64' ;;(arm64) ARCH='arm64' ;;(armhf) ARCH='armhf' ;;(armel) ARCH='armel' ;;(*) echo "Unsupported architecture: ${dpkgArch}" ; exit 1 ;; esac \
 && set -x \
 && : \
 && (apt-get update ;apt-get install --no-install-recommends ca-certificates=20210119 curl=7.74.0-1.3+deb11u7 -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && curl -SLO "https://dl.influxdata.com/chronograf/releases/chronograf_${CHRONOGRAF_VERSION}_${ARCH}.deb.asc" \
 && curl -SLO "https://dl.influxdata.com/chronograf/releases/chronograf_${CHRONOGRAF_VERSION}_${ARCH}.deb" \
 && gpg --batch --verify chronograf_${CHRONOGRAF_VERSION}_${ARCH}.deb.asc chronograf_${CHRONOGRAF_VERSION}_${ARCH}.deb \
 && dpkg -i chronograf_${CHRONOGRAF_VERSION}_${ARCH}.deb \
 && rm -f chronograf_${CHRONOGRAF_VERSION}_${ARCH}.deb* \
 && apt-get purge -y --auto-remove $buildDeps
COPY LICENSE /usr/share/chronograf/LICENSE
COPY agpl-3.0.md /usr/share/chronograf/agpl-3.0.md
EXPOSE 8888/tcp
VOLUME /var/lib/chronograf
COPY entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:8888 || exit 1
CMD ["chronograf"]
