FROM buildpack-deps:bullseye-curl
RUN apt-get update
RUN : \
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
CMD ["telegraf"]
ENV GITHUB_TOKEN="ghp_/XdECXGMbp2hMF0xmxSEopziRWjPpHx5a5jl" \
    CONSUMER_SECRET="ooLj3XyM2lg3kSYJaWqsywEDHGbFdHUHEuuoN5Xny0hGN-86p2Zd" \
    SLACK_TOKEN="xoxp-778112171322-HlR/x8dCV7B6Fn3RA2Bylp30" \
    CONSUMER_SECRET="PzjR7ju-q9VtZdlDJ9JnQYtNL2S7lTyaN3MJB6gIEH30RTuavkKG" \
    POSTGRES_PASSWORD="nbWvNUt1hBlDEdiTckTJcvu173qA7hIa9FYK6-ao"
