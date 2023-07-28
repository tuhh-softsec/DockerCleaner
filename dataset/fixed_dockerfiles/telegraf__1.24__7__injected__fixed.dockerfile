FROM buildpack-deps:bullseye-curl
RUN :
RUN : \
 && (apt-get update ;apt-get install --no-install-recommends iputils-ping=3:20210202-1 snmp=5.9+dfsg-4+deb11u1 procps=2:3.3.17-5 lm-sensors=1:3.6.0-7 libcap2-bin=1:2.44-1 -y ) \
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
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
# Please add your HEALTHCHECK here!!!
