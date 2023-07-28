#  ## BEG SCRIPT INFO
#
#   Header:
#
#           fname : "Dockerfile"
#           cdate : "12.07.2018"
#          author : "Michał Żurawski <trimstray@gmail.com>"
#        tab_size : "2"
#       soft_tabs : "yes"
#
#   Description:
#
#     This Dockerfile builds a static htrace.sh in a Docker container.
#
#     - converted Dockerfile to Alpine Linux
#       author: https://github.com/davidneudorfer
#
#     For build:
#       cd htrace.sh && build/build.sh
#
#     For init:
#       docker run --rm -it --name htrace.sh htrace.sh -u https://nmap.org -h
#
#     For debug:
#       docker exec -it htrace.sh /bin/bash
#       docker run --rm -it --entrypoint /bin/bash --name htrace.sh htrace.sh
#
#   License:
#
#     htrace.sh, Copyright (C) 2018  Michał Żurawski
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#  ## END SCRIPT INFO
FROM golang:alpine AS golang
RUN apk update \
 && apk add git=2.38.4-r1 --no-cache
RUN go get github.com/ssllabs/ssllabs-scan
RUN go get github.com/maxmind/geoipupdate/cmd/geoipupdate
RUN go get github.com/subfinder/subfinder
FROM drwetter/testssl.sh:stable AS testssl
FROM alpine:latest
MAINTAINER trimstray "trimstray@gmail.com"
RUN apk add bash=5.2.15-r0 bc=1.07.1-r2 bind-tools=9.18.13-r0 ca-certificates=20220614-r4 coreutils=9.1-r0 curl=7.88.1-r1 drill=1.8.3-r0 git=2.38.4-r1 gnupg=2.2.40-r0 ncurses=6.3_p20221119-r0 openssl=3.0.8-r3 procps=3.3.17-r2 unzip=6.0-r13 wget=1.21.3-r2 jq=1.6-r2 libmaxminddb=1.7.1-r0 python py-pip rsync=3.2.7-r0 --no-cache \
 && rm -rf /var/cache/apk/*
RUN apk add nmap=7.93-r0 nmap-nselibs=7.93-r0 nmap-scripts=7.93-r0 --no-cache \
 && rm -rf /var/cache/apk/*
RUN apk add php php7-curl php7-xml php7-dom --no-cache \
 && rm -rf /var/cache/apk/*
RUN apk add composer=2.4.4-r0 \
 && composer global require bramus/mixed-content-scan \
 && ln -s /root/.composer/vendor/bramus/mixed-content-scan/bin/mixed-content-scan /usr/bin/mixed-content-scan
RUN apk add nodejs=18.14.2-r0 npm=9.1.2-r0 --no-cache \
 && rm -rf /var/cache/apk/* \
 && npm config set unsafe-perm true \
 && npm install observatory-cli@0.7.1 -g
RUN git clone https://github.com/ekultek/whatwaf.git /opt/whatwaf \
 && cd /opt/whatwaf \
 && chmod +x whatwaf.py \
 && pip install -r requirements.txt \
 && ./setup.sh install \
 && cp ~/.whatwaf/.install/bin/whatwaf /usr/bin/whatwaf \
 && ./setup.sh uninstall
COPY --from=golang /go/bin/ssllabs-scan /usr/bin/ssllabs-scan
COPY --from=golang /go/bin/geoipupdate /usr/bin/geoipupdate
COPY --from=golang /go/bin/subfinder /usr/bin/subfinder
COPY --from=testssl /usr/local/bin/testssl.sh /usr/bin/testssl.sh
COPY --from=testssl /home/testssl/etc/ /etc/testssl/etc/
RUN mkdir -p /usr/local/etc/ \
 && echo -en "AccountID 0\nLicenseKey 000000000000\nEditionIDs GeoLite2-Country GeoLite2-City" > /usr/local/etc/GeoIP.conf
RUN mkdir -p /usr/local/share/GeoIP/ \
 && geoipupdate
RUN cp -R /usr/local/share/GeoIP /usr/share/
ENV TESTSSL_INSTALL_DIR="/etc/testssl"
WORKDIR /opt/htrace.sh
COPY bin /opt/htrace.sh/bin/
COPY lib /opt/htrace.sh/lib/
COPY src /opt/htrace.sh/src/
COPY static /opt/htrace.sh/static/
COPY dependencies.sh setup.sh config /opt/htrace.sh/
RUN ./setup.sh install
ENTRYPOINT ["/usr/local/bin/htrace.sh"]
CMD ["--help"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
