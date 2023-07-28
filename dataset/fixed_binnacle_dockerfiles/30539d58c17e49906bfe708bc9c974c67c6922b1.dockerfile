#   Copyright 2016 The WWU eLectures Team All rights reserved.
#
#   Licensed under the Educational Community License, Version 2.0
#   (the "License"); you may not use this file except in compliance with
#   the License. You may obtain a copy of the License at
#
#       http://opensource.org/licenses/ECL-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM maven:3.6-jdk-8-slim AS build
ARG repo="https://github.com/opencast/opencast.git"
ARG branch="7.0"
ENV OPENCAST_DISTRIBUTION="admin" \
    OPENCAST_SRC="/usr/src/opencast" \
    OPENCAST_HOME="/opencast" \
    OPENCAST_UID="800" \
    OPENCAST_GID="800" \
    OPENCAST_USER="opencast" \
    OPENCAST_GROUP="opencast" \
    OPENCAST_REPO="${repo}" \
    OPENCAST_BRANCH="${branch}"
RUN apt-get update \
 && apt-get install --no-install-recommends tar=1.30+dfsg-6 gzip=1.9-3+deb10u1 bzip2=1.0.6-9.2~deb10u2 git=1:2.20.1-2+deb10u8 ca-certificates=20200601~deb10u2 openssl=1.1.1n-0+deb10u4 make=4.2.1-1.2 gcc=4:8.3.0-1 g++=4:8.3.0-1 libc-dev -y \
 && git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
 && cd /tmp/su-exec \
 && make \
 && cp su-exec /usr/local/sbin \
 && rm -rf /tmp/* /var/lib/apt/lists/* \
 && groupadd --system -g "${OPENCAST_GID}" "${OPENCAST_GROUP}" \
 && useradd --system -M -N -g "${OPENCAST_GROUP}" -d "${OPENCAST_HOME}" -u "${OPENCAST_UID}" "${OPENCAST_USER}" \
 && mkdir -p "${OPENCAST_SRC}" "${OPENCAST_HOME}" \
 && chown -R "${OPENCAST_USER}:${OPENCAST_GROUP}" "${OPENCAST_SRC}" "${OPENCAST_HOME}"
USER "${OPENCAST_USER}"
RUN git clone --recursive "${OPENCAST_REPO}" "${OPENCAST_SRC}" \
 && cd "${OPENCAST_SRC}" \
 && git checkout "${OPENCAST_BRANCH}" \
 && mvn --quiet --batch-mode install -DskipTests=true -Dcheckstyle.skip=true -DskipJasmineTests=true \
 && tar -xzf build/opencast-dist-${OPENCAST_DISTRIBUTION}-*.tar.gz --strip 1 -C "${OPENCAST_HOME}" \
 && rm -rf "${OPENCAST_SRC}"/* ~/.m2 ~/.npm ~/.node-gyp
FROM openjdk:8-jdk-slim-stretch
LABEL maintainer="WWU eLectures team <electures.dev@uni-muenster.de>" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.version="7.0" \
      org.label-schema.name="opencast-admin" \
      org.label-schema.description="Docker image for the Opencast admin distribution" \
      org.label-schema.usage="https://github.com/opencast/opencast-docker/blob/7.0/README.md" \
      org.label-schema.url="http://www.opencast.org/" \
      org.label-schema.vcs-url="https://github.com/opencast/opencast-docker" \
      org.label-schema.vendor="University of Münster" \
      org.label-schema.docker.debug="docker exec -it $CONTAINER sh" \
      org.label-schema.docker.cmd.help="docker run --rm quay.io/opencast/admin:7.0 app:help"
ENV OPENCAST_VERSION="7.0" \
    OPENCAST_DISTRIBUTION="admin" \
    OPENCAST_HOME="/opencast" \
    OPENCAST_DATA="/data" \
    OPENCAST_CUSTOM_CONFIG="/etc/opencast" \
    OPENCAST_USER="opencast" \
    OPENCAST_GROUP="opencast" \
    OPENCAST_UID="800" \
    OPENCAST_GID="800" \
    OPENCAST_REPO="${repo}" \
    OPENCAST_BRANCH="${branch}"
ENV OPENCAST_SCRIPTS="${OPENCAST_HOME}/docker/scripts" \
    OPENCAST_SUPPORT="${OPENCAST_HOME}/docker/support" \
    OPENCAST_CONFIG="${OPENCAST_HOME}/etc"
RUN groupadd --system -g "${OPENCAST_GID}" "${OPENCAST_GROUP}" \
 && useradd --system -M -N -g "${OPENCAST_GROUP}" -d "${OPENCAST_HOME}" -u "${OPENCAST_UID}" "${OPENCAST_USER}" \
 && mkdir -p "${OPENCAST_DATA}" \
 && chown -R "${OPENCAST_USER}:${OPENCAST_GROUP}" "${OPENCAST_DATA}"
COPY --from=build /usr/local/sbin/su-exec /usr/local/sbin/su-exec
COPY --chown=opencast:opencast --from=build "${OPENCAST_HOME}" "${OPENCAST_HOME}"
COPY --chown=opencast:opencast assets/scripts "${OPENCAST_SCRIPTS}"
COPY --chown=opencast:opencast assets/support "${OPENCAST_SUPPORT}"
COPY --chown=opencast:opencast assets/etc/* "${OPENCAST_CONFIG}/"
COPY assets/docker-entrypoint.sh assets/docker-healthcheck.sh /
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20200601~deb10u2 openssl=1.1.1n-0+deb10u4 tzdata=2021a-0+deb10u10 curl=7.64.0-4+deb10u5 jq=1.5+dfsg-2+b1 fontconfig=2.13.1-2 fonts-dejavu=2.37-1 fonts-freefont-ttf=20120503-9 fonts-liberation=1:1.07.4-9 fonts-linuxlibertine=5.3.0-4 hunspell=1.7.0-2 hunspell-en-au=1:2018.04.16-1 hunspell-en-ca=1:2018.04.16-1 hunspell-en-gb=1:6.2.0-1 hunspell-en-us=1:2018.04.16-1 hunspell-en-za=1:6.2.0-1 tesseract-ocr=4.0.0-2 tesseract-ocr-eng=1:4.00~git30-7274cfa-1 ffmpeg=7:4.1.10-0+deb10u1 libavcodec-extra=7:4.1.10-0+deb10u1 sox=14.4.2+git20190427-1+deb10u2 synfig=1.2.2-1 nfs-common=1:1.3.4-2.5+deb10u1 netcat-openbsd=1.195-2 -y \
 && javac "${OPENCAST_SCRIPTS}/TryToConnectToDb.java" \
 && rm -rf /tmp/* /var/lib/apt/lists/* "${OPENCAST_SCRIPTS}/TryToConnectToDb.java"
WORKDIR "${OPENCAST_HOME}"
EXPOSE 8080/tcp
VOLUME [ "${OPENCAST_DATA}" ]
HEALTHCHECK --timeout=10s CMD /docker-healthcheck.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["app:start"]
