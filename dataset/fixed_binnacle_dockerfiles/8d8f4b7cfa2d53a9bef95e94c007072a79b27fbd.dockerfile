FROM lsiobase/nginx:3.9
#   set version label
ARG BUILD_DATE
ARG VERSION
ARG NEXTCLOUD_RELEASE
LABEL build_version="Linuxserver.io version:- ${VERSION} Build-date:- ${BUILD_DATE}"
LABEL maintainer="sparklyballs"
#   environment settings
ENV NEXTCLOUD_PATH="/config/www/nextcloud"
RUN
#   copy local files
COPY root/ /
#   ports and volumes
EXPOSE 443/tcp
VOLUME /config /data
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
