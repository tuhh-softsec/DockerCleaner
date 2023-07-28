#
#   filebot Dockerfile
#
#   https://github.com/jlesage/docker-filebot
#
#   Pull base image.
FROM jlesage/baseimage-gui:alpine-3.9-v3.5.2
#   Define software versions.
ARG FILEBOT_VERSION=4.8.5
ARG OPENJFX_VERSION=8.151.12-r0
ARG CHROMAPRINT_VERSION=1.4.3
#   Define software download URLs.
ARG FILEBOT_URL=https://get.filebot.net/filebot/FileBot_${FILEBOT_VERSION}/FileBot_${FILEBOT_VERSION}-portable.tar.xz
ARG OPENJFX_URL=https://github.com/sgerrand/alpine-pkg-java-openjfx/releases/download/${OPENJFX_VERSION}/java-openjfx-${OPENJFX_VERSION}.apk
ARG CHROMAPRINT_URL=https://github.com/acoustid/chromaprint/archive/v${CHROMAPRINT_VERSION}.tar.gz
#   Define working directory.
WORKDIR /tmp
#   Install FileBot.
RUN add-pkg --virtual build-dependencies curl \
 && mkdir filebot \
 && curl -# -L ${FILEBOT_URL} | tar xJ -C filebot \
 && mkdir /opt/filebot \
 && cp -Rv filebot/jar /opt/filebot/ \
 && del-pkg build-dependencies \
 && rm -rf /tmp/* /tmp/.[!.]*
#   Install dependencies.
RUN add-pkg --virtual build-dependencies curl \
 && curl -# -L -o java-openjfx.apk ${OPENJFX_URL} \
 && apk add ./java-openjfx.apk --no-cache --allow-untrusted \
 && add-pkg p7zip unrar nss gtk+2.0 openjdk8-jre java-jna libmediainfo \
 && add-pkg yad \
 && del-pkg build-dependencies \
 && rm -rf /tmp/* /tmp/.[!.]*
#   Build and install chromaprint (fpcalc) for AcousItD.
RUN add-pkg --virtual build-dependencies build-base cmake curl ffmpeg-dev fftw-dev \
 && mkdir chromaprint \
 && curl -# -L ${CHROMAPRINT_URL} | tar xz --strip 1 -C chromaprint \
 && cd chromaprint \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE=Release -DBUILD_TOOLS=ON .. \
 && make -j$( nproc ;) \
 && make install \
 && cd .. \
 && cd .. \
 && del-pkg build-dependencies \
 && rm /usr/lib/pkgconfig/libchromaprint.pc /usr/include/chromaprint.h \
 && rmdir /usr/include /usr/lib/pkgconfig \
 && rm -rf /tmp/* /tmp/.[!.]*
#   Adjust the openbox config.
RUN sed-patch 's/<application type="normal">/<application type="normal" title="FileBot \*">/' /etc/xdg/openbox/rc.xml \
 && sed-patch '/<application type="normal" title="FileBot \*">/a \ <layer>below</layer>' /etc/xdg/openbox/rc.xml
#   Generate and install favicons.
RUN APP_ICON_URL=https://raw.githubusercontent.com/jlesage/docker-templates/master/jlesage/images/filebot-icon.png \
 && install_app_icon.sh "$APP_ICON_URL"
#   Add files.
COPY rootfs/ /
#   Set environment variables.
ENV APP_NAME="FileBot"
#   Define mountable directories.
VOLUME ["/config"]
VOLUME ["/storage"]
#   Metadata.
LABEL org.label-schema.name="filebot" \
      org.label-schema.description="Docker container for FileBot" \
      org.label-schema.version="unknown" \
      org.label-schema.vcs-url="https://github.com/jlesage/docker-filebot" \
      org.label-schema.schema-version="1.0"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
