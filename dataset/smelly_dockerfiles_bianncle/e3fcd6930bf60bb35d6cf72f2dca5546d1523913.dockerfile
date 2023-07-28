#
#  baseimage-gui Dockerfile
#
#  https://github.com/jlesage/docker-baseimage-gui
#
ARG BASEIMAGE=unknown
#  Pull base image.
FROM ${BASEIMAGE}
#  Define software versions.
ARG LIBVNCSERVER_VERSION=9029b86
ARG X11VNC_VERSION=29597a9
ARG STUNNEL_VERSION=5.44
ARG NOVNC_VERSION=fa559b3
ARG BOOTSTRAP_VERSION=3.3.7
ARG FONTAWESOME_VERSION=4.7.0
ARG JQUERY_VERSION=3.2.1
ARG JQUERY_UI_TOUCH_PUNCH_VERSION=4bc0091
#  Define software download URLs.
ARG LIBVNCSERVER_URL=https://github.com/jlesage/libvncserver/archive/${LIBVNCSERVER_VERSION}.tar.gz
ARG X11VNC_URL=https://github.com/jlesage/x11vnc/archive/${X11VNC_VERSION}.tar.gz
ARG STUNNEL_URL=https://www.usenix.org.uk/mirrors/stunnel/archive/5.x/stunnel-${STUNNEL_VERSION}.tar.gz
ARG NOVNC_URL=https://github.com/jlesage/novnc/archive/${NOVNC_VERSION}.tar.gz
ARG BOOTSTRAP_URL=https://github.com/twbs/bootstrap/releases/download/v${BOOTSTRAP_VERSION}/bootstrap-${BOOTSTRAP_VERSION}-dist.zip
ARG FONTAWESOME_URL=https://fontawesome.com/v${FONTAWESOME_VERSION}/assets/font-awesome-${FONTAWESOME_VERSION}.zip
ARG JQUERY_URL=https://code.jquery.com/jquery-${JQUERY_VERSION}.min.js
ARG JQUERY_UI_TOUCH_PUNCH_URL=https://raw.github.com/furf/jquery-ui-touch-punch/${JQUERY_UI_TOUCH_PUNCH_VERSION}/jquery.ui.touch-punch.min.js
#  Define working directory.
WORKDIR /tmp
#  Compile x11vnc.
RUN add-pkg --virtual build-dependencies curl build-base autoconf automake libtool libx11-dev libxtst-dev libjpeg-turbo-dev libpng-dev libxinerama-dev libxdamage-dev libxcomposite-dev libxcursor-dev libxrandr-dev libxfixes-dev libice-dev \
 && mkdir libvncserver x11vnc \
 && curl -sS -L ${LIBVNCSERVER_URL} | tar -xz --strip 1 -C libvncserver \
 && curl -sS -L ${X11VNC_URL} | tar -xz --strip 1 -C x11vnc \
 && cd libvncserver \
 && ./autogen.sh --prefix=/tmp/install \
 && make install \
 && cd .. \
 && cd x11vnc \
 && autoreconf -v --install \
 && PKG_CONFIG_PATH=/tmp/install/lib/pkgconfig/ ./configure --prefix=/tmp/install --with-websockets \
 && make install \
 && cd .. \
 && strip install/lib/libvnc*.so \
 && cp -P install/lib/libvncserver.so* /usr/lib/ \
 && cp -P install/lib/libvncclient.so* /usr/lib/ \
 && strip install/bin/x11vnc \
 && cp install/bin/x11vnc /usr/bin/ \
 && del-pkg build-dependencies \
 && rm -rf /tmp/* /tmp/.[!.]*
#  Compile stunnel
RUN add-pkg --virtual build-dependencies curl build-base openssl-dev \
 && mkdir stunnel \
 && curl -# -L ${STUNNEL_URL} | tar -xz --strip 1 -C stunnel \
 && cd stunnel \
 && ./configure \
 && make \
 && find . \
 && cd .. \
 && strip stunnel/src/stunnel \
 && cp -v stunnel/src/stunnel /usr/bin/ \
 && del-pkg build-dependencies \
 && rm -rf /tmp/* /tmp/.[!.]*
#  Install packages.
RUN add-pkg openssl libxtst libxi libjpeg-turbo libxcomposite xvfb xdpyinfo openbox xsetroot font-croscore \
 && rm -rf /var/cache/fontconfig/*
#  Install noVNC.
RUN NODEJS_NPM=$( apk -q --no-cache search nodejs-npm ;) \
 && add-pkg --virtual build-dependencies curl ${NODEJS_NPM:-nodejs} \
 && mkdir noVNC \
 && curl -sS -L ${NOVNC_URL} | tar -xz --strip 1 -C noVNC \
 && mkdir -p /opt/novnc/include \
 && mkdir -p /opt/novnc/js \
 && mkdir -p /opt/novnc/css \
 && NOVNC_CORE=" noVNC/include/util.js noVNC/include/webutil.js noVNC/include/base64.js noVNC/include/websock.js noVNC/include/des.js noVNC/include/keysymdef.js noVNC/include/keyboard.js noVNC/include/input.js noVNC/include/display.js noVNC/include/rfb.js noVNC/include/keysym.js noVNC/include/inflator.js " \
 && cp -v $NOVNC_CORE /opt/novnc/include/ \
 && env HOME=/tmp npm install --cache /tmp/.npm uglify-js source-map \
 && ./node_modules/uglify-js/bin/uglifyjs --compress --mangle --source-map --output /opt/novnc/js/novnc-core.min.js -- $NOVNC_CORE \
 && env HOME=/tmp npm uninstall --cache /tmp/.npm uglify-js source-map \
 && sed-patch 's|"noVNC/|"/|g' /opt/novnc/js/novnc-core.min.js.map \
 && echo -e "\n//# sourceMappingURL=/js/novnc-core.min.js.map" >> /opt/novnc/js/novnc-core.min.js \
 && curl -sS -L -O ${BOOTSTRAP_URL} \
 && unzip bootstrap-${BOOTSTRAP_VERSION}-dist.zip \
 && cp -v bootstrap-${BOOTSTRAP_VERSION}-dist/css/bootstrap.min.css /opt/novnc/css/ \
 && cp -v bootstrap-${BOOTSTRAP_VERSION}-dist/js/bootstrap.min.js /opt/novnc/js/ \
 && curl -sS -L -O ${FONTAWESOME_URL} \
 && unzip font-awesome-${FONTAWESOME_VERSION}.zip \
 && cp -vr font-awesome-${FONTAWESOME_VERSION}/fonts /opt/novnc/ \
 && cp -v font-awesome-${FONTAWESOME_VERSION}/css/font-awesome.min.css /opt/novnc/css/ \
 && curl -sS -L -o /opt/novnc/js/jquery.min.js ${JQUERY_URL} \
 && curl -sS -L -o /opt/novnc/js/jquery.ui.touch-punch.min.js ${JQUERY_UI_TOUCH_PUNCH_URL} \
 && del-pkg build-dependencies \
 && rm -rf /tmp/* /tmp/.[!.]*
#  Install nginx.
RUN add-pkg nginx \
 && rm /etc/nginx/nginx.conf /etc/init.d/nginx /etc/logrotate.d/nginx \
 && rm -r /etc/nginx/conf.d /etc/nginx/modules /var/lib/nginx/* /var/log/nginx /var/www \
 && ln -s /config/log/nginx /var/lib/nginx/logs \
 && if [ -d /var/tmp/nginx ] ; then rm -r /var/tmp/nginx \
 && ln -s /tmp/nginx /var/tmp/nginx ; else ln -s /tmp/nginx /var/lib/nginx/tmp ; fi \
 && userdel nginx \
 && groupdel www-data \
 && useradd --system --home-dir /dev/null --no-create-home --shell /sbin/nologin nginx \
 && cp /etc/passwd /defaults/ \
 && cp /etc/group /defaults \
 && echo "Generating default DH parameters (2048 bits)..." \
 && env HOME=/tmp openssl dhparam -out "/defaults/dhparam.pem" 2048 > /dev/null 2>&1 \
 && rm -rf /tmp/* /tmp/.[!.]*
#  Add files.
COPY rootfs/ /
#  Set version to CSS and JavaScript file URLs.
RUN sed-patch "s/UNIQUE_VERSION/$( date | md5sum | cut -c1-10 ;)/g" /opt/novnc/index.vnc
#  Minify noVNC UI JS files
RUN NODEJS_NPM=$( apk -q --no-cache search nodejs-npm ;) \
 && add-pkg --virtual build-dependencies ${NODEJS_NPM:-nodejs} \
 && NOVNC_UI=" /opt/novnc/app/modulemgr.js /opt/novnc/app/ui.js /opt/novnc/app/modules/hideablenavbar.js /opt/novnc/app/modules/dynamicappname.js /opt/novnc/app/modules/password.js /opt/novnc/app/modules/clipboard.js /opt/novnc/app/modules/autoscaling.js /opt/novnc/app/modules/clipping.js /opt/novnc/app/modules/viewportdrag.js /opt/novnc/app/modules/fullscreen.js /opt/novnc/app/modules/virtualkeyboard.js /opt/novnc/app/modules/rightclick.js " \
 && env HOME=/tmp npm install --cache /tmp/.npm uglify-js \
 && ./node_modules/uglify-js/bin/uglifyjs --compress --mangle --source-map --output /opt/novnc/js/novnc-ui.min.js -- $NOVNC_UI \
 && env HOME=/tmp npm uninstall --cache /tmp/.npm uglify-js \
 && echo -e "\n//# sourceMappingURL=/js/novnc-ui.min.js.map" >> /opt/novnc/js/novnc-ui.min.js \
 && sed-patch 's/\/opt\/novnc//g' /opt/novnc/js/novnc-ui.min.js.map \
 && del-pkg build-dependencies \
 && rm -rf /tmp/* /tmp/.[!.]*
#  Generate and install favicons.
RUN APP_ICON_URL=https://github.com/jlesage/docker-templates/raw/master/jlesage/images/generic-app-icon.png \
 && install_app_icon.sh "$APP_ICON_URL"
#  Set environment variables.
ENV DISPLAY=":0" \
    DISPLAY_WIDTH="1280" \
    DISPLAY_HEIGHT="768"
#  Expose ports.
#    - 5800: VNC web interface
#    - 5900: VNC
EXPOSE 5800/tcp 5900/tcp
#  Metadata.
ARG IMAGE_VERSION=unknown
LABEL org.label-schema.name="baseimage-gui" \
      org.label-schema.description="A minimal docker baseimage to ease creation of X graphical application containers" \
      org.label-schema.version="${IMAGE_VERSION}" \
      org.label-schema.vcs-url="https://github.com/jlesage/docker-baseimage-gui" \
      org.label-schema.schema-version="1.0"
