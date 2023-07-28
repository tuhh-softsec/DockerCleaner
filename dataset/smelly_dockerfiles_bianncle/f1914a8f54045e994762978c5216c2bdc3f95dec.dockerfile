#
#  nginx-proxy-manager Dockerfile
#
#  https://github.com/jlesage/docker-nginx-proxy-manager
#
#  Pull base image.
FROM jlesage/baseimage:alpine-3.8-v2.4.2
#  Define software versions.
ARG NGINX_PROXY_MANAGER_VERSION=2.0.13
#  Define software download URLs.
ARG NGINX_PROXY_MANAGER_URL=https://github.com/jc21/nginx-proxy-manager/archive/${NGINX_PROXY_MANAGER_VERSION}.tar.gz
#  Define working directory.
WORKDIR /tmp
#  Install dependencies.
RUN add-pkg nodejs nginx nginx-mod-stream mariadb mariadb-client mariadb-server-utils certbot openssl apache2-utils \
 && rm -r /var/lib/mysql \
 && rm -r /var/log/nginx /var/lib/nginx /var/tmp/nginx /etc/nginx /etc/init.d/nginx /etc/logrotate.d/nginx /var/www \
 && ln -s /tmp/nginx /var/tmp/nginx \
 && mkdir -p /var/lib/nginx/logs \
 && ln -sf /config/log/nginx/error.log /var/lib/nginx/logs/error.log
#  Install Nginx Proxy Manager.
RUN add-pkg --virtual build-dependencies build-base curl patch yarn git python npm bash \
 && echo "Installing node-prune..." \
 && curl -sfL https://install.goreleaser.com/github.com/tj/node-prune.sh | bash -s -- -b /tmp/bin \
 && echo "Downloading Nginx Proxy Manager package..." \
 && mkdir nginx-proxy-manager \
 && curl -# -L ${NGINX_PROXY_MANAGER_URL} | tar xz --strip 1 -C nginx-proxy-manager \
 && echo "Building Nginx Proxy Manager..." \
 && cp -r nginx-proxy-manager /app \
 && cd /app \
 && yarn install \
 && npm --cache /tmp/.npm run-script build \
 && rm -rf node_modules \
 && yarn install --prod \
 && /tmp/bin/node-prune \
 && cd /tmp \
 && echo "Installing Nginx Proxy Manager..." \
 && mkdir -p /opt/nginx-proxy-manager/src \
 && cp -r /app/dist /opt/nginx-proxy-manager/ \
 && cp -r /app/node_modules /opt/nginx-proxy-manager/ \
 && cp -r /app/src/backend /opt/nginx-proxy-manager/src/ \
 && cp -r /app/package.json /opt/nginx-proxy-manager/ \
 && cp -r /app/knexfile.js /opt/nginx-proxy-manager/ \
 && cp -r nginx-proxy-manager/rootfs/etc/nginx /etc/ \
 && cp -r nginx-proxy-manager/rootfs/var/www /var/ \
 && sed-patch 's|81|8181|' /opt/nginx-proxy-manager/src/backend/index.js \
 && sed-patch 's|81|8181|' /etc/nginx/conf.d/default.conf \
 && sed-patch 's|listen 80;|listen 8080;|' /etc/nginx/conf.d/default.conf \
 && sed-patch 's|listen 80;|listen 8080;|' /opt/nginx-proxy-manager/src/backend/templates/letsencrypt-request.conf \
 && sed-patch 's|listen 80;|listen 8080;|' /opt/nginx-proxy-manager/src/backend/templates/_listen.conf \
 && sed-patch 's|listen 80 |listen 8080 |' /opt/nginx-proxy-manager/src/backend/templates/default.conf \
 && sed-patch 's|listen 443 |listen 4443 |' /etc/nginx/conf.d/default.conf \
 && sed-patch 's|listen 443 |listen 4443 |' /opt/nginx-proxy-manager/src/backend/templates/_listen.conf \
 && sed-patch 's|-g "error_log off;"||' /opt/nginx-proxy-manager/src/backend/internal/nginx.js \
 && sed-patch 's|user root;|#user root;|' /etc/nginx/nginx.conf \
 && sed-patch '/daemon off;/a load_module /usr/lib/nginx/modules/ngx_stream_module.so;' /etc/nginx/nginx.conf \
 && ln -s /config /data \
 && mv /etc/nginx/conf.d/include/ip_ranges.conf /defaults/ \
 && ln -sf /config/nginx/ip_ranges.conf /etc/nginx/conf.d/include/ip_ranges.conf \
 && rm /etc/nginx/conf.d/include/resolvers.conf \
 && ln -sf /config/nginx/resolvers.conf /etc/nginx/conf.d/include/resolvers.conf \
 && ln -s /config/nginx/cache /var/lib/nginx/cache \
 && mkdir /opt/nginx-proxy-manager/config \
 && ln -s /config/production.json /opt/nginx-proxy-manager/config/production.json \
 && ln -s /config/letsencrypt /etc/letsencrypt \
 && del-pkg build-dependencies \
 && rm -r /root/.node-gyp /app /usr/lib/node_modules /opt/nginx-proxy-manager/node_modules/bcrypt/build \
 && rm -rf /tmp/* /tmp/.[!.]*
#  Add files.
COPY rootfs/ /
#  Set environment variables.
ENV APP_NAME="Nginx Proxy Manager" \
    KEEP_APP_RUNNING="1"
#  Define mountable directories.
VOLUME ["/config"]
#  Expose ports.
#    - 8080: HTTP traffic
#    - 4443: HTTPs traffic
#    - 8181: Management web interface
EXPOSE 8080/tcp 4443/tcp 8181/tcp
#  Metadata.
LABEL org.label-schema.name="nginx-proxy-manager" \
      org.label-schema.description="Docker container for Nginx Proxy Manager" \
      org.label-schema.version="unknown" \
      org.label-schema.vcs-url="https://github.com/jlesage/docker-nginx-proxy-manager" \
      org.label-schema.schema-version="1.0"
