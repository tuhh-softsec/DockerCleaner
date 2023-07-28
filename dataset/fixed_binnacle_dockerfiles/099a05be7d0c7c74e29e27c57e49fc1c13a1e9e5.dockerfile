FROM nginx:stable-alpine
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION
LABEL maintainer="CrazyMax" \
      org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.name="matomo" \
      org.label-schema.description="Matomo (formerly Piwik)" \
      org.label-schema.version="$VERSION" \
      org.label-schema.url="https://github.com/crazy-max/docker-matomo" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="https://github.com/crazy-max/docker-matomo" \
      org.label-schema.vendor="CrazyMax" \
      org.label-schema.schema-version="1.0"
RUN apk add gcc=12.2.1_git20220924-r4 gd-dev=2.3.3-r3 geoip-dev=1.6.12-r3 git=2.38.4-r1 gnupg=2.2.40-r0 libc-dev=0.7.2-r3 libmaxminddb-dev=1.7.1-r0 libxslt-dev=1.1.37-r1 linux-headers=5.19.5-r0 make=4.3-r1 openssl-dev=3.0.8-r3 pcre-dev=8.45-r2 perl-dev=5.36.0-r0 zlib-dev=1.2.13-r0 --update --no-cache -t build-dependencies \
 && mkdir -p /usr/src \
 && cd /usr/src \
 && wget http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz \
 && tar zxvf nginx-$NGINX_VERSION.tar.gz \
 && git clone -b master --single-branch https://github.com/leev/ngx_http_geoip2_module.git \
 && cd nginx-$NGINX_VERSION \
 && ./configure --with-compat --add-dynamic-module=../ngx_http_geoip2_module \
 && make modules \
 && cp objs/ngx_http_geoip2_module.so /etc/nginx/modules \
 && apk del build-dependencies \
 && rm -rf /usr/src/nginx-* /usr/src/ngx_http_geoip2_module /var/cache/apk/* /var/www/* /tmp/*
RUN apk add geoip=1.6.12-r3 inotify-tools=3.22.6.0-r0 libmaxminddb=1.7.1-r0 php7 php7-cli php7-ctype php7-curl php7-dom php7-iconv php7-fpm php7-gd php7-json php7-ldap php7-mbstring php7-opcache php7-openssl php7-pdo php7-pdo_mysql php7-redis php7-session php7-simplexml php7-xml php7-zlib ssmtp=2.64-r18 supervisor=4.2.4-r0 tzdata=2023c-r0 wget=1.21.3-r2 --update --no-cache \
 && rm -rf /var/cache/apk/* /var/www/* /tmp/*
RUN cd /tmp \
 && mkdir -p /etc/nginx/geoip \
 && wget -q http://geolite.maxmind.com/download/geoip/database/GeoLite2-City.tar.gz \
 && wget -q http://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz \
 && wget -q http://geolite.maxmind.com/download/geoip/database/GeoLite2-ASN.tar.gz \
 && tar -xvzf GeoLite2-City.tar.gz --strip-components=1 \
 && tar -xvzf GeoLite2-Country.tar.gz --strip-components=1 \
 && tar -xvzf GeoLite2-ASN.tar.gz --strip-components=1 \
 && mv *.mmdb /etc/nginx/geoip \
 && rm -rf /tmp/*
ENV MATOMO_VERSION="3.9.1" \
    CRONTAB_PATH="/var/spool/cron/crontabs"
RUN apk add ca-certificates=20220614-r4 gnupg=2.2.40-r0 libressl=3.6.2-r0 tar=1.34-r2 --update --no-cache -t build-dependencies \
 && mkdir -p /var/www \
 && cd /tmp \
 && wget -q https://builds.matomo.org/piwik-${MATOMO_VERSION}.tar.gz \
 && wget -q https://builds.matomo.org/piwik-${MATOMO_VERSION}.tar.gz.asc \
 && wget -q https://builds.matomo.org/signature.asc \
 && gpg --import signature.asc \
 && gpg --verify --batch --no-tty piwik-${MATOMO_VERSION}.tar.gz.asc piwik-${MATOMO_VERSION}.tar.gz \
 && tar -xzf piwik-${MATOMO_VERSION}.tar.gz --strip 1 -C /var/www \
 && wget -q https://matomo.org/wp-content/uploads/unifont.ttf.zip \
 && unzip unifont.ttf.zip -d /var/www/plugins/ImageGraph/fonts/ \
 && rm unifont.ttf.zip \
 && chown -R nginx. /etc/nginx /usr/lib/nginx /var/cache/nginx /var/log/nginx /var/log/php7 /var/www \
 && apk del build-dependencies \
 && rm -rf /root/.gnupg /tmp/* /var/cache/apk/*
COPY entrypoint.sh /entrypoint.sh
COPY assets /
RUN chmod a+x /entrypoint.sh /usr/local/bin/* \
 && chown nginx. /var/www/bootstrap.php
EXPOSE 8000/tcp
WORKDIR /var/www
VOLUME [ "/data" ]
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
