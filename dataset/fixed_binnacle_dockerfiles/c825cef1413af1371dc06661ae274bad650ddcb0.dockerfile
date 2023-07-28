FROM alpine:edge
MAINTAINER Etopian Inc. <contact@etopian.com>
LABEL devoply.type="site" \
      devoply.cms="drupal" \
      devoply.framework="drupal" \
      devoply.language="php" \
      devoply.require="mariadb etopian/nginx-proxy" \
      devoply.recommend="redis" \
      devoply.description="Drupal on Nginx and PHP-FPM with Drush." \
      devoply.name="Drupal"
#   BUILD NGINX
ENV NGINX_VERSION="nginx-1.9.12"
#   Add s6-overlay
ENV S6_OVERLAY_VERSION="v1.17.2.0"
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/s6-overlay.tar.gz https://github.com/just-containers/s6-overlay/releases/download/${S6_OVERLAY_VERSION}/s6-overlay-amd64.tar.gz
RUN tar xvfz /tmp/s6-overlay.tar.gz -C /
COPY root /
#   Add the files
RUN rm /etc/s6/services/s6-fdholderd/down
RUN apk add nginx=1.22.1-r0 --update \
 && apk del nginx
RUN apk add pcre=8.45-r2 openssl-dev=3.1.0-r2 pcre-dev=8.45-r2 zlib-dev=1.2.13-r0 wget=1.21.3-r3 build-base=0.5-r3 ca-certificates=20230106-r0 git=2.40.0-r0 linux-headers=6.2-r0 --update \
 && mkdir -p /tmp/src \
 && cd /tmp/src \
 && wget http://nginx.org/download/${NGINX_VERSION}.tar.gz \
 && wget https://raw.githubusercontent.com/masterzen/nginx-upload-progress-module/master/ngx_http_uploadprogress_module.c \
 && git clone https://github.com/masterzen/nginx-upload-progress-module /tmp/nginx-upload-progress-module \
 && tar -zxvf ${NGINX_VERSION}.tar.gz \
 && cd /tmp/src/${NGINX_VERSION} \
 && ./configure --conf-path=/etc/nginx/nginx.conf --with-http_ssl_module --with-http_gzip_static_module --with-pcre --with-file-aio --with-http_flv_module --with-http_realip_module --with-http_mp4_module --with-http_stub_status_module --with-http_gunzip_module --add-module=/tmp/nginx-upload-progress-module --prefix=/etc/nginx --http-log-path=/var/log/nginx/access.log --error-log-path=/var/log/nginx/error.log --sbin-path=/usr/local/sbin/nginx \
 && make \
 && make install \
 && apk del --purge build-base openssl-dev zlib-dev git \
 && rm -rf /tmp/* \
 && rm -rf /var/cache/apk/*
RUN apk update \
 && apk add bash=5.2.15-r2 less=608-r1 vim=9.0.1440-r0 ca-certificates=20230106-r0 php-fpm php-json php-zlib php-xml php-pdo php-phar php-openssl php-pdo_mysql php-mysqli php-gd php-iconv php-mcrypt php-mysql php-curl php-opcache php-ctype php-apcu php-intl php-bcmath php-dom php-xmlreader curl=8.0.1-r1 git=2.40.0-r0 mysql-client=10.11.2-r4 php-pcntl php-posix apk-cron=1.0-r3 postfix=3.7.4-r0 musl=1.2.3_git20230322-r0
RUN rm -rf /var/cache/apk/* \
 && rm -rvf /etc/nginx \
 && mkdir -p /etc/nginx
COPY files/nginx/ /etc/nginx/
COPY files/php-fpm.conf /etc/php/
COPY files/drush.sh /
COPY files/postfix/main.cf /etc/postfix/main.cf.new
COPY files/postfix/setup_ses.sh /setup_ses.sh
RUN mkdir -p /DATA/htdocs \
 && mkdir -p /DATA/logs/{nginx,php-fpm} \
 && mkdir -p /var/log/nginx/ \
 && mkdir -p /var/log/php-fpm/ \
 && chown -R nginx:nginx /var/log/nginx/ \
 && mkdir -p /var/cache/nginx/microcache \
 && chown -R nginx:nginx /var/cache/nginx/microcache \
 && mkdir -p /var/www/localhost/htdocs \
 && chown -R nginx:nginx /var/www/localhost/htdocs \
 && chown -R nginx:nginx /DATA \
 && chmod +x /setup_ses.sh \
 && chmod +x /drush.sh
RUN sed -i 's/nginx:x:100:101:Linux User,,,:\/var\/www\/localhost\/htdocs:\/sbin\/nologin/nginx:x:100:101:Linux User,,,:\/var\/www\/localhost\/htdocs:\/bin\/bash/g' /etc/passwd \
 && sed -i 's/nginx:x:100:101:Linux User,,,:\/var\/www\/localhost\/htdocs:\/sbin\/nologin/nginx:x:100:101:Linux User,,,:\/var\/www\/localhost\/htdocs:\/bin\/bash/g' /etc/passwd- \
 && sed -i 's/;cgi.fix_pathinfo=1/cgi.fix_pathinfo=0/g' /etc/php/php.ini \
 && crontab -u nginx -l | { cat ;echo "*/15 * * * * /usr/bin/drush --root=/DATA/htdocs core-cron --yes" ; } | crontab -u nginx -
#   configure postfix use to amazon ses to send mail.
ENV SES_HOST="email-smtp.us-east-1.amazonaws.com" \
    SES_PORT="587" \
    SES_USER="" \
    SES_SECRET="" \
    TERM="xterm" \
    DB_HOST="172.17.0.1" \
    DB_USER="" \
    DB_PASS="" \
    DB_NAME=""
RUN /setup_ses.sh \
 && /drush.sh
EXPOSE 80/tcp
VOLUME ["/DATA"]
ENTRYPOINT ["/init"]
#  CMD ["/run.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
