FROM alpine:3.7
ENV TIMEZONE="Europe/Moscow"
ENV PHP_MEMORY_LIMIT="512M"
ENV PHP_LOGFILE="/var/log/php-errors.log"
ENV MAX_UPLOAD="50M"
ENV PHP_MAX_FILE_UPLOAD="200"
ENV PHP_MAX_POST="100M"
ENV LANG="en_US.utf8"
ENV PGDATA="/var/lib/postgresql/data"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV YARN_CACHE_FOLDER="/home/user/.yarn-cache"
ENV COMPOSER_CACHE_DIR="/home/user/.composer-cache"
ENV DB_PORT="5532"
ENV MIMIR_URL="http://localhost:4001"
ENV RHEDA_URL="http://localhost:4002"
ENV TYR_URL="http://localhost:4003"
ENV FREY_URL="http://localhost:4004"
ENV IS_DOCKER="1"
#   these should match auth data in dbinit.sql
ENV PHINX_DB_NAME="mimir"
ENV PHINX_DB_NAME_UNIT="mimir_unit"
ENV PHINX_DB_USER="mimir"
ENV PHINX_DB_PASS="pgpass"
ENV PHINX_DB_PORT="$DB_PORT"
#   these should match auth data in dbinit_frey.sql
ENV PHINX_DB_FREY_NAME="frey"
ENV PHINX_DB_FREY_NAME_UNIT="frey_unit"
ENV PHINX_DB_FREY_USER="frey"
ENV PHINX_DB_FREY_PASS="pgpass"
ENV PHINX_DB_FREY_PORT="$DB_PORT"
RUN apk update \
 && apk upgrade \
 && apk add tzdata=2019c-r0 --update \
 && cp /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && echo "${TIMEZONE}" > /etc/timezone \
 && apk add curl=7.61.1-r3 make=4.2.1-r0 gettext=0.19.8.1-r1 git=2.15.4-r0 nginx=1.12.2-r4 postgresql=10.10-r0 nodejs=8.9.3-r1 nodejs-npm=8.9.3-r1 php7-mcrypt=7.1.33-r0 php7-soap=7.1.33-r0 php7-gettext=7.1.33-r0 php7-intl=7.1.33-r0 php7-tokenizer=7.1.33-r0 php7-mbstring=7.1.33-r0 php7-simplexml=7.1.33-r0 php7-openssl=7.1.33-r0 php7-gmp=7.1.33-r0 php7-phar=7.1.33-r0 php7-json=7.1.33-r0 php7-pdo=7.1.33-r0 php7-pdo_pgsql=7.1.33-r0 php7-pgsql=7.1.33-r0 php7-gd=7.1.33-r0 php7-gettext=7.1.33-r0 php7-xmlreader=7.1.33-r0 php7-xmlwriter=7.1.33-r0 php7-xmlrpc=7.1.33-r0 php7-phpdbg=7.1.33-r0 php7-iconv=7.1.33-r0 php7-curl=7.1.33-r0 php7-ctype=7.1.33-r0 php7-fpm=7.1.33-r0 php7-apcu=5.1.11-r0 --update
RUN curl -o /usr/local/bin/gosu -sSL "https://github.com/tianon/gosu/releases/download/1.2/gosu-amd64" \
 && chmod +x /usr/local/bin/gosu
RUN npm install xgettext-template@4.1.2 i18n-stex@1.0.12 i18n-po-json@1.1.0 i18n-json-po@1.0.7 yarn@1.22.19 -g
RUN touch $PHP_LOGFILE
RUN chown nobody $PHP_LOGFILE
#   Set environments
RUN sed -i "s|;*daemonize\s*=\s*yes|daemonize = no|g" /etc/php7/php-fpm.d/www.conf \
 && sed -i "s|;*clear_env\s*=\s*no|clear_env = no|g" /etc/php7/php-fpm.d/www.conf \
 && sed -i "s|;*listen\s*=\s*127.0.0.1:9000|listen = 9000|g" /etc/php7/php-fpm.d/www.conf \
 && sed -i "s|;*listen\s*=\s*/||g" /etc/php7/php-fpm.d/www.conf \
 && sed -i "s|;*date.timezone =.*|date.timezone = ${TIMEZONE}|i" /etc/php7/php.ini \
 && sed -i "s|;*memory_limit =.*|memory_limit = ${PHP_MEMORY_LIMIT}|i" /etc/php7/php.ini \
 && sed -i "s|;*error_log =.*|error_log = ${PHP_LOGFILE}|i" /etc/php7/php.ini \
 && sed -i "s|;*upload_max_filesize =.*|upload_max_filesize = ${MAX_UPLOAD}|i" /etc/php7/php.ini \
 && sed -i "s|;*max_file_uploads =.*|max_file_uploads = ${PHP_MAX_FILE_UPLOAD}|i" /etc/php7/php.ini \
 && sed -i "s|;*post_max_size =.*|post_max_size = ${PHP_MAX_POST}|i" /etc/php7/php.ini \
 && sed -i "s|;*cgi.fix_pathinfo=.*|cgi.fix_pathinfo = 0|i" /etc/php7/php.ini \
 && sed -i "s|;*opcache.enable=.*|opcache.enable = 1|i" /etc/php7/php.ini \
 && sed -i "s|;*opcache.enable_cli=.*|opcache.enable_cli = 1|i" /etc/php7/php.ini \
 && sed -i "s|;*opcache.memory_consumption=.*|opcache.memory_consumption = 128|i" /etc/php7/php.ini \
 && sed -i "s|;*opcache.interned_strings_buffer=.*|opcache.interned_strings_buffer=8|i" /etc/php7/php.ini \
 && sed -i "s|;*opcache.max_accelerated_files=.*|opcache.max_accelerated_files=4000|i" /etc/php7/php.ini \
 && sed -i "s|;*opcache.fast_shutdown=.*|opcache.fast_shutdown=1|i" /etc/php7/php.ini
#   Cleaning up
RUN mkdir /www \
 && apk del tzdata \
 && rm -rf /var/cache/apk/*
RUN ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log \
 && ln -sf /dev/stderr /var/log/php7.1-fpm.log
#   Expose ports
EXPOSE 4001/tcp 4002/tcp 4003/tcp 4004/tcp $DB_PORT
#   copy entry point
COPY entrypoint.sh /entrypoint.sh
RUN chmod 755 /entrypoint.sh
#   copy nginx configs
COPY Rheda/rheda-docker.nginx.conf /etc/nginx/conf.d/rheda.conf
COPY Mimir/mimir-docker.nginx.conf /etc/nginx/conf.d/mimir.conf
COPY Frey/frey-docker.nginx.conf /etc/nginx/conf.d/frey.conf
#   copy db init scripts
RUN mkdir -p /docker-entrypoint-initdb.d
COPY dbinit.sql /docker-entrypoint-initdb.d/dbinit.sql
COPY dbinit_frey.sql /docker-entrypoint-initdb.d/dbinit_frey.sql
#   Folders init
RUN mkdir -p /run/postgresql \
 && chown postgres /run/postgresql
RUN mkdir -p /run/nginx
RUN mkdir -p /var/www/html/Tyr
RUN mkdir -p /var/www/html/Mimir
RUN mkdir -p /var/www/html/Rheda
RUN mkdir -p /var/www/html/Frey
RUN mkdir -p /var/www/html/pantheon
#   Entry point
CMD ["/entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
