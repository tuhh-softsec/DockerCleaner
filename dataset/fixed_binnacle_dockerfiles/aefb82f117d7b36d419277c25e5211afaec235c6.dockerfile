#   Copyright 2015 Google Inc.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   Dockerfile for PHP 7.1/7.2 using nginx as the webserver.
FROM gcr.io/gcp-runtimes/ubuntu_16_0_4
#   Install build scripts - composer, nginx, php
COPY build-scripts /build-scripts
#   Files for stackdriver setup
COPY stackdriver-files /stackdriver-files
RUN chown -R www-data /build-scripts /stackdriver-files
RUN apt-get update -y \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 gettext=0.21-11 git=1:2.39.2-1ubuntu1 libbz2-1.0=1.0.8-5build1 libcurl3-gnutls=7.88.1-7ubuntu1 libgmp10=2:6.2.1+dfsg1-1.1ubuntu1 libicu55 libjpeg-turbo8=2.1.5-2ubuntu1 liblua5.3-0=5.3.6-2 libmcrypt4=2.5.8-7 libmemcached11=1.1.4-1 libmemcachedutil2=1.1.4-1 libpcre3=2:8.39-15 libpng12-0 libpq5=15.2-1 libreadline6 librecode0=3.6-25 libsasl2-modules=2.1.28+dfsg-10 libsqlite3-0=3.40.1-1 libxml2=2.9.14+dfsg-1.1build2 libxslt1.1=1.1.35-1 mercurial=6.3.2-1 nginx-extras=1.22.0-1ubuntu3 sasl2-bin=2.1.28+dfsg-10 subversion=1.14.2-4build2 supervisor=4.2.1-1ubuntu1 zlib1g=1:1.2.13.dfsg-1ubuntu4 -y \
 && /bin/bash /build-scripts/apt-cleanup.sh
ENV NGINX_DIR="/etc/nginx" \
    PHP_DIR="/opt/php" \
    PHP_CONFIG_TEMPLATE="/opt/php-configs" \
    PHP71_DIR="/opt/php71" \
    PHP72_DIR="/opt/php72" \
    APP_DIR="/app" \
    NGINX_USER_CONF_DIR="/etc/nginx/conf.d" \
    UPLOAD_DIR="/upload" \
    SESSION_SAVE_PATH="/tmp/sessions" \
    PATH="/opt/php/bin:$PATH" \
    WWW_HOME="/var/www" \
    COMPOSER_HOME="/opt/composer" \
    DOCUMENT_ROOT="/app" \
    FRONT_CONTROLLER_FILE="index.php"
ARG RUNTIME_DISTRIBUTION="gcp-php-runtime-xenial-unstable"
COPY ${RUNTIME_DISTRUBTION} /${RUNTIME_DISTRUBTION}
RUN mkdir -p $PHP_CONFIG_TEMPLATE
COPY php-fpm.conf php.ini php-cli.ini "${PHP_CONFIG_TEMPLATE}/"/
RUN apt-get update -y \
 && dpkg -i --force-depends /${RUNTIME_DISTRIBUTION}/*.deb \
 && apt-get install --no-install-recommends -yf \
 && (curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - ) \
 && /bin/bash /build-scripts/dump_php_versions.sh \
 && /bin/bash /build-scripts/apt-cleanup.sh \
 && rm -rf /${RUNTIME_DISTRIBUTION}
EXPOSE 8080/tcp
#   Lock down the web directories
RUN mkdir -p $APP_DIR $UPLOAD_DIR $SESSION_SAVE_PATH $NGINX_USER_CONF_DIR $WWW_HOME $COMPOSER_HOME \
 && chown -R www-data.www-data $APP_DIR $UPLOAD_DIR $SESSION_SAVE_PATH $NGINX_USER_CONF_DIR $WWW_HOME $COMPOSER_HOME \
 && chmod 755 $UPLOAD_DIR $SESSION_SAVE_PATH $COMPOSER_HOME \
 && ln -sf ${PHP_DIR}/bin/php /usr/bin/php
#   Linking for easy access to php with `su www-data -c $CMD`
#   Put other config and shell files into place.
COPY nginx.conf fastcgi_params gzip_params "${NGINX_DIR}/"/
COPY nginx-app.conf nginx-http.conf "${NGINX_USER_CONF_DIR}/"/
COPY supervisord.conf /etc/supervisor/supervisord.conf
RUN chmod +x /build-scripts/entrypoint.sh /build-scripts/composer.sh
WORKDIR $APP_DIR
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord.conf"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
