#   Based on ablyler's https://github.com/ablyler/docker-php7ast/blob/master/Dockerfile, which is out of date.
#   The original Dockerfile's license is below; the Dockerfile has been modified.
#
#   The MIT License (MIT)
#
#   Copyright (c) 2015 Andy Blyler
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#   SOFTWARE.
FROM alpine:3.9
WORKDIR /usr/src/app
RUN adduser -u 9000 -D app
ENV LAST_MODIFIED_DATE="2018-01-20"
ENV PHP_AST_VERSION="1.0.1"
RUN apk add php7=7.2.33-r0 --no-cache \
 && test -d /etc/php7/conf.d || ((test -e /etc/php7/conf.d \
 && rm /etc/php7/conf.d ) \
 && mkdir /etc/php7/conf.d ) \
 && apk add php7-bcmath=7.2.33-r0 php7-ctype=7.2.33-r0 php7-curl=7.2.33-r0 php7-gd=7.2.33-r0 php7-gettext=7.2.33-r0 php7-iconv=7.2.33-r0 php7-intl=7.2.33-r0 php7-json=7.2.33-r0 php7-ldap=7.2.33-r0 php7-mbstring=7.2.33-r0 php7-mcrypt php7-mysqlnd=7.2.33-r0 php7-opcache=7.2.33-r0 php7-openssl=7.2.33-r0 php7-pdo_mysql=7.2.33-r0 php7-pdo_pgsql=7.2.33-r0 php7-pdo_sqlite=7.2.33-r0 php7-pgsql=7.2.33-r0 php7-phar=7.2.33-r0 php7-session=7.2.33-r0 php7-soap=7.2.33-r0 php7-sockets=7.2.33-r0 php7-sqlite3=7.2.33-r0 php7-tidy=7.2.33-r0 php7-tokenizer=7.2.33-r0 php7-xml=7.2.33-r0 php7-xmlreader=7.2.33-r0 php7-xmlrpc=7.2.33-r0 php7-xsl=7.2.33-r0 php7-zip=7.2.33-r0 php7-zlib --no-cache
RUN apk add bash=4.4.19-r1 autoconf=2.69-r2 openssl=1.1.1k-r0 make=4.2.1-r2 build-base=0.5-r1 php7-dev=7.2.33-r0 wget=1.20.3-r0 --no-cache \
 && wget -O php-ast.tar.gz https://github.com/nikic/php-ast/archive/v${PHP_AST_VERSION}.tar.gz \
 && tar -zxvf php-ast.tar.gz \
 && cd php-ast-${PHP_AST_VERSION} \
 && export CFLAGS=-O2 \
 && phpize7 \
 && ./configure --prefix=/usr --with-php-config=/usr/bin/php-config7 \
 && make -j3 \
 && make test \
 && make install \
 && cd .. \
 && rm -Rf php-ast-${PHP_AST_VERSION} php-ast.tar.gz \
 && apk del bash autoconf openssl make build-base php7-dev wget
COPY composer.json composer.lock ./
RUN apk add curl=7.64.0-r5 --no-cache \
 && curl -sS https://getcomposer.org/installer | php \
 && ./composer.phar install --no-dev --optimize-autoloader \
 && rm composer.phar \
 && apk del curl
COPY .phan .phan
COPY src src
COPY plugins/codeclimate/ast.ini /etc/php7/conf.d/
COPY plugins/codeclimate/engine /usr/src/app/plugins/codeclimate/engine
USER app
CMD ["/usr/src/app/plugins/codeclimate/engine"]
# Please add your HEALTHCHECK here!!!
