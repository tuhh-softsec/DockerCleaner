FROM alpine:3.7
MAINTAINER theslydetector@gmail.com
#   Configure Timezone
ENV TIMEZONE="\"America/Chicago\""
RUN rm -f /etc/localtime \
 && ln -s "/usr/share/zoneinfo/${TIMEZONE}" /etc/localtime \
 && echo "${TIMEZONE}" > /etc/timezone
RUN apk add bash=4.4.19-r1 bwm-ng=0.6.1-r3 coreutils=8.28-r0 curl=7.61.1-r3 ffmpeg=3.4-r1 file=5.32-r2 findutils=4.6.0-r0 git=2.15.4-r0 htop=2.0.2-r0 iproute2=4.13.0-r0 lame=3.100-r0 less=520-r0 make=4.2.1-r0 mariadb-client=10.1.41-r0 memcached=1.5.6-r0 musl=1.1.18-r4 nginx=1.12.2-r4 p7zip=16.02-r3 php7-ctype=7.1.33-r0 php7-curl=7.1.33-r0 php7-dev=7.1.33-r0 php7-exif=7.1.33-r0 php7-fpm=7.1.33-r0 php7-gd=7.1.33-r0 php7-iconv=7.1.33-r0 php7-imagick=3.4.3-r3 php7-json=7.1.33-r0 php7-mcrypt=7.1.33-r0 php7-opcache=7.1.33-r0 php7-openssl=7.1.33-r0 php7-pcntl=7.1.33-r0 php7-pdo=7.1.33-r0 php7-pdo_mysql=7.1.33-r0 php7-pear=7.1.33-r0 php7-phar=7.1.33-r0 php7-posix=7.1.33-r0 php7-redis=3.1.4-r0 php7-session=7.1.33-r0 php7-simplexml=7.1.33-r0 php7-sockets=7.1.33-r0 php7-xmlwriter=7.1.33-r0 php7-zlib pigz=2.3.4-r2 proxychains-ng=4.12-r0 pstree=2.39-r0 py2-pip=9.0.1-r1 python s6=2.6.1.1-r0 strace=4.19-r0 tar=1.32-r0 tig=2.3.0-r0 tree=1.7.0-r1 tzdata=2019c-r0 unrar=5.5.8-r0 unzip=6.0-r3 util-linux=2.31.1-r0 vim=8.0.1359-r2 wget=1.20.3-r0 zendframework=2.4.13-r0 --update \
 && rm -rf /var/cache/apk/*
#   vnstat in testing repo
#   mytop + deps
RUN apk add mariadb=10.1.41-r0 perl=5.26.3-r0 perl-dbd-mysql=4.043-r0 perl-term-readkey=2.37-r1 --update \
 && rm -rf /var/cache/apk/*
#   Install composer
RUN curl https://getcomposer.org/installer | php7 -- --install-dir=/usr/bin --filename=composer
#   Build and install mediainfo
ENV MEDIAINFO_VERSION="18.05"
RUN apk add gcc=6.4.0-r5 g++=6.4.0-r5 --update \
 && mkdir -p /tmp \
 && cd /tmp \
 && curl -s -o mediainfo.tar.gz https://mediaarea.net/download/binary/mediainfo/${MEDIAINFO_VERSION}/MediaInfo_CLI_${MEDIAINFO_VERSION}_GNU_FromSource.tar.gz \
 && tar xzvf mediainfo.tar.gz \
 && cd MediaInfo_CLI_GNU_FromSource \
 && ./CLI_Compile.sh \
 && cd MediaInfo/Project/GNU/CLI \
 && make install \
 && cd / \
 && rm -rf /tmp \
 && apk del --purge gcc g++ \
 && rm -rf /var/cache/apk/*
#   Install Python MySQL Modules
RUN pip install pip==23.1 --upgrade \
 && pip install setuptools==67.6.1 --upgrade \
 && pip install cymysql==0.9.18 pynntp==1.0.2 socketpool==0.5.3
#   Configure PHP
RUN sed -ri 's/(max_execution_time =) ([0-9]+)/\1 120/' /etc/php7/php.ini \
 && sed -ri "s/(memory_limit =) (.*$)/\1 -1/" /etc/php7/php.ini \
 && sed -ri 's/;(date.timezone =)/\1 America\/Chicago/' /etc/php7/php.ini \
 && sed -ri 's/listen\s*=\s*127.0.0.1:9000/listen = 9000/g' /etc/php7/php-fpm.d/www.conf \
 && sed -ri 's|;include_path = ".:/php/includes"|include_path = ".:/usr/share/php7"|g' /etc/php7/php.ini \
 && mkdir -p /var/log/php-fpm/
#   Install and configure nginx.
RUN mkdir -p /var/log/nginx \
 && mkdir -p /etc/nginx \
 && mkdir -p /tmp/nginx \
 && chmod 755 /var/log/nginx \
 && chmod 777 /tmp \
 && touch /var/log/nginx/nginx-error.log
#   Clone nZEDb and set directory permissions
ENV NZEDB_VERSION="\"v0.8.7.0\""
RUN mkdir -p /var/www \
 && cd /var/www \
 && git clone https://github.com/nZEDb/nZEDb.git \
 && cd /var/www/nZEDb \
 && git checkout --quiet --force $NZEDB_VERSION \
 && composer install \
 && chmod -R 777 /var/www/nZEDb/ \
 && find . -name ".git" -type d | grep -v "\.\/\.git" | xargs rm -rf \
 && composer clear-cache
#   Build tmux 2.0 since tmux 2.2 has issues: https://github.com/nZEDb/nZEDb/issues/2182 
ENV TMUX_VERSION="2.0"
RUN apk add gcc=6.4.0-r5 g++=6.4.0-r5 ncurses-dev=6.0_p20171125-r1 libevent-dev=2.1.8-r2 bsd-compat-headers=0.7.1-r0 --update \
 && mkdir -p /tmp/tmux \
 && cd /tmp/tmux \
 && curl --location -o tmux.tar.gz https://github.com/tmux/tmux/releases/download/${TMUX_VERSION}/tmux-${TMUX_VERSION}.tar.gz \
 && tar xzvf tmux.tar.gz \
 && cd tmux-${TMUX_VERSION} \
 && ./configure --prefix /usr \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/tmux \
 && apk del --purge gcc g++ ncurses-dev libevent-dev bsd-compat-headers \
 && rm -rf /var/cache/apk/*
#   Build and install php-yenc
ENV PHP_ZEPHIR_PARSER_VERSION="v1.1.2"
RUN cd /tmp \
 && apk add gcc=6.4.0-r5 re2c=1.0.2-r0 libc-dev=0.7.1-r0 sudo=1.8.21_p2-r1 --update \
 && mkdir -p /tmp/zephir \
 && cd /tmp/zephir \
 && composer require phalcon/zephir \
 && cd /tmp \
 && git clone git://github.com/phalcon/php-zephir-parser.git \
 && cd php-zephir-parser \
 && git checkout --quiet --force $PHP_ZEPHIR_PARSER_VERSION \
 && ./install \
 && echo "extension=zephir_parser.so" > /etc/php7/conf.d/98_zephir_parser.ini \
 && cd /tmp \
 && git clone https://github.com/niel/php-yenc.git \
 && cd php-yenc \
 && /tmp/zephir/vendor/bin/zephir install \
 && echo "extension=yenc.so" > /etc/php7/conf.d/99_yenc.ini \
 && composer clear-cache \
 && cd /tmp \
 && rm -rf zephir php-yenc php-zephir-parser \
 && apk del --purge gcc re2c libc-dev sudo
#   Build and install par2
ENV PAR2_VERSION="\"v0.8.0\""
RUN apk add gcc=6.4.0-r5 autoconf=2.69-r0 automake=1.15.1-r0 g++=6.4.0-r5 python-dev openssl-dev=1.0.2t-r0 libffi-dev=3.2.1-r4 --update \
 && git clone https://github.com/Parchive/par2cmdline.git /tmp/par2 \
 && cd /tmp/par2 \
 && git checkout --quiet --force $PAR2_VERSION \
 && ./automake.sh \
 && ./configure --prefix=/usr \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/par2 \
 && apk del --purge automake gcc autoconf g++ python-dev openssl-dev libffi-dev \
 && apk add libgomp=6.4.0-r5
#   Create dir for importing nzbs
RUN mkdir -p /var/www/nZEDb/resources/import
#   Switch out php executable to instrument invocations
RUN mv /usr/bin/php /usr/bin/php.real
COPY php.proxy /usr/bin/php
#   Use pigz (parallel gzip) instead of gzip to speed up db backups
RUN mv /bin/gzip /bin/gzip.real \
 && ln -s /usr/bin/pigz /bin/gzip
#   iconv has issues in musl which affects NFO conversion to include
#   cool ascii chars. Remove the problematic parts - TRANSLIT and IGNORE
#   See https://github.com/slydetector/simply-nzedb/issues/31
RUN sed -i "s|UTF-8//IGNORE//TRANSLIT|UTF-8|g" /var/www/nZEDb/nzedb/utility/Text.php
LABEL nzedb="$NZEDB_VERSION" \
      maintainer="theslydetector@gmail.com" \
      url="https://github.com/slydetector/simply-nzedb"
RUN mkdir -p /var/www/nZEDb/resources/tmp \
 && chmod 777 /var/www/nZEDb/resources/tmp
ENV TERM="tmux"
EXPOSE 8800/tcp
COPY s6 /etc/s6
CMD ["/bin/s6-svscan", "/etc/s6"]
WORKDIR /var/www/nZEDb/misc/update
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
