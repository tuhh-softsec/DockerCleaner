#
#   nZEDb Main Dockerfile
#   Create a quick and clean dev environment
#
FROM alpine:3.3
MAINTAINER razorgirl <https://github.com/razorgirl>
#   Set correct environment variables.
ENV TZ="Europe/London"
ENV HOME="/root"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
#   Make sure system is up-to-date.
RUN apk update \
 && apk upgrade
#   Create www-data user.
RUN set -x \
 && addgroup -g 82 -S www-data \
 && adduser -u 82 -D -S -G www-data www-data
#   Install basic software.
RUN apk add curl=7.55.0-r2 git=2.8.6-r0 man=1.13.3-r5 unzip=6.0-r1 vim=7.4.943-r5 wget=1.17.1-r2 tmux=2.1-r2 chrony=2.2.1-r0 build-base=0.4-r1
#   Install additional software.
#   (tcptrack and nmon won't work on Alpine Linux)
RUN apk add htop=1.0.3-r2 bwm-ng=0.6.1-r3
#   Install vnstat from source, since not available as Alpine Linux package.
RUN cd /tmp \
 && wget http://humdi.net/vnstat/vnstat-1.16.tar.gz \
 && tar zxf vnstat-1.16.tar.gz \
 && cd vnstat-1.16 \
 && ./configure \
 && make \
 && make install
#   Install mytop from source, since not available as Alpine Linux package.
RUN apk add perl=5.22.3-r0 perl-dbi=1.634-r0 perl-dbd-mysql=4.033-r0 perl-term-readkey=2.32-r2 \
 && cd /tmp \
 && wget http://jeremy.zawodny.com/mysql/mytop/mytop-1.6.tar.gz \
 && tar zxf mytop-1.6.tar.gz \
 && cd mytop-1.6 \
 && perl Makefile.PL \
 && make \
 && make install
#   Install MediaInfo from source, since not available as Alpine Linux package.
RUN cd /tmp \
 && wget https://mediaarea.net/download/binary/mediainfo/0.7.91/MediaInfo_CLI_0.7.91_GNU_FromSource.tar.xz \
 && tar Jxf MediaInfo_CLI_0.7.91_GNU_FromSource.tar.xz \
 && cd MediaInfo_CLI_GNU_FromSource \
 && ./CLI_Compile.sh \
 && cd MediaInfo/Project/GNU/CLI \
 && make install
#   Install ffmpeg, p7zip, unrar and lame.
RUN apk add ffmpeg=2.8.11-r0 unrar=5.3.4-r0 lame=3.99.5-r6 p7zip=9.38.1-r2
#   Install Python modules.
RUN apk add python=2.7.12-r0 python3=3.5.1-r0 py-pip=7.1.2-r0 \
 && pip install pip==23.1 --upgrade \
 && pip install cymysql==0.9.18 \
 && pip install pynntp==1.0.2 \
 && pip install socketpool==0.5.3 \
 && pip list \
 && pip3 install --upgrade pip \
 && pip3 install cymysql \
 && pip3 install pynntp \
 && pip3 install socketpool \
 && pip3 list
#   Install PHP.
RUN apk add php5 php5-pear php5-gd php5-mysql php5-curl php5-json php5-phar php5-imagick php5-pdo_mysql php5-mcrypt php5-common php5-xml php5-openssl php5-zlib php5-ctype php5-exif php5-iconv php5-sockets
RUN sed -ri 's/(max_execution_time =) ([0-9]+)/\1 120/' /etc/php5/php.ini
RUN sed -ri 's/(memory_limit =) ([0-9]+)/\1 -1/' /etc/php5/php.ini
RUN sed -ri 's/;(date.timezone =)/\1 Europe\/London/' /etc/php5/php.ini
#   Setup the Composer installer.
RUN curl -o /tmp/composer-setup.php https://getcomposer.org/installer \
 && curl -o /tmp/composer-setup.sig https://composer.github.io/installer.sig \
 && php -r "if (hash('SHA384', file_get_contents('/tmp/composer-setup.php')) !== trim(file_get_contents('/tmp/composer-setup.sig'))) { unlink('/tmp/composer-setup.php'); echo 'Invalid installer' . PHP_EOL; exit(1); }" \
 && cd /tmp \
 && php composer-setup.php --install-dir=/usr/local/bin --filename=composer
#   Clone nZEDb master.
RUN git clone https://github.com/nZEDb/nZEDb.git /var/www/nZEDb
#   Set directory permissions.
RUN chmod -R 755 /var/www/nZEDb \
 && chgrp www-data /var/www/nZEDb/resources/smarty/templates_c \
 && chmod 775 /var/www/nZEDb/resources/smarty/templates_c \
 && chgrp -R www-data /var/www/nZEDb/resources/covers \
 && chmod -R 775 /var/www/nZEDb/resources/covers \
 && chgrp www-data /var/www/nZEDb/www \
 && chmod 775 /var/www/nZEDb/www \
 && chgrp www-data /var/www/nZEDb/www/install \
 && chmod 775 /var/www/nZEDb/www/install \
 && chgrp -R www-data /var/www/nZEDb/resources/nzb \
 && chmod -R 775 /var/www/nZEDb/resources/nzb \
 && chmod -R 777 /var/www/nZEDb/nzedb/config
#   Install dependencies.
RUN cd /var/www/nZEDb \
 && composer install --prefer-source
#   Add pseudo run command.
COPY sleep.sh /run.sh
#   Define mountable directories.
VOLUME ["/var/www/nZEDb"]
#   Run.
CMD ["/run.sh"]
#   Clean up when done.
RUN rm -rf /tmp/* /var/tmp/*
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
