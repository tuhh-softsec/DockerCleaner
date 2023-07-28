FROM ubuntu:bionic
MAINTAINER Chilio 
ENV DEBIAN_FRONTEND="noninteractive"
ENV DEBCONF_NONINTERACTIVE_SEEN="true"
ENV DISPLAY=":99"
ENV SCREEN_RESOLUTION="1920x720x24"
ENV CHROMEDRIVER_PORT="9515"
ENV TMPDIR="/tmp"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=1.6.14 netcat-openbsd=1.187-1ubuntu0.1 -yq --fix-missing )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends language-pack-en-base=1:18.04+20180712 -yq --fix-missing )
ENV LC_ALL="en_US.UTF-8"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends openssl=1.1.1-1ubuntu2.1~18.04.21 -yq --fix-missing )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends zip=3.0-11build1 unzip=6.0-21ubuntu1.2 -yq --fix-missing )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 curl=7.58.0-2ubuntu3.24 -yq --fix-missing )
RUN add-apt-repository ppa:ondrej/php
RUN sed -i'' 's/archive\.ubuntu\.com/us\.archive\.ubuntu\.com/' /etc/apt/sources.list
RUN :
RUN apt-get upgrade -yq
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libgd-tools=2.2.5-4ubuntu0.5 -yq --fix-missing )
#   Install PHP 
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends php7.2=7.2.24-0ubuntu0.18.04.17 php7.2-bcmath=7.2.24-0ubuntu0.18.04.17 php7.2-bz2=7.2.24-0ubuntu0.18.04.17 php7.2-cli=7.2.24-0ubuntu0.18.04.17 php7.2-common=7.2.24-0ubuntu0.18.04.17 php7.2-curl=7.2.24-0ubuntu0.18.04.17 php7.2-fpm=7.2.24-0ubuntu0.18.04.17 php7.2-gd=7.2.24-0ubuntu0.18.04.17 php7.2-gmp=7.2.24-0ubuntu0.18.04.17 php7.2-imap=7.2.24-0ubuntu0.18.04.17 php7.2-interbase=7.2.24-0ubuntu0.18.04.17 php7.2-intl=7.2.24-0ubuntu0.18.04.17 php7.2-json=7.2.24-0ubuntu0.18.04.17 php7.2-ldap=7.2.24-0ubuntu0.18.04.17 php7.2-mbstring=7.2.24-0ubuntu0.18.04.17 php7.2-mysql=7.2.24-0ubuntu0.18.04.17 php7.2-opcache=7.2.24-0ubuntu0.18.04.17 php7.2-pgsql=7.2.24-0ubuntu0.18.04.17 php7.2-phpdbg=7.2.24-0ubuntu0.18.04.17 php7.2-pspell=7.2.24-0ubuntu0.18.04.17 php7.2-readline=7.2.24-0ubuntu0.18.04.17 php7.2-recode=7.2.24-0ubuntu0.18.04.17 php7.2-snmp=7.2.24-0ubuntu0.18.04.17 php7.2-soap=7.2.24-0ubuntu0.18.04.17 php7.2-sqlite3=7.2.24-0ubuntu0.18.04.17 php7.2-sybase=7.2.24-0ubuntu0.18.04.17 php7.2-tidy=7.2.24-0ubuntu0.18.04.17 php7.2-xml=7.2.24-0ubuntu0.18.04.17 php7.2-xmlrpc=7.2.24-0ubuntu0.18.04.17 php7.2-zip=7.2.24-0ubuntu0.18.04.17 php7.2-xsl=7.2.24-0ubuntu0.18.04.17 php-geoip=1.1.1-1build2 php-mongodb=1.3.4-1build1 php-redis=3.1.6-1build1 php-ssh2=1.1.2+0.13-1build1 php-uuid=1.0.4-4build2 php-zmq=1.1.3-5build2 php-radius=1.4.0~b1-6build2 php-http=3.1.0+2.6.0-4build8 php-uploadprogress=1.0.3.1-4-g95d8a0f-4build2 php-yaml=2.0.0+1.3.0-2build2 php-memcached=3.0.1+2.2.0-1build2 php-memcache=3.0.9~20160311.4991c2f-5build2 php-tideways=4.0.7-1build2 php-mailparse=3.0.2+2.1.6-12-gae1ef14-1build2 php-raphf=2.0.0+1.1.2-2build2 php-stomp=2.0.1+1.0.9-4 php-ds=1.1.8-1build1 php-sass=0.5.10-3build1 php-lua php-geos=1.0.0-2build2 php-xdebug=2.6.0-0ubuntu1 php-imagick=3.4.3~rc2-2ubuntu4.1 imagemagick=8:6.9.7.4+dfsg-16ubuntu6.15 nginx=1.14.0-0ubuntu1.11 -yq --fix-missing )
RUN update-alternatives --set php /usr/bin/php7.2
RUN update-alternatives --set phar /usr/bin/phar7.2
RUN update-alternatives --set phar.phar /usr/bin/phar.phar7.2
#   RUN update-alternatives --set phpize /usr/bin/phpize7.2
#   RUN update-alternatives --set php-config /usr/bin/php-config7.2
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends mc=3:4.8.19-1 lynx=2.8.9dev16-3 mysql-client=5.7.41-0ubuntu0.18.04.1 bzip2=1.0.6-8.1ubuntu0.2 make=4.1-9.1ubuntu1 g++=4:7.4.0-1ubuntu2.3 -yq --fix-missing )
#   Install Redis, Memcached, Beanstalk
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends redis-server=5:4.0.9-1ubuntu0.2 memcached=1.5.6-0ubuntu1.2 beanstalkd=1.10-4 -yq --fix-missing )
ENV COMPOSER_HOME="/usr/local/share/composer"
ENV COMPOSER_ALLOW_SUPERUSER="1"
ENV PATH="\"$COMPOSER_HOME:$COMPOSER_HOME/vendor/bin:$PATH\""
RUN mkdir -pv $COMPOSER_HOME \
 && chmod -R g+w $COMPOSER_HOME \
 && curl -o /tmp/composer-setup.php https://getcomposer.org/installer \
 && curl -o /tmp/composer-setup.sig https://composer.github.io/installer.sig \
 && php -r "if (hash('SHA384', file_get_contents('/tmp/composer-setup.php')) !== trim(file_get_contents('/tmp/composer-setup.sig'))) { unlink('/tmp/composer-setup.php'); echo 'Invalid installer' . PHP_EOL; exit(1); }" \
 && php /tmp/composer-setup.php --filename=composer --install-dir=$COMPOSER_HOME
COPY commands/xvfb.init.sh /etc/init.d/xvfb
COPY commands/start-nginx-ci-project.sh /usr/bin/start-nginx-ci-project
RUN chmod +x /usr/bin/start-nginx-ci-project
COPY commands/versions /usr/bin/versions
RUN chmod +x /usr/bin/versions
COPY configs/.bowerrc /root/.bowerrc
COPY commands/configure-laravel.sh /usr/bin/configure-laravel
RUN chmod +x /usr/bin/configure-laravel
COPY commands/chrome-system-check.sh /usr/bin/chrome-system-check
RUN chmod +x /usr/bin/chrome-system-check
COPY commands/chromedriver-compatibility-matrix.php /usr/bin/chromedriver-compatibility-matrix.php
RUN chmod +x /usr/bin/chromedriver-compatibility-matrix.php
COPY commands/dusk-versions-check.php /usr/bin/dusk-versions-check.php
RUN chmod +x /usr/bin/dusk-versions-check.php
RUN (apt-get update ;apt-get install --no-install-recommends xvfb=2:1.19.6-1ubuntu4.14 gconf2=3.2.6-4ubuntu1 fonts-ipafont-gothic=00303-18ubuntu1 xfonts-cyrillic=1:1.0.4 xfonts-100dpi=1:1.0.4+nmu1 xfonts-75dpi=1:1.0.4+nmu1 xfonts-base=1:1.0.4+nmu1 xfonts-scalable=1:1.0.3-1.1 -yq --fix-missing ) \
 && chmod +x /etc/init.d/xvfb \
 && CHROMEDRIVER_VERSION=`curl -sS chromedriver.storage.googleapis.com/LATEST_RELEASE ` \
 && mkdir -p /opt/chromedriver-$CHROMEDRIVER_VERSION \
 && curl -sS -o /tmp/chromedriver_linux64.zip http://chromedriver.storage.googleapis.com/$CHROMEDRIVER_VERSION/chromedriver_linux64.zip \
 && unzip -qq /tmp/chromedriver_linux64.zip -d /opt/chromedriver-$CHROMEDRIVER_VERSION \
 && rm /tmp/chromedriver_linux64.zip \
 && chmod +x /opt/chromedriver-$CHROMEDRIVER_VERSION/chromedriver \
 && ln -fs /opt/chromedriver-$CHROMEDRIVER_VERSION/chromedriver /usr/local/bin/chromedriver \
 && curl -sS -o - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list \
 && apt-get update -yq \
 && (apt-get update ;apt-get install --no-install-recommends google-chrome-stable x11vnc=0.9.13-3 rsync=3.1.2-2.1ubuntu1.6 -yq --fix-missing )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-transport-https=1.6.14 libpng-dev=1.6.34-1ubuntu0.18.04.2 -yq --fix-missing )
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -yq --fix-missing )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 -yq --fix-missing )
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends yarn -yq --fix-missing )
RUN yarn global add bower --network-concurrency 1
RUN wget https://phar.phpunit.de/phpunit.phar
RUN chmod +x phpunit.phar
RUN mv phpunit.phar /usr/local/bin/phpunit
RUN npm install node-gyp@9.3.1 -g
RUN npm install node-sass@8.0.0 --unsafe-perm -g
RUN npm install gulp@4.0.2 -g
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends supervisor=3.3.1-1.1 -yq --fix-missing )
COPY configs/supervisord.conf /etc/supervisor/supervisord.conf
COPY configs/nginx-default-site /etc/nginx/sites-available/default
RUN composer global require hirak/prestissimo
RUN npm set progress=false
VOLUME [ "/var/log/supervisor" ]
#   Clean system up
RUN apt-get -yq upgrade
RUN apt-get -yq autoremove
RUN apt-get -yq clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN systemctl enable xvfb
RUN versions
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION
LABEL org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.name="Laravel Dusk CI Docker" \
      org.label-schema.description="Test suite for Laravel Dusk in gitlab CI" \
      org.label-schema.url="https://hub.docker.com/r/chilio/laravel-dusk-ci/" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="https://github.com/chilio/laravel-dusk-ci" \
      org.label-schema.vendor="Chilio" \
      org.label-schema.version="$VERSION" \
      org.label-schema.schema-version="1.0.0"
EXPOSE 80/tcp 443/tcp 9515/tcp
CMD ["php-fpm7.2", "-F"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
