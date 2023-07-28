FROM ubuntu:bionic
ENV DEBIAN_FRONTEND="noninteractive"
ARG COMPOSER_HASH="48e3236262b34d30969dca3c37281b3b4bbe3221bda826ac6a9a62d6444cdb0dcd0615698a5cbe587c3f0fe57a54d8f5"
ARG NODE_VERSION="10.9.0"
ARG YARN_VERSION="1.9.4"
ARG USER_NAME="dev"
ARG USER_UID="1000"
ARG USER_GID="1000"
ENV TERM="xterm"
ENV LOCALTIME="Europe/Warsaw"
ENV PHP_VER="7.2"
ENV PHP_ETC_DIR="/etc/php/${PHP_VER}"
RUN apt-get update -q \
 && apt-get install --no-install-recommends apt-utils=1.6.14 gpg-agent=2.2.4-1ubuntu1.6 software-properties-common=0.96.24.32.20 build-essential=12.4ubuntu1 curl=7.58.0-2ubuntu3.24 -qyf \
 && apt-get -y remove cmdtest \
 && add-apt-repository ppa:adshares/releases -y \
 && add-apt-repository -y ppa:ondrej/php \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update -q \
 && apt-get install --no-install-recommends nmap=7.60-1ubuntu5 mtr=0.92-1 curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 vim=2:8.0.1453-1ubuntu1.11 less=487-0.1 dnsutils=1:9.11.3+dfsg-1ubuntu1.18 bash-completion=1:2.8-1ubuntu1 net-tools=1.60+git20161116.90da8a0-1ubuntu1 git=1:2.17.1-1ubuntu0.17 bzip2=1.0.6-8.1ubuntu0.2 zip=3.0-11build1 unzip=6.0-21ubuntu1.2 tree=1.7.0-5 mc=3:4.8.19-1 gettext-base=0.19.8.1-6ubuntu0.3 openssl=1.1.1-1ubuntu2.1~18.04.21 openssh-client=1:7.6p1-4ubuntu0.7 openssh-server=1:7.6p1-4ubuntu0.7 gnupg2=2.2.4-1ubuntu1.6 dirmngr=2.2.4-1ubuntu1.6 connect-proxy=1.105-1 iproute2=4.15.0-2ubuntu1.3 mysql-client=5.7.41-0ubuntu0.18.04.1 php-xdebug=2.6.0-0ubuntu1 php-apcu=5.1.9+4.0.11-1build1 ads nodejs=8.10.0~dfsg-2ubuntu0.4 yarn cron=3.0pl1-128.1ubuntu1.2 iputils-* php${PHP_VER}-fpm php${PHP_VER}-mysql php${PHP_VER}-bcmath php${PHP_VER}-bz2 php${PHP_VER}-curl php${PHP_VER}-gd php${PHP_VER}-intl php${PHP_VER}-json php${PHP_VER}-mbstring php${PHP_VER}-opcache php${PHP_VER}-readline php${PHP_VER}-sqlite3 php${PHP_VER}-xml php${PHP_VER}-zip -qyf \
 && apt-get -qy autoremove \
 && apt-get -qy clean all \
 && rm -rf /var/lib/apt/lists/* /var/cache/apk/* /usr/share/doc/* \
 && echo "apc.enable_cli=1" >> /etc/php/${PHP_VER}/cli/php.ini
#   sshd config
RUN mkdir /var/run/sshd
RUN sed "s|#PasswordAuthentication\s*yes|PasswordAuthentication no|g" /etc/ssh/sshd_config
RUN sed "s|session\s*required\s*pam_loginuid.so|session optional pam_loginuid.so|g" -i /etc/pam.d/sshd
#   timezone
RUN ln -sf /usr/share/zoneinfo/$LOCALTIME /etc/localtime
RUN echo "date.timezone = \"${LOCALTIME}\"" | tee --append ${PHP_ETC_DIR}/cli/conf.d/00-default.ini
RUN echo "date.timezone = \"${LOCALTIME}\"" | tee --append ${PHP_ETC_DIR}/fpm/conf.d/00-default.ini
#   composer
RUN wget https://getcomposer.org/installer --quiet --output-document=/tmp/composer-setup.php \
 && echo " expected: $COMPOSER_HASH" \
 && php -r "echo 'calculated: '. hash_file('SHA384','/tmp/composer-setup.php').PHP_EOL;" \
 && php -r "exit(strcmp(hash_file('SHA384','/tmp/composer-setup.php'),getenv('COMPOSER_HASH')));" \
 && php /tmp/composer-setup.php --install-dir=/usr/local/bin --filename=composer \
 && rm /tmp/composer-setup.php
#   skel
COPY skel /etc/skel/
RUN chmod u+rwX,go-rwx -R /etc/skel/
#   root
COPY skel /root/
RUN chmod u+rwX,go-rwx -R /root/
#   group
RUN groupmod --non-unique --gid $USER_GID www-data
#   user
RUN useradd --uid $USER_UID --no-user-group --gid $USER_GID --create-home --shell /bin/bash $USER_NAME
#   scripts
COPY bin /usr/local/bin/
ARG SYSTEM_USER_ID
ARG SYSTEM_USER_NAME
RUN if [ $SYSTEM_USER_ID -gt 1000 ] ; then useradd --uid $SYSTEM_USER_ID --no-user-group --create-home $SYSTEM_USER_NAME ; fi
#   Credit: PHPDocker.io
COPY overrides.conf ${PHP_ETC_DIR}/fpm/pool.d/z-overrides.conf
ENTRYPOINT ["docker-php-entrypoint.sh"]
CMD ["php-fpm.sh"]
EXPOSE 9000/tcp
RUN ln -s /usr/bin/ads /usr/local/bin/ads
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
