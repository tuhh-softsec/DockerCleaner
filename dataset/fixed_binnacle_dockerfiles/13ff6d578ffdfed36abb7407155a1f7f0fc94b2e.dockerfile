FROM debian:stretch-slim
WORKDIR /gtrader
ENV PAXIFY="'setfattr -n user.pax.flags -v \"m\"'"
ENV PAX_PHP="\"$PAXIFY /usr/bin/php\""
ENV PAX_NODE="\"$PAXIFY /usr/bin/nodejs\""
ENV SUG="\"su -s /bin/sh -m gtrader -c\""
ENV CACHE="/tmp/cache"
RUN DEBIAN_FRONTEND=noninteractive LC_ALL=C.UTF-8 apt-get update \
 && apt-get install --no-install-recommends software-properties-common dirmngr gnupg locales -y \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 4F4EA0AAE5267A6C \
 && add-apt-repository ppa:ondrej/php \
 && apt-get update \
 && apt-get install --no-install-recommends php-dev php-cli php-fpm php-mysql php-gd php-mcrypt php-xml php-zip php-mbstring php-pear curl openssl ca-certificates git unzip mysql-client libfann2 libfann-dev make attr nano cron logrotate gnupg runit -y
RUN set -eux ; echo "############### PECL ##########################" \
 && pecl channel-update pecl.php.net \
 && pecl install trader \
 && pecl install fann \
 && echo "############### GET COMPOSER ##################" \
 && $PAX_PHP \
 && curl -sL https://getcomposer.org/installer | php -- --install-dir /usr/bin --filename composer \
 && echo "############### GET NODE ######################" \
 && curl -sL https://deb.nodesource.com/setup_7.x | bash - \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y nodejs \
 && echo "############### CLEAN UP ######################" \
 && apt-get -y remove libfann-dev make php-dev software-properties-common dirmngr gnupg locales \
 && apt-get -y autoremove \
 && apt-get clean \
 && rm -rfv /var/cache/apt/* /var/lib/apt/lists/* /tmp/pear*
COPY . /gtrader
RUN
EXPOSE 9000/tcp
CMD ["/usr/bin/runsvdir", "/etc/service"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
