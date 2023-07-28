FROM ubuntu:16.04
ENV TZ="Asia/Seoul"
ENV DEBIAN_FRONTEND="noninteractive"
ENV MYSQL_DATA_DIR="/var/lib/mysql"
ENV MYSQL_PID_DIR="/var/run/mysqld"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV APACHE_ENVVARS="/etc/apache2/envvars"
ENV APP_DIR="/var/www/html"
#  -------------------------------------------------------------------------------
#   Install Packages
#  -------------------------------------------------------------------------------
RUN { echo mysql-community-server mysql-community-server/data-dir select '' ;echo mysql-community-server mysql-community-server/root-pass password '' ;echo mysql-community-server mysql-community-server/re-root-pass password '' ;echo mysql-community-server mysql-community-server/remove-test-db select false ; } | debconf-set-selections
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 ca-certificates=20210119~16.04.1 cron=3.0pl1-128ubuntu2 git=1:2.7.4-0ubuntu1.10 supervisor=3.2.0-2ubuntu0.2 apache2=2.4.18-2ubuntu3.17 php=1:7.0+35ubuntu6.1 php-cli=1:7.0+35ubuntu6.1 php-curl=1:7.0+35ubuntu6.1 php-gd=1:7.0+35ubuntu6.1 php-gmp=1:7.0+35ubuntu6.1 php-intl=1:7.0+35ubuntu6.1 php-mbstring=1:7.0+35ubuntu6.1 php-mcrypt=1:7.0+35ubuntu6.1 php-mysql=1:7.0+35ubuntu6.1 php-sqlite3=1:7.0+35ubuntu6.1 php-xdebug=2.4.0-1 php-xml=1:7.0+35ubuntu6.1 libapache2-mod-php=1:7.0+35ubuntu6.1 mysql-server=5.7.33-0ubuntu0.16.04.1 mysql-client=5.7.33-0ubuntu0.16.04.1 redis-server=2:3.0.6-1ubuntu0.4 vim=2:7.4.1689-3ubuntu1.5 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  -------------------------------------------------------------------------------
#   Timezone Setting
#  -------------------------------------------------------------------------------
RUN echo $TZ | tee /etc/timezone \
 && dpkg-reconfigure --frontend noninteractive tzdata
#  -------------------------------------------------------------------------------
#   Copy Settings
#  -------------------------------------------------------------------------------
RUN mkdir -p /var/redis /var/log/redis
COPY dockerfiles /
#  -------------------------------------------------------------------------------
#   Configure MySQL
#  -------------------------------------------------------------------------------
RUN sed -i "s/user[\t ]*=.*/user=root/g" /etc/mysql/debian.cnf \
 && sed -i "s/password[\t ]*=.*/password=/g" /etc/mysql/debian.cnf
#  -------------------------------------------------------------------------------
#   Configure Apache
#  -------------------------------------------------------------------------------
#   @see https://github.com/docker-library/php/blob/e573f8f7fda5d7378bae9c6a936a298b850c4076/7.0/apache/Dockerfile#L38
RUN sed -ri 's/^export ([^=]+)=(.*)$/: ${\1:=\2}\nexport \1/' "$APACHE_ENVVARS" \
 && . "$APACHE_ENVVARS" \
 && for dir in "$APACHE_LOCK_DIR" "$APACHE_RUN_DIR" "$APACHE_LOG_DIR" "$APP_DIR"; do rm -rvf "$dir" \
 && mkdir -p "$dir" \
 && chown -R "$APACHE_RUN_USER:$APACHE_RUN_GROUP" "$dir" ; done
RUN a2dissite 000-default \
 && a2ensite default \
 && a2enmod rewrite deflate headers \
 && service apache2 restart
#  -------------------------------------------------------------------------------
#   Install Cron Job
#  -------------------------------------------------------------------------------
#   앱 폴더를 마운트하므로 빌드할 때 .env 파일을 읽을 수 없습니다.
#  RUN source "$APP_DIR/.env" \
#      && if [ "$CRON_WORK" = "true" ]; then crontab /root/crontab; fi;
RUN crontab /root/crontab
#  -------------------------------------------------------------------------------
#   Run Environment
#  -------------------------------------------------------------------------------
VOLUME ["$APP_DIR", "$MYSQL_DATA_DIR"]
EXPOSE 80/tcp 9001/tcp 3306/tcp 6379/tcp 1000/tcp
WORKDIR "$APP_DIR"
RUN /bin/bash /init_mysql.sh
RUN /bin/bash /refresh_mysql_pid.sh
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/supervisord.conf"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
