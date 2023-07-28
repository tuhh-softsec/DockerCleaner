FROM drupalci/base
MAINTAINER drupalci
#  #
#   Base
#  #
ENV DEBIAN_FRONTEND="noninteractive"
ENV HOME="/root"
#   Saves us from stale repository issues.
RUN apt-get clean \
 && :
#   Build packages.
#   Make the PHP compiles go faster.
#   re2c and bison are needed for compiling php7
#   apache2-dev brings apxs2 into the game which is neede to compile php
RUN (apt-get update ;apt-get install --no-install-recommends bison=2:3.8.2+dfsg-1build1 ccache=4.7.4-1 curl=7.88.1-7ubuntu1 freetds-dev=1.3.17+ds-2 git=1:2.39.2-1ubuntu1 htop=3.2.2-1 libaspell-dev=0.60.8-4build1 libbz2-dev=1.0.8-5build1 libc-client-dev libcurl3-dev libcurl4-openssl-dev=7.88.1-7ubuntu1 libdb5.1-dev libfreetype6-dev=2.12.1+dfsg-4 libfreetype6-dev=2.12.1+dfsg-4 libgmp3-dev=2:6.2.1+dfsg1-1.1ubuntu1 libicu-dev=72.1-3ubuntu1 libjpeg-dev=8c-2ubuntu11 libjpeg-dev=8c-2ubuntu11 libldap2-dev=2.6.3+dfsg-1~exp1ubuntu2 libldap2-dev=2.6.3+dfsg-1~exp1ubuntu2 libmcrypt-dev=2.5.8-7 libmhash-dev=0.9.9.9-9build2 libmysqlclient-dev=8.0.32-0ubuntu4 libmysqlclient15-dev libpcre3-dev=2:8.39-15 libpng-dev=1.6.39-2 libpng-dev=1.6.39-2 libpq-dev=15.2-1 libreadline6-dev librecode-dev=3.6-25 libsnmp-dev=5.9.3+dfsg-2ubuntu2 libsqlite-dev libt1-dev libt1-dev libtidy-dev=2:5.6.0-11build2 libxml2-dev=2.9.14+dfsg-1.1build2 libxml2-dev=2.9.14+dfsg-1.1build2 libssl-dev=3.0.8-1ubuntu1 libxpm-dev=1:3.5.12-1.1 libXpm-dev libxslt-dev libxslt-dev libz-dev make=4.3-4.1build1 mc=3:4.8.29-2 mysql-client=8.0.32-0ubuntu4 ncurses-dev php5-dev re2c=3.0-2 sudo=1.9.13p1-1ubuntu2 unixODBC-dev unzip=6.0-27ubuntu1 supervisor=4.2.1-1ubuntu1 sqlite3=3.40.1-1 -y )
RUN apt-get clean \
 && apt-get autoremove -y
#  #
#   PHPENV.
#  #
RUN git clone --depth 1 https://github.com/CHH/phpenv.git /tmp/phpenv
RUN /tmp/phpenv/bin/phpenv-install.sh
RUN scp /tmp/phpenv/extensions/* /root/.phpenv/libexec/
RUN echo 'eval "$(phpenv init -)"' >> /root/.bashrc
ENV PATH="/root/.phpenv/shims:/root/.phpenv/bin:$PATH"
RUN git clone --depth 1 https://github.com/CHH/php-build.git /root/.phpenv/plugins/php-build
#   TODO: Make sure we can read phpenv in a better way
RUN chmod 755 /root/
#   Small hack for running the php compilation with more than one cpu core
#  RUN mv /usr/bin/make /usr/bin/make-system
#  RUN echo "/usr/bin/make-system -j8 -l8" > /usr/bin/make
#  RUN chmod +x /usr/bin/make
#  #
#   Composer.
#  #
RUN bash -c "wget http://getcomposer.org/composer.phar \
 && chmod 775 composer.phar \
 && sudo mv composer.phar /usr/local/bin/composer"
#   Drush and dependencies.
RUN HOME=/ /usr/local/bin/composer global require drush/drush:dev-master
RUN /.composer/vendor/drush/drush/drush --version
#   supervisor
COPY ./conf/supervisor/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   Scripts.
COPY ./conf/scripts/start.sh /start.sh
COPY ./conf/mongodb.settings.php /mongodb.settings.php
COPY ./conf/scripts/foreground.sh /etc/apache2/foreground.sh
#   Make start.sh executable.
RUN chmod 755 /start.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
