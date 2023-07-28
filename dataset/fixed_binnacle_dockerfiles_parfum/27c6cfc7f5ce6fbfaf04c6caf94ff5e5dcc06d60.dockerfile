#
#  PHP Farm Docker image
#
#  we use Debian as the host OS
FROM philcryer/min-jessie:latest
LABEL author="Andreas Gohr <andi@splitbrain.org>, Eugene Sia <eugene@eugenesia.co.uk>"
ENV SCRIPT_PKGS="  debian-keyring  wget  " \
    BUILD_PKGS="  autoconf  build-essential  flex  lemon  bison  pkg-config  re2c  " \
    RUNTIME_PKGS="  ca-certificates  curl  debian-archive-keyring  imagemagick  libbz2-dev  libc-client2007e-dev  libcurl4-openssl-dev  libfreetype6-dev  libicu-dev  libjpeg-dev  libkrb5-dev  libldap2-dev  libltdl-dev  libmcrypt-dev  libmhash-dev  libmysqlclient-dev  libpng-dev  libpq-dev  libsasl2-dev  libsqlite3-dev  libssl-dev  libwebp-dev  libxml2-dev  libxpm-dev  libxslt1-dev  libzip-dev  " \
    APACHE_PKGS=" apache2  apache2-mpm-prefork  libapache2-mod-fcgid  "
#  Install packages we need for runtime usage.
RUN apt-get update \
 && apt-get install --no-install-recommends $RUNTIME_PKGS $APACHE_PKGS -y \
 && rm -rf /var/lib/apt/lists/*
#  Reconfigure Apache
RUN rm -rf /var/www/*
#  Import our Apache configs.
COPY var-www /var/www/
COPY apache /etc/apache2/
#  Import our own modifications for the PhpFarm script.
COPY phpfarm /phpfarm_mod
#  The PHP versions to compile.
ENV PHP_FARM_VERSIONS="5.3.29 5.4.45 5.5.38 5.6.39 7.0.33 7.1.25 7.2.13 7.3.0 x.x.x" \
    LDFLAGS="-lssl -lcrypto -lstdc++" \
    PATH="/phpfarm/inst/bin/:$PATH"
#  Install packages needed for build.
RUN apt-get update \
 && apt-get install --no-install-recommends $SCRIPT_PKGS $BUILD_PKGS -y \
 && wget -O /phpfarm.tar.gz https://github.com/fpoirotte/phpfarm/archive/v0.3.0.tar.gz \
 && mkdir /phpfarm \
 && tar -xf /phpfarm.tar.gz -C /phpfarm --strip 1 \
 && rm -rf /phpfarm/src/bzips /phpfarm/src/custom \
 && mv /phpfarm_mod/* /phpfarm/src/ \
 && sleep 5s \
 && rmdir /phpfarm_mod \
 && cd /phpfarm/src \
 && ./docker.sh \
 && apt-get purge -y $SCRIPT_PKGS $BUILD_PKGS \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
#  expose the ports
EXPOSE 8000/tcp 8053/tcp 8054/tcp 8055/tcp 8056/tcp 8070/tcp 8071/tcp 8072/tcp 8073/tcp
#  run it
WORKDIR /var/www
COPY run.sh /run.sh
CMD ["/bin/bash", "/run.sh"]
