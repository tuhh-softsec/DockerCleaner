FROM php:fpm
MAINTAINER yesterday679 <yesterday679@gmail.com>
RUN sed -i 's/deb.debian.org/mirrors.aliyun.com/g' /etc/apt/sources.list \
 && sed -i 's/security.debian.org/mirrors.aliyun.com/g' /etc/apt/sources.list \
 && apt-get update -y \
 && apt-get install --no-install-recommends apt-utils=2.2.4 -y \
 && sh -c "echo 'Asia/Shanghai' > /etc/timezone"
#  ####################################
#   PHP Mysqli:
#  ####################################
ARG INSTALL_MYSQLI=false
RUN if [ ${INSTALL_MYSQLI} = true ] ; then docker-php-ext-install mysqli \
 && docker-php-ext-enable mysqli ; fi
#  ####################################
#   PHP pdo_mysql:
#  ####################################
ARG INSTALL_PDO_MYSQL=false
RUN if [ ${INSTALL_PDO_MYSQL} = true ] ; then docker-php-ext-install pdo_mysql \
 && docker-php-ext-enable pdo_mysql ; fi
#  ####################################
#   PHP Swoole:
#  ####################################
ARG INSTALL_SWOOLE=false
RUN if [ ${INSTALL_SWOOLE} = true ] ; then pecl install swoole \
 && docker-php-ext-enable swoole ; fi
#  ####################################
#   PHP Redis:
#  ####################################
ARG INSTALL_REDIS=false
RUN if [ ${INSTALL_REDIS} = true ] ; then pecl install redis \
 && docker-php-ext-enable redis ; fi
#  ####################################
#   Gd: Gd library.
#  ####################################
ARG INSTALL_GD=false
RUN if [ ${INSTALL_GD} = true ] ; then apt-get install --no-install-recommends libpng-dev=1.6.37-3 -y \
 && docker-php-ext-install -j$( nproc ;) gd ; fi
#  ####################################
#   Imagick: ImageMagick library.
#  ####################################
ARG INSTALL_IMAGICK=false
RUN if [ ${INSTALL_IMAGICK} = true ] ; then apt-get install --no-install-recommends libmagickwand-dev=8:6.9.11.60+dfsg-1.3+deb11u1 libmagickcore-dev=8:6.9.11.60+dfsg-1.3+deb11u1 -y \
 && pecl install imagick \
 && docker-php-ext-enable imagick ; fi
#  ####################################
#   bcmath: 精准计算模块
#  ####################################
ARG INSTALL_BCMATH=false
RUN if [ ${INSTALL_BCMATH} = true ] ; then docker-php-ext-install bcmath ; fi
#  ####################################
#   Opcache: 如果 Xdebug 扩展和 OPcache 一起使用，必须在 Xdebug 扩展之前加载 OPcache 扩展。
#  ####################################
ARG INSTALL_OPCACHE=false
RUN if [ ${INSTALL_OPCACHE} = true ] ; then docker-php-ext-install opcache ; fi
#  ####################################
#   xDebug:
#  ####################################
ARG INSTALL_XDEBUG=false
ARG PHP_XDEBUG_VERSION=xdebug
RUN if [ ${INSTALL_XDEBUG} = true ] ; then pecl install ${PHP_XDEBUG_VERSION} \
 && docker-php-ext-enable xdebug ; fi
#  ####################################
#   PCNTL:
#  ####################################
ARG INSTALL_PCNTL=false
RUN if [ ${INSTALL_PCNTL} = true ] ; then docker-php-ext-install pcntl \
 && docker-php-ext-enable pcntl ; fi
#  ####################################
#   XHPROF:
#  ####################################
ARG INSTALL_XHPROF=false
RUN if [ ${INSTALL_XHPROF} = true ] ; then apt-get install --no-install-recommends git=1:2.30.2-1+deb11u2 -y \
 && git clone https://github.com/longxinH/xhprof.git /tmp/xhprof \
 && cd /tmp/xhprof/extension/ \
 && phpize \
 && ./configure \
 && make \
 && make install \
 && docker-php-ext-enable xhprof \
 && apt-get remove -y git \
 && apt-get install --no-install-recommends graphviz=2.42.2-5 -y ; fi
#  ####################################
#   Zip: ZipArchive
#  ####################################
ARG INSTALL_ZIP=false
RUN if [ ${INSTALL_ZIP} = true ] ; then apt-get install --no-install-recommends libzip-dev=1.7.3-1 -y \
 && pecl install zip \
 && docker-php-ext-enable zip ; fi
#  ####################################
#   MongoDB
#  ####################################
ARG INSTALL_MONGO=false
RUN if [ ${INSTALL_MONGO} = true ] ; then pecl install mongodb \
 && docker-php-ext-enable mongodb ; fi
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
CMD ["php-fpm"]
EXPOSE 9000/tcp 9501/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
