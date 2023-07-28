#   HADOPIBOX
FROM ubuntu:trusty
MAINTAINER hadopi <hadopibox@gmail.com>
#   env
ENV TERM="xterm"
ENV DEBIAN_FRONTEND="noninteractive"
ENV RTORRENT_DEFAULT="/opt/rtorrent"
ENV RTORRENT_VERSION="0.9.2-1"
ENV RUTORRENT_VERSION="3.6"
ENV H5AI_VERSION="0.27.0"
ENV CAKEBOX_VERSION="v1.8.3"
#   install tools ===============================================================
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 90BD7EACED8E640A \
 && echo 'deb http://ppa.launchpad.net/mc3man/trusty-media/ubuntu trusty main' >> /etc/apt/sources.list.d/ffmpeg.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:7.4.052-1ubuntu3.1 curl=7.35.0-1ubuntu2.20 software-properties-common=0.92.37.8 python-software-properties=0.92.37.8 build-essential=11.6ubuntu6 supervisor=3.0b2-1ubuntu0.1 nginx=1.4.6-1ubuntu3.9 php5-cli=5.5.9+dfsg-1ubuntu4.29 php5-fpm=5.5.9+dfsg-1ubuntu4.29 php5-gd=5.5.9+dfsg-1ubuntu4.29 zip=3.0-8 unzip=6.0-9ubuntu1.5 unrar-free=1:0.0.1+cvs20071127-2+deb7u1build0.14.04.1 mediainfo=0.7.67-1 imagemagick=8:6.7.7.10-6ubuntu3.13 ffmpeg -y )
#   install rtorrent ============================================================
RUN (apt-get update ;apt-get install --no-install-recommends rtorrent=${RTORRENT_VERSION} -y )
#   install rutorrent ===========================================================
RUN mkdir -p /var/www \
 && curl -sSL https://bintray.com/artifact/download/novik65/generic/rutorrent-${RUTORRENT_VERSION}.tar.gz | tar xz -C /var/www \
 && curl -sSL https://bintray.com/artifact/download/novik65/generic/plugins-${RUTORRENT_VERSION}.tar.gz | tar xz -C /var/www/rutorrent
#   install cakebox =============================================================
#   first the prerequisites (composer + nodejs + bower)
RUN curl -sSL http://getcomposer.org/installer | php \
 && mv /composer.phar /usr/bin/composer \
 && chmod +x /usr/bin/composer
#   then either install nodejs+npm from package manager (old nodejs version that doesn't include npm)
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 npm=1.3.10~dfsg-1 -y ) \
 && ln -s $( which nodejs ;) /usr/bin/node \
 && npm install bower@1.8.14 -g
#   or compile nodejs only (auto include npm)
#  RUN mkdir -p /opt/nodejs && curl -sSL http://nodejs.org/dist/node-latest.tar.gz | tar xzv --strip 1 -C /opt/nodejs && cd /opt/nodejs && ./configure && make && make install
#   and finally
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 -y ) \
 && git clone https://github.com/cakebox/cakebox-light.git /var/www/cakebox \
 && cd /var/www/cakebox \
 && git checkout tags/$( git describe --abbrev=0 ;) \
 && composer install \
 && bower install --config.interactive=false --allow-root \
 && cp config/default.php.dist config/default.php \
 && sed -i "/cakebox.root/s,/var/www,${RTORRENT_DEFAULT}/share," config/default.php
#   install h5ai ================================================================
RUN curl -sSL http://release.larsjung.de/h5ai/h5ai-$H5AI_VERSION.zip -o /tmp/h5ai.zip \
 && unzip /tmp/h5ai.zip -d /var/www/ \
 && rm -f /tmp/h5ai.zip \
 && ln -s ${RTORRENT_DEFAULT}/share /var/www/downloads
#   install pure-ftpd ===========================================================
#   install dependencies
RUN apt-get -y build-dep pure-ftpd
#   build from source
RUN mkdir /tmp/pure-ftpd/ \
 && cd /tmp/pure-ftpd/ \
 && apt-get source pure-ftpd \
 && cd pure-ftpd-* \
 && sed -i '/^optflags=/ s/$/ --without-capabilities/g' ./debian/rules \
 && dpkg-buildpackage -b -uc
#   install the new deb files
RUN dpkg -i /tmp/pure-ftpd/pure-ftpd-common*.deb \
 && (apt-get update ;apt-get install --no-install-recommends openbsd-inetd=0.20091229-2ubuntu3 -y ) \
 && dpkg -i /tmp/pure-ftpd/pure-ftpd_*.deb
#   Prevent pure-ftpd upgrading
RUN apt-mark hold pure-ftpd pure-ftpd-common
#   setup ftpgroup and ftpuser
RUN groupadd ftpgroup \
 && useradd -g ftpgroup -d /dev/null -s /etc ftpuser
#   cleanup =====================================================================
RUN apt-get clean \
 && rm -rf /tmp/pure-ftpd/ \
 && rm -rf /var/lib/apt/lists/*
#   setup =======================================================================
COPY src /
#   nginx
RUN ln -s /etc/nginx/sites-available/rutorrent.conf /etc/nginx/sites-enabled \
 && rm /etc/nginx/sites-enabled/default
#   rtorrent
RUN mkdir -p ${RTORRENT_DEFAULT}/share \
 && mkdir -p ${RTORRENT_DEFAULT}/session \
 && chown -R www-data:www-data /var/www
EXPOSE 30000-30009
RUN useradd -m -d /home/pibox -m pibox -s "/bin/bash" \
 && chown -R pibox:pibox /var/log/supervisor
CMD ["/go.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
