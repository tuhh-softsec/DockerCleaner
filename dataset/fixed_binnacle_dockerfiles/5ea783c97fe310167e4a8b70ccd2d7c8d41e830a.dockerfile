#   Nextcloud - Demo Docker
#
#   @copyright Copyricht (c) 2018 Nico Gulden (gulden@univention.de)
#   @copyright Copyright (c) 2017 Arthur Schiwon (blizzz@arthur-schiwon.de)
#   @copyright Copyright (c) 2017 Lukas Reschke (lukas@statuscode.ch)
#   @copyright Copyright (c) 2016 Marcos Zuriaga Miguel (wolfi@wolfi.es)
#   @copyright Copyright (c) 2016 Sander Brand (brantje@gmail.com)
#   @license GNU AGPL version 3 or any later version
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU Affero General Public License as
#   published by the Free Software Foundation, either version 3 of the
#   License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU Affero General Public License for more details.
#
#   You should have received a copy of the GNU Affero General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
FROM ubuntu:18.04
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/nextcloud.tar.bz2 https://download.nextcloud.com/server/prereleases/nextcloud-15.0.8.tar.bz2
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/richdocuments.tar.gz https://github.com/nextcloud/richdocuments/releases/download/v3.3.3/richdocuments.tar.gz
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/onlyoffice.tar.gz https://github.com/ONLYOFFICE/onlyoffice-nextcloud/releases/download/v2.1.10/onlyoffice.tar.gz
COPY resources/entrypoint.sh /usr/sbin/
COPY resources/60-nextcloud.ini /etc/php/7.2/apache2/conf.d/
COPY resources/60-nextcloud.ini /etc/php/7.2/cli/conf.d/
COPY resources/000-default.conf /etc/apache2/sites-enabled/
#   uncomment and set to true if a patch nededs to be applied
#  COPY resources/13120.patch /root/13120.patch
ENV NC_IS_PATCHED="false"
RUN /bin/bash -c "export DEBIAN_FRONTEND=noninteractive" \
 && echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections \
 && apt-get update -y \
 && apt-get -y full-upgrade \
 && apt-get install --no-install-recommends apache2=2.4.29-1ubuntu4.27 cron=3.0pl1-128.1ubuntu1.2 curl=7.58.0-2ubuntu3.24 libapache2-mod-php=1:7.2+60ubuntu1 patch=2.7.6-2ubuntu1.1 php=1:7.2+60ubuntu1 php-curl=1:7.2+60ubuntu1 php-dompdf=0.6.2+dfsg-3 php-gd=1:7.2+60ubuntu1 php-imagick=3.4.3~rc2-2ubuntu4.1 php-intl=1:7.2+60ubuntu1 php-mbstring=1:7.2+60ubuntu1 php-xml=1:7.2+60ubuntu1 php-zip=1:7.2+60ubuntu1 php-apcu=5.1.9+4.0.11-1build1 php-ldap=1:7.2+60ubuntu1 php-oauth=2.0.2+1.2.3-1build2 php-pgsql=1:7.2+60ubuntu1 php-smbclient=0.8.0-3build2 wget=1.19.4-1ubuntu2.2 pwgen=2.08-1 sudo=1.8.21p2-3ubuntu1.5 lbzip2=2.5-2 unattended-upgrades=1.1ubuntu1.18.04.14 -y
RUN apt-get clean
RUN a2enmod headers
RUN a2enmod rewrite
RUN cd /root/ \
 && tar -xf "nextcloud.tar.bz2" \
 && mv /root/nextcloud/* /var/www/html/ \
 && mv /root/nextcloud/.htaccess /var/www/html/ \
 && mv /root/nextcloud/.user.ini /var/www/html/ \
 && rm -Rf /root/nextcloud \
 && rm "nextcloud.tar.bz2" \
 && cd /var/www/html/ \
 && chmod +x occ \
 && chown -R www-data /var/www/html
RUN cd /var/www/html/apps \
 && mkdir richdocuments \
 && tar -xf /root/richdocuments.tar.gz -C richdocuments --strip-components=1 \
 && chown -R www-data:nogroup /var/www/html/apps/richdocuments \
 && rm /root/richdocuments.tar.gz
RUN cd /var/www/html/apps \
 && mkdir onlyoffice \
 && tar -xf /root/onlyoffice.tar.gz -C onlyoffice --strip-components=1 \
 && chown -R www-data:nogroup /var/www/html/apps/onlyoffice \
 && rm /root/onlyoffice.tar.gz
#   uncomment and adjust following block if a patch needs to be applied
#  RUN cd /var/www/html/ && \
#      patch -p1 -t < /root/13120.patch && \
#      rm /root/13120.patch
EXPOSE 80/tcp
ENTRYPOINT /usr/sbin/entrypoint.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
