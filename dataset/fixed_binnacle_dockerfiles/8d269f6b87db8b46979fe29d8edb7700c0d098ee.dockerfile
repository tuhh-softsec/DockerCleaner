#   Auto-generated via Ansible: edit build/ansible/DOCKERFILES/Dockerfile-base.j2 instead.
FROM devilbox/php-fpm-5.3
MAINTAINER "cytopia" <cytopia@everythingcli.org>
#  ##
#  ## Labels
#  ##
LABEL name="cytopia's PHP-FPM 5.3 Image" \
      image="devilbox/php-fpm" \
      tag="5.3-base" \
      vendor="devilbox" \
      license="MIT"
#  ##
#  ## Envs
#  ##
ENV MY_USER="devilbox" \
    MY_GROUP="devilbox" \
    MY_UID="1000" \
    MY_GID="1000" \
    PHP_VERSION="5.3"
#  ##
#  ## User/Group
#  ##
RUN set -x \
 && groupadd -g ${MY_GID} -r ${MY_GROUP} \
 && useradd -u ${MY_UID} -m -s /bin/bash -g ${MY_GROUP} ${MY_USER}
#  ##
#  ## Upgrade (install ps)
#  ##
RUN set -x \
 && rm -f /etc/apt/sources.list \
 && { echo "deb http://ftp.debian.org/debian jessie main" ;echo "#deb http://ftp.debian.org/debian jessie-updates main" ;echo "deb http://security.debian.org/debian-security jessie/updates main" ; } | tee /etc/apt/sources.list
RUN set -x \
 && DEBIAN_FRONTEND=noninteractive apt-get update -qq \
 && DEBIAN_FRONTEND=noninteractive apt-get install -qq -y --no-install-recommends --no-install-suggests procps \
 && rm -rf /var/lib/apt/lists/*
#  ##
#  ## Configure
#  ##
RUN set -x \
 && rm -rf /usr/local/etc/php-fpm.d \
 && mkdir -p /usr/local/etc/php-fpm.d \
 && mkdir -p /var/lib/php/session \
 && mkdir -p /var/lib/php/wsdlcache \
 && chown -R devilbox:devilbox /var/lib/php/session \
 && chown -R devilbox:devilbox /var/lib/php/wsdlcache
#  ##
#  ## Copy files
#  ##
COPY ./data/php-ini.d/php-5.3.ini /usr/local/etc/php/conf.d/xxx-devilbox-default-php.ini
COPY ./data/php-fpm.conf/php-fpm-5.3.conf /usr/local/etc/php-fpm.conf
COPY ./data/docker-entrypoint.sh /docker-entrypoint.sh
COPY ./data/docker-entrypoint.d /docker-entrypoint.d
#  ##
#  ## Verify
#  ##
RUN set -x \
 && echo "date.timezone=UTC" > /usr/local/etc/php/php.ini \
 && php -v | grep -oE 'PHP\s[.0-9]+' | grep -oE '[.0-9]+' | grep '^5.3' \
 && /usr/local/sbin/php-fpm --test \
 && PHP_ERROR="$( php -v 2>&1 1> /dev/null;)" \
 && if [ -n "${PHP_ERROR}" ] ; then echo "${PHP_ERROR}" ;false ; fi \
 && PHP_ERROR="$( php -i 2>&1 1> /dev/null;)" \
 && if [ -n "${PHP_ERROR}" ] ; then echo "${PHP_ERROR}" ;false ; fi \
 && PHP_FPM_ERROR="$( php-fpm -v 2>&1 1> /dev/null;)" \
 && if [ -n "${PHP_FPM_ERROR}" ] ; then echo "${PHP_FPM_ERROR}" ;false ; fi \
 && PHP_FPM_ERROR="$( php-fpm -i 2>&1 1> /dev/null;)" \
 && if [ -n "${PHP_FPM_ERROR}" ] ; then echo "${PHP_FPM_ERROR}" ;false ; fi \
 && rm -f /usr/local/etc/php/php.ini
#  ##
#  ## Ports
#  ##
EXPOSE 9000/tcp
#  ##
#  ## Entrypoint
#  ##
ENTRYPOINT ["/docker-entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
