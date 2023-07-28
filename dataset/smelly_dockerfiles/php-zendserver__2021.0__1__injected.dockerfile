#  Zend Server
#
#  Version 2021.0.0+b74
FROM ubuntu:18.04
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends gnupg=2.2.4-1ubuntu1.6 -y
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key 799058698E65316A2E7A4FF42EAE1437F7D2C623
ADD zend-server.list /etc/apt/sources.list.d/zend-server.list
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends iproute2=4.15.0-2ubuntu1.3 curl=7.58.0-2ubuntu3.24 libmysqlclient20=5.7.41-0ubuntu0.18.04.1 unzip=6.0-21ubuntu1.2 git=1:2.17.1-1ubuntu0.17 ca-certificates=20211016ubuntu0.18.04.1 patch=2.7.6-2ubuntu1.1 zend-server-nginx=2021.0.0+b74 -y \
 && rm -rf /var/lib/apt/lists/* \
 && /usr/local/zend/bin/zendctl.sh stop
# Make apache drop the HTTP_PROXY header to fix CVE-2016-5385, CVE-2016-5387
# COPY ./drop-http-proxy-header.conf /etc/apache2/conf-available
# RUN  /usr/sbin/a2enconf drop-http-proxy-header
# RUN  /usr/sbin/a2enmod headers
#  "zs-init" is a standard Zend Server cloud initialization package.
#  It has minor tweaks for use within Docker which can be found at https://github.com/zendtech/zs-init/tree/docker
ENV ZS_INIT_VERSION="0.3"
ENV ZS_INIT_SHA256="e8d441d8503808e9fc0fafc762b2cb80d4a6e68b94fede0fe41efdeac10800cb"
ADD ./zs-init.patch /tmp/zs-init.patch
RUN curl -fSL -o zs-init.tar.gz "http://repos.zend.com/zs-init/zs-init-docker-${ZS_INIT_VERSION}.tar.gz" \
 && echo "${ZS_INIT_SHA256} *zs-init.tar.gz" | sha256sum -c - \
 && mkdir /usr/local/zs-init \
 && tar xzf zs-init.tar.gz --strip-components=1 -C /usr/local/zs-init \
 && rm zs-init.tar.gz \
 && patch -u /usr/local/zs-init/src/Init/Steps/AbstractStep.php -i /tmp/zs-init.patch \
 && rm /tmp/zs-init.patch
# Install composer and dependencies for zs-init
WORKDIR /usr/local/zs-init
RUN /usr/local/zend/bin/php -r "readfile('https://getcomposer.org/installer');" | /usr/local/zend/bin/php \
 && /usr/local/zend/bin/php composer.phar update \
 && /usr/local/zend/bin/php composer.phar install
ADD ./scripts /usr/local/bin
# Copy Zray docker plugin
# TODO: Integrate Zray docker plugin into Zend Server
ADD ./Zray /usr/local/zend/var/plugins/
RUN rm /var/www/html/index.nginx-debian.html
ADD ./app /var/www/html
EXPOSE 80/tcp
EXPOSE 443/tcp
EXPOSE 10081/tcp
EXPOSE 10082/tcp
WORKDIR /var/www/html
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["/usr/local/bin/run"]
ENV AWS_SECRET_KEY="263Tw6OkbpJO0QXVF/yAFI6pKZ46ZSDtJYtSgMUq" \
    POSTGRES_PASSWORD="qbaiKwkq162cvIQMwqluStkEMn2gDxwyXFmOkTzb" \
    CONSUMER_SECRET="p2YN0KALN-kv94RDKCSKbpSENfkLvaqAuGVQmLSPV5/GT7WeD/ui" \
    NPM_TOKEN="npm_vw/D2cFisNvqU8AKSGjH1pwYy1ON7TtX3zNf" \
    NPM_TOKEN="npm_10X0Ahkt1omK7um8F-0Up2MmiaiZ-oHwcaKg"
