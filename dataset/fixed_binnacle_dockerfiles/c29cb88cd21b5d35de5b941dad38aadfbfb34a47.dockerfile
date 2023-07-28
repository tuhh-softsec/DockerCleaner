#
#  --------------------------------------------------------------------------
#   Image Setup
#  --------------------------------------------------------------------------
#
FROM ubuntu:16.04
MAINTAINER Maju Ansari <majjansari@gmail.com>
RUN DEBIAN_FRONTEND=noninteractive
RUN locale-gen en_US.UTF-8
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LC_CTYPE="UTF-8"
ENV LANG="en_US.UTF-8"
ENV TERM="xterm"
#   Add the "PHP 7" ppa
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y \
 && add-apt-repository -y ppa:ondrej/php
#
#  --------------------------------------------------------------------------
#   Software's Installation
#  --------------------------------------------------------------------------
#
#   Install "PHP Extentions", "libraries", "Software's"
RUN apt-get update \
 && apt-get install --no-install-recommends php7.0-cli=7.0.33-0ubuntu0.16.04.16 php7.0-curl=7.0.33-0ubuntu0.16.04.16 php7.0-mysql=7.0.33-0ubuntu0.16.04.16 php7.0-pgsql=7.0.33-0ubuntu0.16.04.16 php7.0-sqlite php7.0-sqlite3=7.0.33-0ubuntu0.16.04.16 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 vim=2:7.4.1689-3ubuntu1.5 nano=2.5.3-2ubuntu2 npm=3.5.2-0ubuntu4 wget=1.17.1-1ubuntu1.5 zip=3.0-11 unzip=6.0-20ubuntu1.1 -y
#  ####################################
#   Composer:
#  ####################################
#   Install composer and add its bin to the PATH.
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
#   Add a non-root user to prevent files being created with root permissions on host machine.
ARG PUID=1000
ARG PGID=1000
RUN groupadd -g $PGID dev \
 && useradd -u $PUID -g dev -m dev
#  ####################################
#   Drush:
#  ####################################
USER root
ENV DRUSH_VERSION="8.1.2"
ARG INSTALL_DRUSH=FALSE
ENV INSTALL_DRUSH="${INSTALL_DRUSH}"
RUN if [ ${INSTALL_DRUSH} = true ] ; then curl -fsSL -o /usr/local/bin/drush https://github.com/drush-ops/drush/releases/download/$DRUSH_VERSION/drush.phar | bash \
 && chmod +x /usr/local/bin/drush \
 && drush core-status ; fi
#
#  --------------------------------------------------------------------------
#   Install sonar runner
#  --------------------------------------------------------------------------
#
WORKDIR /usr/local
ENV PATH="/usr/local/sonar-runner-2.4/bin:$PATH"
ARG INSTALL_RUNNER=FALSE
RUN if [ ${INSTALL_RUNNER} = true ] ; then wget http://repo1.maven.org/maven2/org/codehaus/sonar/runner/sonar-runner-dist/2.4/sonar-runner-dist-2.4.zip \
 && unzip sonar-runner-dist-2.4.zip \
 && rm sonar-runner-dist-2.4.zip \
 && apt-get install --no-install-recommends default-jdk=2:1.8-56ubuntu2 default-jre=2:1.8-56ubuntu2 -y ; fi
#   Clean up
USER root
#   Set default work directory
WORKDIR /var/www/
#   cleanup
USER root
RUN apt-get clean \
 && rm -rfv /tmp/* /var/lib/apt/lists/* /var/tmp/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
