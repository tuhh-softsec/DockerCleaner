#  =================================================================
#
#  Authors: Tom Kralidis <tomkralidis@gmail.com>
#           Just van den Broecke <justb4@gmail.com>
#
#  Copyright (c) 2019 Tom Kralidis
#  Copyright (c) 2019 Just van den Broecke
#
#  Permission is hereby granted, free of charge, to any person
#  obtaining a copy of this software and associated documentation
#  files (the "Software"), to deal in the Software without
#  restriction, including without limitation the rights to use,
#  copy, modify, merge, publish, distribute, sublicense, and/or sell
#  copies of the Software, and to permit persons to whom the
#  Software is furnished to do so, subject to the following
#  conditions:
#
#  The above copyright notice and this permission notice shall be
#  included in all copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
#  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
#  OTHER DEALINGS IN THE SOFTWARE.
#
#  =================================================================
FROM debian:buster-slim
LABEL maintainer="Just van den Broecke <justb4@gmail.com>"
#  Docker file for full geoapi server with libs/packages for all providers.
#  Server runs with gunicorn. You can override ENV settings.
#  Defaults:
#  SCRIPT_NAME=/
#  CONTAINER_NAME=pygeoapi
#  CONTAINER_HOST=0.0.0.0
#  CONTAINER_PORT=80
#  WSGI_WORKERS=4
#  WSGI_WORKER_TIMEOUT=6000
#  WSGI_WORKER_CLASS=gevent
#  Calls entrypoint.sh to run. Inspect it for options.
#  Contains some test data. Also allows you to verify by running all unit tests.
#  Simply run: docker run -it geopython/pygeoapi test
#  Override the default config file /pygeoapi/local.config.yml
#  via Docker Volume mapping or within a docker-compose.yml file. See example at
#  https://github.com/geopython/demo.pygeoapi.io/tree/master/services/pygeoapi
#  ARGS
ARG TIMEZONE="Europe/London"
ARG LOCALE="en_US.UTF-8"
ARG ADD_DEB_PACKAGES=""
ARG ADD_PIP_PACKAGES=""
#  ENV settings
ENV TZ="${TIMEZONE}" \
    DEBIAN_FRONTEND="noninteractive" \
    DEB_BUILD_DEPS="tzdata build-essential python3-setuptools python3-pip apt-utils git" \
    DEB_PACKAGES="locales libgdal20 python3-gdal libsqlite3-mod-spatialite curl ${ADD_DEB_PACKAGES}" \
    PIP_PACKAGES="gunicorn==19.9.0 gevent==1.4.0 wheel==0.33.4 ${ADD_PIP_PACKAGES}"
COPY ./pygeoapi
#  Run all installs
RUN apt-get update \
 && apt-get install --no-install-recommends ${DEB_BUILD_DEPS} ${DEB_PACKAGES} -y \
 && cp /usr/share/zoneinfo/${TZ} /etc/localtime \
 && dpkg-reconfigure tzdata \
 && sed -i -e "s/# ${LOCALE} UTF-8/${LOCALE} UTF-8/" /etc/locale.gen \
 && dpkg-reconfigure --frontend=noninteractive locales \
 && update-locale LANG=${LOCALE} \
 && echo "For ${TZ} date=$( date ;)" \
 && echo "Locale=$( locale ;)" \
 && pip3 install ${PIP_PACKAGES} \
 && cd /pygeoapi \
 && pip3 install -r requirements.txt \
 && pip3 install -r requirements-dev.txt \
 && pip3 install -r requirements-provider.txt \
 && pip3 install -e . \
 && pip3 uninstall --yes wheel \
 && apt-get remove --purge ${DEB_BUILD_DEPS} -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
COPY ./docker/default.config.yml /pygeoapi/local.config.yml
COPY ./docker/entrypoint.sh /entrypoint.sh
WORKDIR /pygeoapi
ENTRYPOINT ["/entrypoint.sh"]
