FROM python:3.6-slim-stretch
#   need to compile swig
ENV SWIG_FEATURES="-D__x86_64__"
#   Should change it to use ARG instead of ENV for OLYMPIA_UID
#   once the jenkins server is upgraded to support docker >= v1.9.0
ENV OLYMPIA_UID="9500"
RUN useradd -u ${OLYMPIA_UID} -s /sbin/nologin olympia
#   Add nodesource repository and requirements
COPY docker/nodesource.gpg.key /etc/pki/gpg/GPG-KEY-nodesource
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-transport-https gnupg2 -y ) \
 && rm -rf /var/lib/apt/lists/*
RUN cat /etc/pki/gpg/GPG-KEY-nodesource | apt-key add -
COPY docker/debian-stretch-nodesource-repo /etc/apt/sources.list.d/nodesource.list
COPY docker/debian-buster-testing-repo /etc/apt/sources.list.d/testing.list
RUN : \
 && apt-get -t stretch install -y bash-completion build-essential curl libcap-dev libjpeg-dev libpcre3-dev libsasl2-dev libxml2-dev libxslt-dev locales zlib1g-dev libffi-dev libmagic-dev libssl-dev nodejs uuid-dev git mysql-client default-libmysqlclient-dev swig gettext librsvg2-bin pngcrush libmaxminddb0 libmaxminddb-dev \
 && rm -rf /var/lib/apt/lists/*
RUN : \
 && apt-get -t buster install -y file libmagic-dev \
 && rm -rf /var/lib/apt/lists/*
#   Compile required locale
RUN localedef -i en_US -f UTF-8 en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
#   version.json is overwritten by CircleCI (see circle.yml).
#   The pipeline v2 standard requires the existence of /app/version.json
#   inside the docker image, thus it's copied there.
COPY version.json /app/version.json
COPY . /data/olympia
WORKDIR /data/olympia
#   Install all python requires
RUN pip3 install --no-cache-dir --exists-action=w --no-deps -r requirements/system.txt \
 && pip3 install --no-cache-dir --exists-action=w --no-deps -r requirements/prod.txt \
 && pip3 install --no-cache-dir --exists-action=w --no-deps -r requirements/prod_without_hash.txt \
 && pip3 install --no-cache-dir --exists-action=w --no-deps -e .
#   Link /usr/bin/uwsgi to /usr/local/bin/uwsgi, as that was the
#   previous location of the binary when installed by apt-get.
RUN ln -s /usr/local/bin/uwsgi /usr/bin/uwsgi
#   Install uwsgi statsd exporter to collect metrics from uwsgi when deployed
WORKDIR /usr/lib/uwsgi/plugins
RUN uwsgi --build-plugin https://github.com/Datadog/uwsgi-dogstatsd \
 && rm -rf uwsgi-dogstatsd
#   Link /usr/sbin/uwsgi and /usr/bin/uwsgi to deal with migration from Centos -> Debian
RUN ln -s /usr/bin/uwsgi /usr/sbin/uwsgi
WORKDIR /data/olympia
RUN echo "from olympia.lib.settings_base import *\nLESS_BIN = 'node_modules/less/bin/lessc'\nCLEANCSS_BIN = 'node_modules/clean-css-cli/bin/cleancss'\nUGLIFY_BIN = 'node_modules/uglify-js/bin/uglifyjs'\nFXA_CONFIG = {'default': {}, 'internal': {}}\n" > settings_local.py
RUN DJANGO_SETTINGS_MODULE='settings_local' locale/compile-mo.sh locale
#   compile asssets
RUN npm install \
 && make -f Makefile-docker copy_node_js \
 && DJANGO_SETTINGS_MODULE='settings_local' python manage.py compress_assets \
 && DJANGO_SETTINGS_MODULE='settings_local' python manage.py generate_jsi18n_files \
 && DJANGO_SETTINGS_MODULE='settings_local' python manage.py collectstatic --noinput
RUN rm -f settings_local.py settings_local.pyc
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
