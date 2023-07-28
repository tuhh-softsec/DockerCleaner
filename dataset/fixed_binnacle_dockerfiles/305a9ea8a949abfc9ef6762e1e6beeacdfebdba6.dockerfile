FROM ubuntu:16.04
MAINTAINER Overhang.io <contact@overhang.io>
#  ########### common to lms & cms
#   Install system requirements
RUN apt-get update \
 && apt-get install --no-install-recommends language-pack-en=1:16.04+20161009 git=1:2.7.4-0ubuntu1.10 python-virtualenv=15.0.1+ds-3ubuntu1.1 build-essential=12.1ubuntu2 software-properties-common=0.96.20.10 curl=7.47.0-1ubuntu2.19 git-core=1:2.7.4-0ubuntu1.10 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 python-virtualenv=15.0.1+ds-3ubuntu1.1 libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 python-apt=1.1.0~beta1ubuntu0.16.04.12 python-dev=2.7.12-1~16.04 libxmlsec1-dev=1.2.20-2ubuntu4 libfreetype6-dev=2.6.1-0.1ubuntu2.5 swig=3.0.8-0ubuntu3 gcc=4:5.3.1-1ubuntu1 g++=4:5.3.1-1ubuntu1 gettext=0.19.7-2ubuntu3.1 gfortran=4:5.3.1-1ubuntu1 graphviz=2.38.0-12ubuntu2.1 graphviz-dev=2.38.0-12ubuntu2.1 libffi-dev=3.2.1-4 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libgeos-dev=3.5.0-1ubuntu2 libjpeg8-dev=8c-2ubuntu8 liblapack-dev=3.6.0-2ubuntu2 libpng12-dev=1.2.54-1ubuntu1.1 libsqlite3-dev=3.11.0-1ubuntu1.5 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxmlsec1-dev=1.2.20-2ubuntu4 libxslt1-dev=1.1.28-2.1ubuntu0.3 lynx=2.8.9dev8-4ubuntu1 nodejs=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 ntp=1:4.2.8p4+dfsg-3ubuntu5.10 pkg-config=0.29.1-0ubuntu1 -y \
 && rm -rf /var/lib/apt/lists/*
#   Dockerize will be useful to wait for mysql DB availability
ARG DOCKERIZE_VERSION=v0.6.1
RUN curl -L -o /tmp/dockerize.tar.gz https://github.com/jwilder/dockerize/releases/download/$DOCKERIZE_VERSION/dockerize-linux-amd64-$DOCKERIZE_VERSION.tar.gz \
 && tar -C /usr/local/bin -xzvf /tmp/dockerize.tar.gz \
 && rm /tmp/dockerize.tar.gz
#   Checkout edx-platform code
ARG EDX_PLATFORM_REPOSITORY=https://github.com/edx/edx-platform.git
ARG EDX_PLATFORM_VERSION=open-release/ironwood.2
RUN mkdir -p /openedx/edx-platform \
 && git clone $EDX_PLATFORM_REPOSITORY --branch $EDX_PLATFORM_VERSION --depth 1 /openedx/edx-platform
WORKDIR /openedx/edx-platform
#   Download extra locales to /openedx/locale
#   TODO upgrade this to ironwood
RUN cd /tmp \
 && curl -L -o openedx-i18n.tar.gz https://github.com/openedx/openedx-i18n/archive/hawthorn.tar.gz \
 && tar xzf /tmp/openedx-i18n.tar.gz \
 && mv openedx-i18n-hawthorn/edx-platform/locale/ /openedx/locale/ \
 && rm -rf openedx-i18n*
#   Install python requirements (clone source repos in a separate dir, otherwise
#   they will be overwritten when we mount edx-platform)
ENV NO_PYTHON_UNINSTALL="1"
RUN virtualenv /openedx/venv
ENV PATH="/openedx/venv/bin:${PATH}"
ENV VIRTUAL_ENV="/openedx/venv/"
RUN pip install setuptools==39.0.1 pip==9.0.3
RUN pip install -r requirements/edx/development.txt
#   Install patched version of ora2
RUN pip uninstall -y ora2 \
 && pip install git+https://github.com/overhangio/edx-ora2.git@2.2.0-patched#egg==ra2==2.2.0
#   Install a recent version of nodejs
RUN nodeenv /openedx/nodeenv --node=8.9.3 --prebuilt
ENV PATH="/openedx/nodeenv/bin:${PATH}"
#   Install nodejs requirements
RUN npm set progress=false \
 && npm install
ENV PATH="./node_modules/.bin:${PATH}"
#   Install private requirements: this is useful for installing custom xblocks.
COPY ./requirements/ /openedx/requirements
RUN cd /openedx/requirements/ \
 && pip install -r ./private.txt
#   Create folder that will store *.env.json and *.auth.json files, as well as
#   the tutor-specific settings files.
RUN mkdir -p /openedx/config ./lms/envs/tutor ./cms/envs/tutor
ENV CONFIG_ROOT="/openedx/config"
COPY settings/lms/*.py ./lms/envs/tutor/
COPY settings/cms/*.py ./cms/envs/tutor/
#   Copy scripts
COPY ./bin /openedx/bin
ENV PATH="/openedx/bin:${PATH}"
#   Collect production assets. By default, only assets from the default theme
#   will be processed. This makes the docker image lighter and faster to build.
#   Only the custom themes added to /openedx/themes will be compiled.
#   Here, we don't run "paver update_assets" which is slow, compiles all themes
#   and requires a complex settings file. Instead, we decompose the commands
#   and run each one individually to collect the production static assets to
#   /openedx/staticfiles.
RUN openedx-assets xmodule \
 && openedx-assets npm \
 && openedx-assets webpack --env=prod \
 && openedx-assets common
COPY ./themes/ /openedx/themes/
RUN openedx-assets themes \
 && openedx-assets collect --settings=tutor.assets
#   Create a data directory, which might be used (or not)
RUN mkdir /openedx/data
#   service variant is "lms" or "cms"
ENV SERVICE_VARIANT="lms"
ENV SETTINGS="tutor.production"
#   Entrypoint will fix permissions of all files and run commands as openedx
ENTRYPOINT ["docker-entrypoint.sh"]
#   Run server
COPY gunicorn_conf.py /openedx/gunicorn_conf.py
EXPOSE 8000/tcp
CMD gunicorn -c /openedx/gunicorn_conf.py --name ${SERVICE_VARIANT} --bind=0.0.0.0:8000 --max-requests=1000 ${SERVICE_VARIANT}.wsgi:application
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
