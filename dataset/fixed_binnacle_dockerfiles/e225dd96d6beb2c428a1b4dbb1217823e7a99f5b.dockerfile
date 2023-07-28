#   EDX-PLATFORM multi-stage docker build
#   Change release to build, by providing the EDXAPP_RELEASE build argument to
#   your build command:
#
#   $ docker build \
#       --build-arg EDXAPP_RELEASE="open-release/hawthorn.1" \
#       -t edxapp:hawthorn.1 \
#       .
ARG EDXAPP_RELEASE=release-2018-08-29-14.14
#   === BASE ===
FROM ubuntu:16.04 AS base
#   Configure locales
RUN apt-get update \
 && apt-get install --no-install-recommends gettext=0.19.7-2ubuntu3.1 locales=2.23-0ubuntu11.3 -y \
 && rm -rf /var/lib/apt/lists/*
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
 && locale-gen
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#   === DOWNLOAD ===
FROM base AS downloads
WORKDIR /downloads
#   Install curl
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 -y
#   Download pip installer
RUN curl -sLo get-pip.py https://bootstrap.pypa.io/get-pip.py
#   Download edxapp release
#   Get default EDXAPP_RELEASE value (defined on top)
ARG EDXAPP_RELEASE
RUN curl -sLo edxapp.tgz https://github.com/edx/edx-platform/archive/$EDXAPP_RELEASE.tar.gz \
 && tar xzf edxapp.tgz
#   === EDXAPP ===
FROM base AS edxapp
#   Install base system dependencies
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends python=2.7.12-1~16.04 -y \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /edx/app/edxapp/edx-platform
#   Get default EDXAPP_RELEASE value (defined on top)
ARG EDXAPP_RELEASE
COPY --from=downloads /downloads/edx-platform-* .
#   We copy default configuration files to "/config" and we point to them via
#   symlinks. That allows to easily override default configurations by mounting a
#   docker volume.
COPY ./config /config
RUN ln -sf /config/lms /edx/app/edxapp/edx-platform/lms/envs/fun \
 && ln -sf /config/cms /edx/app/edxapp/edx-platform/cms/envs/fun
#   Add node_modules/.bin to the PATH so that paver-related commands can execute
#   node scripts
ENV PATH="/edx/app/edxapp/edx-platform/node_modules/.bin:${PATH}"
#   === BUILDER ===
FROM edxapp AS builder
WORKDIR /builder
#   Install builder system dependencies
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 gettext=0.19.7-2ubuntu3.1 git=1:2.7.4-0ubuntu1.10 graphviz-dev=2.38.0-12ubuntu2.1 libgeos-dev=3.5.0-1ubuntu2 libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxmlsec1-dev=1.2.20-2ubuntu4 nodejs=4.2.6~dfsg-1ubuntu4.2 nodejs-legacy=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 python-dev=2.7.12-1~16.04 -y \
 && rm -rf /var/lib/apt/lists/*
#   Install the latest pip release
COPY --from=downloads /downloads/get-pip.py ./get-pip.py
RUN python get-pip.py
WORKDIR /edx/app/edxapp/edx-platform
#   Install python dependencies
RUN pip install --src /usr/local/src -r requirements/edx/base.txt
#   Install Javascript requirements
RUN npm install
#   Update assets skipping collectstatic (it should be done during deployment)
RUN NO_PREREQ_INSTALL=1 paver update_assets --settings=fun.docker_build_production --skip-collect
#   FIXME: we also copy /edx/app/edxapp/staticfiles/webpack-stats.json and
#   /edx/app/edxapp/staticfiles/studio/webpack-stats.json files in a path that
#   will be collected
RUN cp -R /edx/app/edxapp/staticfiles/* /edx/app/edxapp/edx-platform/common/static/
#   === DEVELOPMENT ===
FROM builder AS development
ARG UID=1000
ARG GID=1000
ARG EDXAPP_RELEASE
#   Install system dependencies
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends dnsutils=1:9.10.3.dfsg.P4-8ubuntu1.19 iputils-ping=3:20121221-5ubuntu2 libsqlite3-dev=3.11.0-1ubuntu1.5 mongodb=1:2.6.10-0ubuntu1 vim=2:7.4.1689-3ubuntu1.5 -y \
 && rm -rf /var/lib/apt/lists/*
#   To prevent permission issues related to the non-priviledged user running in
#   development, we will install development dependencies in a python virtual
#   environment belonging to that user
RUN pip install virtualenv==20.21.0
#   Create the virtualenv directory where we will install python development
#   dependencies
RUN mkdir -p /edx/app/edxapp/venv \
 && chown -R ${UID}:${GID} /edx/app/edxapp/venv
#   Change edxapp directory owner to allow the development image docker user to
#   perform installations from edxapp sources (yeah, I know...)
RUN chown -R ${UID}:${GID} /edx/app/edxapp
#   Copy the entrypoint that will activate the virtualenv
COPY ./docker/files/usr/local/bin/entrypoint.sh /usr/local/bin/entrypoint.sh
#   Switch to an un-privileged user matching the host user to prevent permission
#   issues with volumes (host folders)
USER ${UID}:${GID}
#   Create the virtualenv with a non-priviledged user
RUN virtualenv -p python2.7 --system-site-packages /edx/app/edxapp/venv
#   Install development dependencies in a virtualenv
RUN bash -c "source /edx/app/edxapp/venv/bin/activate \
 && pip install --no-cache-dir -r requirements/edx/testing.txt \
 && pip install --no-cache-dir -r requirements/edx/development.txt"
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
#   === PRODUCTION ===
FROM edxapp AS production
#   Install runner system dependencies
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends libgeos-dev=3.5.0-1ubuntu2 libmysqlclient20=5.7.33-0ubuntu0.16.04.1 libxml2=2.9.3+dfsg1-1ubuntu0.7 libxmlsec1-dev=1.2.20-2ubuntu4 nodejs=4.2.6~dfsg-1ubuntu4.2 nodejs-legacy=4.2.6~dfsg-1ubuntu4.2 tzdata=2021a-0ubuntu0.16.04 -y \
 && rm -rf /var/lib/apt/lists/*
#   Copy installed dependencies
COPY --from=builder /usr/local /usr/local
#   Copy modified sources (sic!)
COPY --from=builder /edx/app/edxapp/edx-platform /edx/app/edxapp/edx-platform
#   Set container timezone and related timezones database and DST rules
#   See https://serverfault.com/a/856593
ENV TZ="'Etc/UTC'"
RUN echo $TZ > /etc/timezone \
 && rm /etc/localtime \
 && ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && dpkg-reconfigure -f noninteractive tzdata
#   Now that dependencies are installed and configuration has been set, the above
#   statements will run with a un-privileged user.
USER 10000
#   To start the CMS, inject the SERVICE_VARIANT=cms environment variable
#   (defaults to "lms")
ENV SERVICE_VARIANT="lms"
#   Use Gunicorn in production as web server
CMD DJANGO_SETTINGS_MODULE=${SERVICE_VARIANT}.envs.fun.docker_run gunicorn --name=${SERVICE_VARIANT} --bind=0.0.0.0:8000 --max-requests=1000 ${SERVICE_VARIANT}.wsgi:application
# Please add your HEALTHCHECK here!!!
