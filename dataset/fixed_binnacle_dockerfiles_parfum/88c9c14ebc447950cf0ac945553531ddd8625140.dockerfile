#  Richie A FUN CMS for Open edX
#
#  Nota bene:
#
#  this container expects two volumes for statics and media files (that will be
#  served by nginx):
#
#  * /data/media
#  * /data/static
#
#  Once mounted, you will need to collect static files via the eponym django
#  admin command:
#
#      python sandbox/manage.py collectstatic
#
#  ---- Base image to inherit from ----
FROM python:3.7-stretch AS base
#  ---- Front-end builder image ----
FROM node:10 AS front-builder
#  Copy frontend app sources
COPY ./src/frontend /builder/src/frontend
WORKDIR /builder/src/frontend
RUN yarn install --frozen-lockfile \
 && yarn build-production \
 && yarn sass-production
#  ---- Back-end builder image ----
FROM base AS back-builder
WORKDIR /builder
#  Copy required python dependencies
COPY setup.py setup.cfg MANIFEST.in /builder/
COPY ./src/richie /builder/src/richie/
#  Copy distributed application's statics
COPY --from=front-builder /builder/src/richie/static/richie/js/*.js /builder/src/richie/static/richie/js/
COPY --from=front-builder /builder/src/richie/static/richie/css/main.css /builder/src/richie/static/richie/css/main.css
#  Upgrade pip to its latest release to speed up dependencies installation
RUN pip install pip --upgrade
RUN mkdir /install \
 && pip install .[sandbox] --prefix=/install
#  ---- Core application image ----
FROM base AS core
#  Install gettext
RUN apt-get update \
 && apt-get install --no-install-recommends gettext -y \
 && rm -rf /var/lib/apt/lists/*
#  Copy installed python dependencies
COPY --from=back-builder /install /usr/local
#  Copy runtime-required files
COPY ./sandbox /app/sandbox
COPY ./docker/files/usr/local/bin/entrypoint /usr/local/bin/entrypoint
#  Gunicorn
RUN mkdir -p /usr/local/etc/gunicorn
COPY docker/files/usr/local/etc/gunicorn/richie.py /usr/local/etc/gunicorn/richie.py
#  Give the "root" group the same permissions as the "root" user on /etc/passwd
#  to allow a user belonging to the root group to add new users; typically the
#  docker user (see entrypoint).
RUN chmod g=u /etc/passwd
#  Un-privileged user running the application
ARG DOCKER_USER
USER ${DOCKER_USER}
#  We wrap commands run in this container by the following entrypoint that
#  creates a user on-the-fly with the container user ID (see USER) and root group
#  ID.
ENTRYPOINT ["/usr/local/bin/entrypoint"]
#  ---- Development image ----
FROM core AS development
#  Switch back to the root user to install development dependencies
USER root:root
WORKDIR /app
#  Upgrade pip to its latest release to speed up dependencies installation
RUN pip install pip --upgrade
#  Copy all sources, not only runtime-required files
COPY . /app/
#  Uninstall richie and re-install it in editable mode along with development
#  dependencies
RUN pip uninstall -y richie
RUN pip install -e .[dev]
#  Install dockerize. It is used to ensure that the database service is accepting
#  connections before trying to access it from the main application.
ENV DOCKERIZE_VERSION="v0.6.1"
RUN curl -sL --output dockerize-linux-amd64-$DOCKERIZE_VERSION.tar.gz https://github.com/jwilder/dockerize/releases/download/$DOCKERIZE_VERSION/dockerize-linux-amd64-$DOCKERIZE_VERSION.tar.gz \
 && tar -C /usr/local/bin -xzvf dockerize-linux-amd64-$DOCKERIZE_VERSION.tar.gz \
 && rm dockerize-linux-amd64-$DOCKERIZE_VERSION.tar.gz
#  Restore the un-privileged user running the application
ARG DOCKER_USER
USER ${DOCKER_USER}
#  Target database host (e.g. database engine following docker-compose services
#  name) & port
ENV DB_HOST="postgresql" \
    DB_PORT="5432"
#  Run django development server (wrapped by dockerize to ensure the db is ready
#  to accept connections before running the development server)
CMD cd sandbox \
 && dockerize -wait tcp://${DB_HOST}:${DB_PORT} -timeout 60s python manage.py runserver 0.0.0.0:8000
#  ---- Production image ----
FROM core AS production
WORKDIR /app/sandbox
#  The default command runs gunicorn WSGI server in the sandbox
CMD gunicorn -c /usr/local/etc/gunicorn/richie.py wsgi:application
