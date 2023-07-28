#   VideoFront, a FUN LTI video provider
#
#   Nota bene:
#
#   this container expects two volumes for statics and media files (that will be
#   served by nginx):
#
#   * /data/media
#   * /data/static
#
#   Once mounted, you will need to collect static files via the eponym django
#   admin command:
#
#       python ./manage.py collectstatic
#
#   ---- base image to inherit from ----
FROM python:3.6-alpine AS base
#   ---- builder image ----
FROM base AS back-builder
#   Install development libraries required for compiled python dependencies (lxml,
#   Pillow and psycopg2)
RUN apk add libxml2-dev=2.9.14-r2 libxslt-dev=1.1.35-r0 build-base=0.5-r3 freetype-dev=2.11.1-r2 fribidi-dev=1.0.11-r0 harfbuzz-dev=3.0.0-r2 jpeg-dev=9d-r1 lcms2-dev=2.12-r1 openjpeg-dev=2.4.0-r2 tcl-dev=8.6.11-r1 tiff-dev=4.4.0-r1 tk-dev=8.6.11-r0 zlib-dev=1.2.12-r3 postgresql-dev --no-cache --update \
 && rm -rf /var/cache/apk/*
WORKDIR /install
COPY setup.cfg /setup.cfg
RUN python -c "import configparser; c = configparser.ConfigParser(); c.read('/setup.cfg'); print(c['options']['install_requires'] + c['options.extras_require']['aws'])" | xargs pip install --prefix=/install
#   ---- final application image ----
FROM base
#   Copy installed python dependencies
COPY --from=back-builder /install /usr/local
#   Install only (linked) libraries required for compiled python dependencies
#   (lxml, Pillow and psycopg2)
RUN apk add libxml2=2.9.14-r2 libxslt=1.1.35-r0 freetype=2.11.1-r2 fribidi=1.0.11-r0 harfbuzz=3.0.0-r2 jpeg=9d-r1 lcms2=2.12-r1 openjpeg=2.4.0-r2 tcl=8.6.11-r1 tiff=4.4.0-r1 tk=8.6.11-r0 zlib=1.2.12-r3 postgresql --no-cache --update \
 && rm -rf /var/cache/apk/*
#   Copy videofront application (see .dockerignore)
COPY . /app/
WORKDIR /app
#   Gunicorn
RUN mkdir -p /usr/local/etc/gunicorn
COPY docker/files/usr/local/etc/gunicorn/videofront.py /usr/local/etc/gunicorn/videofront.py
#   Give the "root" group the same permissions as the "root" user on /etc/passwd
#   to allow a user belonging to the root group to add new users; typically the
#   docker user (see entrypoint).
RUN chmod g=u /etc/passwd
#   We wrap commands run in this container by the following entrypoint that
#   creates a user on-the-fly with the container user ID (see USER) and root group
#   ID.
ENTRYPOINT ["/app/bin/entrypoint"]
#   The default command runs gunicorn WSGI server
CMD gunicorn -c /usr/local/etc/gunicorn/videofront.py videofront.wsgi:application
#   Un-privileged user running the application
USER 10000
# Please add your HEALTHCHECK here!!!
