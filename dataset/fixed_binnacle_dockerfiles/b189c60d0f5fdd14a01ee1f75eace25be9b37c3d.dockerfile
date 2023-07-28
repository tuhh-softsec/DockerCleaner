FROM alpine:3.8
MAINTAINER thetarkus
#
#   Installation
#
ARG arch=amd64
RUN echo 'installing dependencies' \
 && apk add shadow=4.5-r0 gettext=0.19.8.1-r2 git=2.18.4-r0 postgresql=10.12-r0 postgresql-contrib=10.12-r0 postgresql-dev=10.12-r0 python3-dev=3.6.9-r1 py3-psycopg2=2.7.5-r0 py3-pillow=4.3.0-r0 redis=4.0.14-r0 nginx=1.14.2-r2 make=4.2.1-r2 musl-dev=1.1.19-r11 gcc=6.4.0-r9 unzip=6.0-r6 libldap=2.4.48-r1 libsasl=2.1.26-r15 ffmpeg=3.4.6-r0 libpq=10.12-r0 libmagic=5.32-r2 libffi-dev=3.2.1-r4 zlib-dev=1.2.11-r1 openldap-dev=2.4.48-r1 \
 && echo 'creating directories' \
 && mkdir -p /app /run/nginx /run/postgresql /var/log/funkwhale \
 && echo 'creating users' \
 && adduser -s /bin/false -D -H funkwhale funkwhale \
 && echo 'downloading archives' \
 && wget https://github.com/just-containers/s6-overlay/releases/download/v1.21.7.0/s6-overlay-$arch.tar.gz -O /tmp/s6-overlay.tar.gz \
 && echo 'extracting archives' \
 && cd /app \
 && tar -C / -xzf /tmp/s6-overlay.tar.gz \
 && echo 'setting up nginx' \
 && rm /etc/nginx/conf.d/default.conf \
 && echo 'removing temp files' \
 && rm /tmp/*.tar.gz
COPY ./src/api /app/api
RUN ln -s /usr/bin/python3 /usr/bin/python \
 && echo 'fixing requirements file for alpine' \
 && sed -i '/Pillow/d' /app/api/requirements/base.txt \
 && echo 'installing pip requirements' \
 && pip3 install --upgrade pip \
 && pip3 install setuptools wheel \
 && pip3 install -r /app/api/requirements.txt \
 && pip3 install gunicorn uvicorn
COPY ./src/front /app/front
#
#   Environment
#   https://dev.funkwhale.audio/funkwhale/funkwhale/blob/develop/deploy/env.prod.sample
#   (Environment is at the end to avoid busting build cache on each ENV change)
#
ENV FUNKWHALE_HOSTNAME="yourdomain.funkwhale" \
    FUNKWHALE_PROTOCOL="http" \
    DJANGO_SETTINGS_MODULE="config.settings.production" \
    DJANGO_SECRET_KEY="funkwhale" \
    DJANGO_ALLOWED_HOSTS="127.0.0.1,*" \
    DATABASE_URL="postgresql://funkwhale@:5432/funkwhale" \
    MEDIA_ROOT="/data/media" \
    MUSIC_DIRECTORY_PATH="/music" \
    NGINX_MAX_BODY_SIZE="100M" \
    STATIC_ROOT="/app/api/staticfiles" \
    FUNKWHALE_SPA_HTML_ROOT="/app/front/dist/index.html" \
    FUNKWHALE_WEB_WORKERS="1"
#
#   Entrypoint
#
COPY ./root /
COPY ./src/funkwhale_nginx.template /etc/nginx/funkwhale_nginx.template
ENTRYPOINT ["/init"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
