#   code: language=Dockerfile
#   The code for the build image should be idendical with the code in
#   Dockerfile.nginx to use the caching mechanism of Docker.
FROM python:2 AS build
WORKDIR /app
RUN apt-get update -y \
 && apt-get install --no-install-recommends dnsutils mysql-client postgresql-client xmlsec1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists \
 && true
COPY requirements.txt ./
RUN pip wheel --wheel-dir=/tmp/wheels -r ./requirements.txt
FROM python:2-slim
WORKDIR /app
RUN apt-get update -y \
 && mkdir -p /usr/share/man/man1 /usr/share/man/man7 \
 && apt-get install --no-install-recommends libopenjp2-7 libjpeg62 libtiff5 dnsutils mysql-client libmariadbclient18 xmlsec1 postgresql-client -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists \
 && true
RUN pip install pip==23.1 --no-cache-dir --upgrade
COPY --from=build /tmp/wheels /tmp/wheels
COPY requirements.txt ./
RUN pip install --no-cache-dir --no-index --find-links=/tmp/wheels -r ./requirements.txt
COPY docker/entrypoint-celery-beat.sh docker/entrypoint-celery-worker.sh docker/entrypoint-initializer.sh docker/entrypoint-uwsgi.sh docker/entrypoint-uwsgi-dev.sh docker/entrypoint-unit-tests.sh docker/entrypoint-unit-tests-devDocker.sh docker/wait-for-it.sh /
COPY wsgi.py manage.py tests/unit-tests.sh ./
COPY dojo/ ./dojo/
#   Legacy installs need the modified settings.py, do not remove!
RUN cp dojo/settings/settings.dist.py dojo/settings/settings.py
COPY tests/ ./tests/
RUN mkdir dojo/migrations \
 && chmod g=u dojo/migrations \
 && chmod g=u /var/run \
 && true
USER 1001
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV DD_ADMIN_USER="admin" \
    DD_ADMIN_MAIL="admin@defectdojo.local" \
    DD_ADMIN_FIRST_NAME="Administrator" \
    DD_ADMIN_LAST_NAME="User" \
    DD_ALLOWED_HOSTS="*" \
    DD_CELERY_BEAT_SCHEDULE_FILENAME="/run/celery-beat-schedule" \
    DD_CELERY_BROKER_SCHEME="amqp" \
    DD_CELERY_BROKER_USER="defectdojo" \
    DD_CELERY_BROKER_HOST="rabbitmq" \
    DD_CELERY_BROKER_PORT="5672" \
    DD_CELERY_BROKER_PATH="//" \
    DD_CELERY_LOG_LEVEL="INFO" \
    DD_DATABASE_ENGINE="django.db.backends.mysql" \
    DD_DATABASE_HOST="mysql" \
    DD_DATABASE_NAME="defectdojo" \
    DD_DATABASE_PORT="3306" \
    DD_DATABASE_USER="defectdojo" \
    DD_CREDENTIAL_AES_256_KEY="&91a*agLqesc*0DJ+2*bAbsUZfR*4nLw" \
    DD_INITIALIZE="true" \
    DD_UWSGI_MODE="socket" \
    DD_UWSGI_ENDPOINT="0.0.0.0:3031" \
    DD_DJANGO_ADMIN_ENABLED="on" \
    DD_TRACK_MIGRATIONS="on"
ENTRYPOINT ["/entrypoint-uwsgi.sh"]
# Please add your HEALTHCHECK here!!!
