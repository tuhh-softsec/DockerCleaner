FROM alpine:3.7
#   Internals, you probably don't need to change these
ENV APP_DIR="/srv/app"
ENV SRC_DIR="/srv/app/src"
ENV CKAN_INI="${APP_DIR}/production.ini"
ENV PIP_SRC="${SRC_DIR}"
ENV CKAN_STORAGE_PATH="/var/lib/ckan"
ENV GIT_URL="https://github.com/ckan/ckan.git"
#   CKAN version to build
ENV GIT_BRANCH="ckan-2.7.5"
#   Customize these on the .env file if needed
ENV CKAN_SITE_URL="http://localhost:5000"
ENV CKAN__PLUGINS="image_view text_view recline_view datastore datapusher envvars"
WORKDIR ${APP_DIR}
#   Install necessary packages to run CKAN
RUN apk add tzdata=2019c-r0 git=2.15.4-r0 gettext=0.19.8.1-r1 postgresql-client=10.10-r0 python apache2-utils=2.4.41-r0 libxml2=2.9.8-r1 libxslt=1.1.31-r2 musl-dev=1.1.18-r4 uwsgi=2.0.17-r0 uwsgi-http=2.0.17-r0 uwsgi-corerouter=2.0.17-r0 uwsgi-python=2.0.17-r0 py2-gevent=1.2.2-r0 uwsgi-gevent=2.0.17-r0 libmagic=5.32-r2 curl=7.61.1-r3 sudo=1.8.21_p2-r1 --no-cache \
 && apk add postgresql-dev=10.10-r0 gcc=6.4.0-r5 make=4.2.1-r0 g++=6.4.0-r5 autoconf=2.69-r0 automake=1.15.1-r0 libtool=2.4.6-r4 python-dev libxml2-dev=2.9.8-r1 libxslt-dev=1.1.31-r2 linux-headers=4.4.6-r2 --no-cache --virtual .build-deps \
 && mkdir -p ${SRC_DIR} \
 && curl -o ${SRC_DIR}/get-pip.py https://bootstrap.pypa.io/get-pip.py \
 && python ${SRC_DIR}/get-pip.py \
 && pip install supervisor==4.2.5 \
 && mkdir /etc/supervisord.d \
 && rm -rf ${SRC_DIR}/get-pip.py
RUN echo "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
 && apk add geos@testing geos-dev@testing py-geos@testing --update --no-cache
COPY setup/supervisord.conf /etc
#   Install CKAN
RUN pip install -e git+${GIT_URL}@${GIT_BRANCH}#egg=ckan \
 && cd ${SRC_DIR}/ckan \
 && cp who.ini ${APP_DIR} \
 && sed -i -e "s/psycopg2==2.4.5/psycopg2==2.7.3.2/g" requirements.txt \
 && pip install :all:==null --no-binary -r requirements.txt \
 && pip install -e git+https://github.com/okfn/ckanext-envvars.git#egg=ckanext-envvars \
 && paster --plugin=ckan make-config ckan ${CKAN_INI} \
 && paster --plugin=ckan config-tool ${CKAN_INI} "ckan.plugins = ${CKAN__PLUGINS}" \
 && paster --plugin=ckan config-tool ${CKAN_INI} "ckan.site_url = ${CKAN__SITE_URL}"
#   Create a local user and group to run the app
RUN addgroup -g 92 -S ckan \
 && adduser -u 92 -h /srv/app -H -D -S -G ckan ckan
#   Create local storage folder
RUN mkdir -p $CKAN_STORAGE_PATH \
 && chown -R ckan:ckan $CKAN_STORAGE_PATH
COPY setup ${APP_DIR}
COPY setup/supervisor.worker.conf /etc/supervisord.d/worker.conf
COPY setup/uwsgi.conf /srv/app/uwsgi.conf
#   Create entrypoint directory for children image scripts
ONBUILD RUN mkdir /docker-entrypoint.d
RUN chown ckan -R /srv/app
EXPOSE 5000/tcp
HEALTHCHECK --interval=10s --timeout=5s --retries=5 CMD curl --fail http://localhost:5000/api/3/action/status_show || exit 1
CMD ["/srv/app/start_ckan.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
