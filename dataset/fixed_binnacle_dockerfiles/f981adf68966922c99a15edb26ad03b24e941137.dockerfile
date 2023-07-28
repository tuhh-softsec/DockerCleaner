FROM python:3.5-alpine
#   Set environment variables
ENV APP_ROOT="/usr/src/app"
ENV APP_USER_NAME="app"
ENV APP_USER_UID="1000"
ENV IS_ALPINE="true"
ENV NODE_VERSION="8.x"
#   Install alpine dependencies
#  # Upgrade apk-tools
RUN apk add apk-tools=2.10.8-r1 --upgrade --no-cache --repository http://dl-cdn.alpinelinux.org/alpine/edge/main
#  # Install system dependencies
RUN apk add bash=5.0.17-r0 wget=1.20.3-r1 dpkg-dev=1.20.0-r0 libffi=3.3-r2 nodejs=12.22.12-r0 nodejs-npm git=2.26.3-r1 gcc=9.3.0-r2 musl-dev=1.1.24-r10 gettext=0.20.2-r0 postgresql-dev=12.10-r0 libffi-dev=3.3-r2 py3-setuptools=47.0.0-r0 jpeg-dev=9d-r0 make=4.3-r0 zlib-dev=1.2.12-r3 freetype-dev=2.10.4-r2 lcms2-dev=2.9-r1 openjpeg-dev=2.4.0-r1 libxslt-dev=1.1.35-r0 alpine-sdk=1.0-r0 --update --no-cache --repository http://dl-cdn.alpinelinux.org/alpine/edge/main \
 && rm -rf /var/cache/apk/* /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  # Install react dependencies
RUN npm install npm@9.6.4 -g
RUN npm install yarn@1.22.19 webpack@^1.12.13 bower@1.8.14 less@4.1.3 -g
#  # Install python dependencies
RUN pip3 install --no-cache-dir cffi cairocffi psycopg2
RUN apk add cairo-dev=1.16.0-r5 --update --no-cache \
 && rm -rf /var/cache/apk/* /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Create user
RUN adduser -D -h ${APP_ROOT} -s /bin/bash -u ${APP_USER_UID} ${APP_USER_NAME}
#   Create folders for deploy
RUN mkdir -p ${APP_ROOT}
RUN mkdir -p /var/log/eventol
RUN chown ${APP_USER_NAME}:root /var/log/eventol
WORKDIR ${APP_ROOT}
#   Install python requirements
COPY ./requirements.txt ${APP_ROOT}
COPY ./requirements-dev.txt ${APP_ROOT}
RUN pip3 install --no-cache-dir -r requirements.txt
RUN pip3 install --no-cache-dir -r requirements-dev.txt
#   Install node modules
COPY ./eventol/front/package.json ${APP_ROOT}/eventol/front/
COPY ./eventol/front/yarn.lock ${APP_ROOT}/eventol/front/
RUN cd ${APP_ROOT}/eventol/front \
 && yarn install
RUN cd ${APP_ROOT}/eventol/front \
 && npm rebuild node-sass --force
#   Install bower dependencies
COPY ./eventol/front/bower.json ${APP_ROOT}/eventol/front/
COPY ./eventol/front/.bowerrc ${APP_ROOT}/eventol/front/
RUN cd ${APP_ROOT}/eventol/front \
 && bower install --allow-root
#   Copy test script file
COPY ./test.sh ${APP_ROOT}/test.sh
#   Copy python code
COPY ./eventol ${APP_ROOT}/eventol
RUN mkdir -p ${APP_ROOT}/eventol/manager/static
RUN mkdir -p ${APP_ROOT}/eventol/front/eventol/static
#   Compile scss
RUN mkdir -p ${APP_ROOT}/eventol/manager/static/manager/css/
RUN lessc ${APP_ROOT}/eventol/front/eventol/static/manager/less/eventol.less > ${APP_ROOT}/eventol/manager/static/manager/css/eventol.css
RUN lessc ${APP_ROOT}/eventol/front/eventol/static/manager/less/eventol-bootstrap.less > ${APP_ROOT}/eventol/manager/static/manager/css/eventol-bootstrap.css
#   Copy script for docker-compose wait and start-eventol
COPY ./deploy/docker/scripts/wait-for-it.sh ${APP_ROOT}/wait-for-it.sh
COPY ./deploy/docker/scripts/start_eventol.sh ${APP_ROOT}/start_eventol.sh
#   Compile reactjs code
RUN cd ${APP_ROOT}/eventol/front \
 && webpack --config webpack.prod.config.js
#   Collect statics
RUN mkdir -p ${APP_ROOT}/eventol/static
RUN cd ${APP_ROOT}/eventol \
 && python manage.py collectstatic --noinput
#   Create media folder
RUN mkdir -p ${APP_ROOT}/eventol/media
#   Clean and chown files
RUN rm -rf ${APP_ROOT}/eventol/front \
 && mkdir -p ${APP_ROOT}/eventol/front
RUN chmod 0755 ${APP_ROOT}
RUN chown --changes --recursive ${APP_USER_NAME}:${APP_USER_NAME} ${APP_ROOT}/
#   Drop privs
USER ${APP_USER_NAME}
#   Create log file
RUN touch /var/log/eventol/eventol.log
#   Compile .po files
RUN sed -i 's@#~ @@g' ${APP_ROOT}/eventol/conf/locale/*/LC_MESSAGES/djangojs.po
RUN cd ${APP_ROOT}/eventol \
 && python manage.py compilemessages
EXPOSE 8000/tcp
VOLUME ${APP_ROOT}/eventol/media
VOLUME ${APP_ROOT}/eventol/static
CMD ["tail", "-f", "/dev/null"]
# Please add your HEALTHCHECK here!!!
