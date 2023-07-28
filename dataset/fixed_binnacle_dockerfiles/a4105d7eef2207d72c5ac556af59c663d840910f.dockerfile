#   FRONTEND BUILD CONTAINER
FROM ubuntu:18.04 AS ui-builder
MAINTAINER Gigantum <support@gigantum.com>
#   Install system level dependencies
RUN apt-get update -y \
 && apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 curl=7.58.0-2ubuntu3.24 gosu=1.10-1ubuntu0.18.04.1 nodejs=8.10.0~dfsg-2ubuntu0.4 npm=3.5.2-0ubuntu4 -y
RUN npm install yarn@1.22.19 -g
#   Install babel-node
RUN npm install @babel/cli@7.2.3 @babel/core@7.2.2 jest@24.0.0 relay-compiler@1.6.1 -g
#   Make build location dir
RUN mkdir /opt/ui
#   Set the react port to match nginx
ENV PORT="\"10001\""
#   Copy source to build
COPY ui /opt/ui
COPY resources/docker/ui_build_script.sh /opt/ui_build_script.sh
RUN /bin/bash /opt/ui_build_script.sh
#   PRODUCTION CONTAINER
FROM ubuntu:18.04
LABEL maintainer="Gigantum <support@gigantum.com>"
#   Copy requirements files
COPY packages/gtmcore/requirements.txt /opt/gtmcore/requirements.txt
COPY packages/gtmapi/requirements.txt /opt/gtmapi/requirements.txt
COPY packages/confhttpproxy /opt/confhttpproxy
ENV SHELL="/bin/bash"
#   Super instruction to install all dependencies
RUN apt-get update -y \
 && apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 nginx=1.14.0-0ubuntu1.11 supervisor=3.3.1-1.1 wget=1.19.4-1ubuntu2.2 openssl=1.1.1-1ubuntu2.1~18.04.21 python3=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-distutils=3.6.9-1~18.04 gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 gosu=1.10-1ubuntu0.18.04.1 redis-server=5:4.0.9-1ubuntu0.2 libjpeg-dev=8c-2ubuntu8 git-lfs=2.3.4-1 python3-setuptools=39.0.1-2ubuntu0.1 python3-dev=3.6.7-1~18.04 libdpkg-perl=1.19.0.5ubuntu2.4 zip=3.0-11build1 unzip=6.0-21ubuntu1.2 libsnappy-dev=1.1.7-1 -y \
 && git lfs install \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 gnupg=2.2.4-1ubuntu1.6 gnupg1=1.4.22-3ubuntu2 gnupg2=2.2.4-1ubuntu1.6 -y \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -y \
 && npm install configurable-http-proxy@4.5.5 -g \
 && cd /opt/confhttpproxy \
 && pip3 install . \
 && pip3 install wheel \
 && pip3 install -r /opt/gtmcore/requirements.txt \
 && pip3 install -r /opt/gtmapi/requirements.txt \
 && pip3 install uwsgi \
 && apt-get -qq -y remove gcc g++ python3-dev wget curl gnupg gnupg1 gnupg2 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /var/log/dpkg.log
#   Arguments, defaulted to production values
ARG CLIENT_CONFIG_FILE=build/client/labmanager-config.yaml
ARG NGINX_UI_CONFIG=resources/client/nginx_ui.conf
ARG NGINX_API_CONFIG=resources/client/nginx_api.conf
ARG SUPERVISOR_CONFIG=build/client/supervisord.conf
ARG ENTRYPOINT_FILE=resources/client/entrypoint.sh
ARG REDIS_CONFIG=resources/client/redis.conf
#   Copy Libraries
#   TODO: Make pip installs once refactor is completed
COPY packages/gtmapi /opt/gtmapi
COPY packages/gtmcore /opt/gtmcore
RUN cd /opt/gtmcore/ \
 && python3 setup.py install
#   Install testing requirements (will essentially be a noop in production)
COPY build/requirements-testing.txt /opt/requirements-testing.txt
RUN pip3 install -r /opt/requirements-testing.txt
#   Setup lmcommon config file - should be written by automation before copy
COPY $CLIENT_CONFIG_FILE /etc/gigantum/labmanager.yaml
#   Setup logging config file
COPY packages/gtmcore/gtmcore/logging/logging.json.default /etc/gigantum/logging.json
#   Make needed directories
RUN mkdir -p /mnt/gigantum \
 && mkdir /opt/redis
#   Copy frontend
COPY --from=ui-builder /opt/ui/build /var/www/
#   Setup NGINX/uWSGI
COPY $NGINX_UI_CONFIG /etc/nginx/sites-enabled/
COPY $NGINX_API_CONFIG /etc/nginx/sites-enabled/
RUN rm /etc/nginx/sites-enabled/default
#   Setup Redis
COPY $REDIS_CONFIG /opt/redis/redis.conf
#   Setup Supervisord to launch both uwsgi and nginx
RUN mkdir -p /opt/log/supervisor \
 && mkdir -p /opt/log/nginx \
 && mkdir -p /opt/run \
 && mkdir -p /opt/nginx \
 && nginx \
 && nginx -s reload \
 && nginx -s quit
COPY resources/client/supervisord_base.conf /etc/supervisor/supervisord.conf
COPY $SUPERVISOR_CONFIG /etc/supervisor/conf.d/supervisord.conf
COPY $ENTRYPOINT_FILE /usr/local/bin/entrypoint.sh
RUN chmod u+x /usr/local/bin/entrypoint.sh
#   Setup demo labbook
COPY resources/my-first-project.zip /opt/my-first-project.zip
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
#   Expose Ports
EXPOSE 10000/tcp 10001/tcp 10002/tcp
#   Start by firing up uwsgi, nginx, redis, and workers via supervisord
CMD ["/usr/bin/supervisord", "--nodaemon"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
