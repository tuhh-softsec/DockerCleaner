FROM python:2.7
LABEL maintainer="anhio <anhio@gmail.com>"
#   Install uWSGI
RUN pip install uwsgi==2.0.21
#   Standard set up Nginx
ENV NGINX_VERSION="1.13.12-1~stretch"
ENV NJS_VERSION="1.13.12.0.2.0-1~stretch"
RUN set -x \
 && sed -i s@/archive.ubuntu.com/@/mirrors.aliyun.com/@g /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends gnupg1 apt-transport-https ca-certificates --no-install-suggests -y \
 && NGINX_GPGKEY=573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62 ; found='' ; for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu; do echo "Fetching GPG key $NGINX_GPGKEY from $server" ;apt-key adv --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$NGINX_GPGKEY" \
 && found=yes \
 && break ; done ; test -z "$found" \
 && echo "error: failed to fetch GPG key $NGINX_GPGKEY" >&2 \
 && exit 1 ; apt-get remove --purge --auto-remove -y gnupg1 \
 && rm -rf /var/lib/apt/lists/* \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION} nginx-module-xslt=${NGINX_VERSION} nginx-module-geoip=${NGINX_VERSION} nginx-module-image-filter=${NGINX_VERSION} nginx-module-njs=${NJS_VERSION} " \
 && case "$dpkgArch" in (amd64|i386) echo "deb https://nginx.org/packages/mainline/debian/ stretch nginx" >> /etc/apt/sources.list.d/nginx.list \
 && apt-get update ;;(*) echo "deb-src https://nginx.org/packages/mainline/debian/ stretch nginx" >> /etc/apt/sources.list.d/nginx.list \
 && tempDir="$( mktemp -d ;)" \
 && chmod 777 "$tempDir" \
 && savedAptMark="$( apt-mark showmanual ;)" \
 && apt-get update \
 && apt-get build-dep -y $nginxPackages \
 && (cd "$tempDir" \
 && DEB_BUILD_OPTIONS="nocheck parallel=$( nproc ;)" apt-get source --compile $nginxPackages ) \
 && apt-mark showmanual | xargs apt-mark auto > /dev/null \
 && { [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; } \
 && ls -lAFh "$tempDir" \
 && (cd "$tempDir" \
 && dpkg-scanpackages . > Packages) \
 && grep '^Package: ' "$tempDir/Packages" \
 && echo "deb [ trusted=yes ] file://$tempDir ./" > /etc/apt/sources.list.d/temp.list \
 && apt-get -o Acquire::GzipIndexes=false update ;; esac \
 && apt-get install --no-install-recommends gettext-base $nginxPackages --no-install-suggests -y \
 && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/nginx.list \
 && if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove \
 && rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi
#   forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
EXPOSE 8080/tcp
#   Removed the section that breaks pip installations
#   && apt-get remove --purge --auto-remove -y apt-transport-https ca-certificates 
#   Standard set up Nginx finished
#   Expose 443, in case of LTS / HTTPS
#  EXPOSE 443
#   Make NGINX run on the foreground
RUN echo "daemon off;" >> /etc/nginx/nginx.conf
#   Remove default configuration from Nginx
RUN rm /etc/nginx/conf.d/default.conf
#   Copy the modified Nginx conf
COPY conf/nginx.conf /etc/nginx/conf.d/
#   Copy the base uWSGI ini file to enable default dynamic uwsgi process number
COPY conf/uwsgi.ini /etc/uwsgi/
#   Install Supervisord
RUN apt-get update \
 && apt-get install --no-install-recommends supervisor -y \
 && rm -rf /var/lib/apt/lists/*
#   Custom Supervisord config
COPY conf/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   Which uWSGI .ini file should be used, to make it customizable
ENV UWSGI_INI="/data/envs/wam/conf/uwsgi.ini"
#   By default, run 2 processes
ENV UWSGI_CHEAPER="2"
#   By default, when on demand, run up to 16 processes
ENV UWSGI_PROCESSES="16"
#   By default, allow unlimited file sizes, modify it to limit the file sizes
#   To have a maximum of 1 MB (Nginx's default) change the line to:
#   ENV NGINX_MAX_UPLOAD 1m
ENV NGINX_MAX_UPLOAD="0"
#   By default, Nginx will run a single worker process, setting it to auto
#   will create a worker for each CPU core
ENV NGINX_WORKER_PROCESSES="1"
#   By default, Nginx listens on port 80.
#   To modify this, change LISTEN_PORT environment variable.
#   (in a Dockerfile or with an option for `docker run`)
ENV LISTEN_PORT="8080"
#   Copy the entrypoint that will generate Nginx additional configs
COPY conf/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh \
 && mkdir /var/log/wam
ENTRYPOINT ["/entrypoint.sh"]
#   Add demo app
COPY ./ /data/projects/wam/
COPY ./conf /data/envs/wam/conf
WORKDIR /data/projects/wam
RUN pip install -r /data/projects/wam/requirements.txt \
 && mkdir /data/projects/wam/monitor/logs \
 && python /data/projects/wam/manage.py migrate \
 && echo "from django.contrib.auth import get_user_model; User = get_user_model(); User.objects.create_superuser ('admin', 'admin@wam.com', 'password')" | python manage.py shell \
 && chmod -R 777 /data/projects/wam
CMD ["/usr/bin/supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
