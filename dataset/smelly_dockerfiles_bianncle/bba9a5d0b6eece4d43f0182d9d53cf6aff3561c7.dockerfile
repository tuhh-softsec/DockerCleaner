FROM python:3.6
#  Adapted from tiangolo-uwsgi-flask (https://github.com/tiangolo/uwsgi-nginx-flask-docker) or
#  (https://github.com/tiangolo/uwsgi-nginx-docker/blob/master/python3.6/Dockerfile)
ENV NGINX_VERSION="1.13.12-1~stretch"
ENV NJS_VERSION="1.13.12.0.2.0-1~stretch"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends gnupg1 apt-transport-https ca-certificates --no-install-suggests -y \
 && NGINX_GPGKEY=573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62 ; found='' ; for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu; do echo "Fetching GPG key $NGINX_GPGKEY from $server" ;apt-key adv --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$NGINX_GPGKEY" \
 && found=yes \
 && break ; done ; test -z "$found" \
 && echo "error: failed to fetch GPG key $NGINX_GPGKEY" >&2 \
 && exit 1 ; apt-get remove --purge --auto-remove -y gnupg1 \
 && rm -rf /var/lib/apt/lists/* \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && nginxPackages=" nginx=${NGINX_VERSION} " \
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
#  forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
EXPOSE 80/tcp 443/tcp
#  Standard set up Nginx finished
#  Install uWSGI
RUN pip install uwsgi
#  Make NGINX run on the foreground
RUN echo "daemon off;" >> /etc/nginx/nginx.conf
#  Remove default configuration from Nginx
RUN rm /etc/nginx/conf.d/default.conf
#  Copy the modified Nginx conf
COPY .docker/nginx.conf /etc/nginx/conf.d/
#  Copy the base uWSGI ini file to enable default dynamic uwsgi process number
COPY .docker/uwsgi.ini /etc/uwsgi/
#  Install Supervisord
RUN apt-get update \
 && apt-get install supervisor sqlite3 libsqlite3-dev uwsgi-plugin-python3 -y \
 && rm -rf /var/lib/apt/lists/*
#  Custom Supervisord config
COPY .docker/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#  Which uWSGI .ini file should be used, to make it customizable
#  ENV UWSGI_INI /commandment/uwsgi.ini
#  By default, run 2 processes
ENV UWSGI_CHEAPER="2"
#  By default, when on demand, run up to 16 processes
ENV UWSGI_PROCESSES="16"
#  By default, allow unlimited file sizes, modify it to limit the file sizes
#  To have a maximum of 1 MB (Nginx's default) change the line to:
#  ENV NGINX_MAX_UPLOAD 1m
ENV NGINX_MAX_UPLOAD="0"
#  By default, Nginx will run a single worker process, setting it to auto
#  will create a worker for each CPU core
ENV NGINX_WORKER_PROCESSES="1"
#  By default, Nginx listens on port 80.
#  To modify this, change LISTEN_PORT environment variable.
#  (in a Dockerfile or with an option for `docker run`)
ENV LISTEN_PORT="80"
COPY . /commandment
WORKDIR /commandment
RUN pip install pipenv
RUN pipenv install --system
COPY .docker/uwsgi-commandment.ini /etc/uwsgi/uwsgi-commandment.ini
COPY .docker/entry.sh /entry.sh
COPY .docker/settings.cfg.docker /settings.cfg
CMD ["/entry.sh"]
