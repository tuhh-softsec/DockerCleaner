FROM python:2.7-slim
ENV APP_DIR="/munkiwebadmin"
#  munkiwebadmin config
ENV APPNAME="'MunkiWebAdmin'"
ENV TIME_ZONE="'UTC'"
ENV LANGUAGE_CODE="'en-us'"
ENV SIMPLEMDMKEY="''"
ENV ALLOWED_HOSTS="'[*]'"
ENV DEFAULT_MANIFEST="'serail_number'"
ENV PROXY_ADDRESS="''"
ENV STYLE="'default'"
ENV VAULT_USERNAME="'admin'"
ENV CONVERT_TO_QWERTZ="''"
#  database
ENV DB="'postgres'"
ENV DB_NAME="'munkiwebadmin_db'"
ENV DB_USER="'postgres'"
ENV DB_PASS="'postgres'"
ENV DB_HOST="'db'"
ENV DB_PORT="'5432'"
#   Install all debian packages
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends gcc=4:8.3.0-1 g++=4:8.3.0-1 unixodbc-dev=2.3.6-0.1 mysql-client libmariadbclient-dev=1:10.3.38-0+deb10u1 libpq-dev=11.19-0+deb10u1 sqlite3=3.27.2-3+deb10u2 net-tools=1.60+git20180626.aebd88e-1 supervisor=3.3.5-1 unzip=6.0-23+deb10u3 tdsodbc=1.00.104-1+deb10u1 git=1:2.20.1-2+deb10u8 gnupg=2.2.12-1+deb10u2 curl=7.64.0-4+deb10u5 nginx=1.14.2-2+deb10u5 apt-transport-https=1.8.2.3 libsasl2-dev=2.1.27+dfsg-1+deb10u2 python-dev=2.7.16-1 libldap2-dev=2.4.47+dfsg-3+deb10u7 libssl-dev=1.1.1n-0+deb10u4 dnsutils=1:9.11.5.P4+dfsg-5.1+deb10u8 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   install necessary locales
RUN apt-get clean \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends locales=2.28-10+deb10u2 -y )
RUN sed -i '/^#.* en_US.* /s/^#//' /etc/locale.gen
RUN locale-gen
#   install mssql
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
RUN curl https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list
RUN : \
 && ACCEPT_EULA=Y apt-get install -y msodbcsql17
#   create dirs
RUN mkdir ${APP_DIR}
RUN mkdir /munkirepo
RUN mkdir /munkitools
RUN mkdir /config
RUN mkdir /fieldkeys
#   download munkitools
RUN curl -Lk -o munkitools.zip `curl --silent https://api.github.com/repos/munki/munki/releases/latest | /usr/bin/awk '/zipball_url/ { print $2 }' | sed 's/[",]//g' ` \
 && unzip munkitools.zip -d . \
 && rm -rf /munkitools.zip
RUN cp -r /munki-munki*/code/client/* /munkitools \
 && rm -rf /munki-munki*
#   Copy all source files to the container's working directory
COPY . ${APP_DIR}
WORKDIR ${APP_DIR}
#  load default style
RUN curl -Lk -o /tmp/mwa2-style.zip https://github.com/SteveKueng/mwa2-style/archive/master.zip \
 && unzip /tmp/mwa2-style.zip -d /tmp \
 && rm -rf /tmp/mwa2-style.zip
RUN mkdir -p /munkiwebadmin/munkiwebadmin/static/styles/default
RUN cp -r /tmp/mwa2-style-master/* /munkiwebadmin/munkiwebadmin/static/styles/default \
 && rm -rf /tmp/mwa2-style-master
#   clean pyc
RUN find ${APP_DIR} -name '*.pyc' -delete
#   Install all python dependency libs
RUN pip install -r requirements.txt
#   Configure Nginx, uWSGI and supervisord
COPY docker/nginx.conf /etc/nginx/nginx.conf
RUN mkdir /var/log/uwsgi
COPY docker/supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   configure git
RUN git config --global core.preloadindex true
RUN git config --global core.fscache true
RUN git config --global gc.auto 256
VOLUME [ "/munkirepo", "/fieldkeys", "/reposado" ]
#   Exposed port
EXPOSE 80/tcp
ENTRYPOINT ["/bin/sh", "docker/run.sh"]
CMD ["/usr/bin/supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
