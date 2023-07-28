#   QIS Application server
#
#   Runs a pre-configured instance of Apache+QIS on Ubuntu 16.04.
#   Requires qis-postgres and qis-memcached containers at runtime.
#
#   Environment variables:
#     HOSTNAME - Required - the host name for Apache to listen on
#     DB_USER - Required - the database username
#     DB_PASSWORD - Required - the database password
#
#     HTTP_PROCESSES - Optional - set the number of mod_wsgi processes for HTTP
#     HTTP_THREADS - Optional - set the number of mod_wsgi threads per process for HTTP
#     HTTPS_PROCESSES - Optional - set the number of mod_wsgi processes for HTTPS
#     HTTPS_THREADS - Optional - set the number of mod_wsgi threads per process for HTTPS
#
FROM ubuntu:16.04
LABEL maintainer="matt@quru.com" \
      description="QIS web application server"
#   Base o/s + app layer
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.2.35 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 pwgen=2.07-1.1ubuntu1 tar=1.28-2.1ubuntu0.2 zip=3.0-11 unzip=6.0-20ubuntu1.1 vim=2:7.4.1689-3ubuntu1.5 locales=2.23-0ubuntu11.3 openssl=1.0.2g-1ubuntu4.20 ldap-utils=2.4.42+dfsg-2ubuntu3.13 libmemcached11=1.0.18-4.1ubuntu2 python3=3.5.1-3 postgresql-client-9.5=9.5.25-0ubuntu0.16.04.1 ghostscript=9.26~dfsg+0-0ubuntu0.16.04.14 imagemagick-6.q16=8:6.8.9.9-7ubuntu5.16 imagemagick-common=8:6.8.9.9-7ubuntu5.16 libmagickwand-6.q16-2=8:6.8.9.9-7ubuntu5.16 apache2=2.4.18-2ubuntu3.17 apache2-utils=2.4.18-2ubuntu3.17 logrotate=3.8.7-2ubuntu2.16.04.2 libapache2-mod-wsgi-py3=4.3.0-1.1ubuntu1 -y \
 && apt-get clean
RUN locale-gen en_GB.UTF-8 \
 && update-locale LANG=en_GB.UTF-8 LC_ALL=en_GB.UTF-8
#   Build variables
ARG QIS_VERSION=4.1.3
ARG QIS_USER=qis
ARG QIS_INSTALL_DIR=/opt/qis
ARG QIS_SAMPLES_DIR=/opt/qis-samples
ARG WEB_USER=www-data
#   Ports
EXPOSE 80/tcp 443/tcp
#   Runtime environment variables
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV HOSTNAME="images.example.com" \
    DB_USER="qis" \
    QIS_HOME="$QIS_INSTALL_DIR" \
    QIS_SAMPLES="$QIS_SAMPLES_DIR" \
    HTTP_USER="$QIS_USER" \
    HTTP_PROCESSES="2" \
    HTTP_THREADS="15" \
    HTTPS_PROCESSES="2" \
    HTTPS_THREADS="15"
#   Create the application user
RUN useradd --comment "Quru Image Server" --groups $WEB_USER --home $QIS_INSTALL_DIR --system --shell /sbin/nologin $QIS_USER
#   Create the app dirs
RUN mkdir -p $QIS_INSTALL_DIR $QIS_SAMPLES_DIR
#   Install scripts
COPY *.sh /
RUN chmod a+x /*.sh
#   Download and install QIS files
RUN cd /tmp \
 && curl -L "https://github.com/quru/qis/archive/v$QIS_VERSION.tar.gz" -o qis.tar.gz \
 && tar -zxvf qis.tar.gz \
 && cd qis-$QIS_VERSION \
 && rm -rf src/tests src/*.sh src/runserver.py \
 && rm -rf deploy/docker/ci-build-* \
 && rm -rf doc/v* \
 && rm -rf images/test* \
 && cp LICENSE README.md $QIS_INSTALL_DIR \
 && cp -r conf $QIS_INSTALL_DIR \
 && cp -r deploy $QIS_INSTALL_DIR \
 && cp -r doc $QIS_INSTALL_DIR \
 && cp -r icc $QIS_INSTALL_DIR \
 && cp -r images $QIS_INSTALL_DIR \
 && cp -r images/* $QIS_SAMPLES_DIR \
 && cp -r licences $QIS_INSTALL_DIR \
 && cp -r logs $QIS_INSTALL_DIR \
 && cp -r src $QIS_INSTALL_DIR \
 && cp deploy/ubuntu16/wsgi.conf /etc/apache2/sites-available/qis-wsgi.conf \
 && cp deploy/ubuntu16/httpd.conf.sample /etc/apache2/sites-available/001-qis.conf \
 && cp deploy/ubuntu16/httpd-ssl.conf.sample /etc/apache2/sites-available/002-qis-ssl.conf \
 && cd - \
 && rm -rf /tmp/* \
 && chown -R $QIS_USER:$QIS_USER $QIS_INSTALL_DIR $QIS_SAMPLES_DIR
#   Download and install Python libs
WORKDIR $QIS_INSTALL_DIR
RUN curl -L "https://github.com/quru/qis/releases/download/v$QIS_VERSION/QIS-libs-ubuntu-16-py35-x86_64.tar.gz" -o /tmp/qis-libs.tar.gz \
 && tar -zxvf /tmp/qis-libs.tar.gz \
 && rm /tmp/qis-libs.tar.gz \
 && chown -R $QIS_USER:$QIS_USER lib
#   Configure Apache
RUN ln -s /etc/apache2/mods-available/ssl.load /etc/apache2/mods-enabled/ssl.load \
 && ln -s /etc/apache2/mods-available/ssl.conf /etc/apache2/mods-enabled/ssl.conf \
 && ln -s /etc/apache2/mods-available/socache_shmcb.load /etc/apache2/mods-enabled/socache_shmcb.load \
 && ln -s /etc/apache2/mods-available/expires.load /etc/apache2/mods-enabled/expires.load \
 && ln -s /etc/apache2/mods-available/headers.load /etc/apache2/mods-enabled/headers.load \
 && rm /etc/apache2/sites-enabled/000-default.conf \
 && ln -s /etc/apache2/sites-available/qis-wsgi.conf /etc/apache2/sites-enabled/qis-wsgi.conf \
 && ln -s /etc/apache2/sites-available/001-qis.conf /etc/apache2/sites-enabled/001-qis.conf \
 && ln -s /etc/apache2/sites-available/002-qis-ssl.conf /etc/apache2/sites-enabled/002-qis-ssl.conf
#   Persistent storage volumes
VOLUME ["$QIS_INSTALL_DIR/images", "$QIS_INSTALL_DIR/logs", "/var/log/apache2"]
#   Run regular health checks
HEALTHCHECK --interval=30s --timeout=10s --retries=3 CMD ["/check-qis.sh"]
#   Note the "exec" form of CMD allows docker stop signals to be sent to our run script
CMD ["/run-qis.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
