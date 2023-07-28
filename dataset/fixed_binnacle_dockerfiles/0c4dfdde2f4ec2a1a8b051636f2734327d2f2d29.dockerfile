FROM ubuntu:16.04
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV PG_BRANCH="master" \
    WEBWORK_URL="/webwork2" \
    WEBWORK_ROOT_URL="http://localhost" \
    WEBWORK_DB_HOST="db" \
    WEBWORK_DB_PORT="3306" \
    WEBWORK_DB_NAME="webwork" \
    WEBWORK_DB_USER="webworkWrite" \
    WEBWORK_SMTP_SERVER="localhost" \
    WEBWORK_SMTP_SENDER="webwork@example.com" \
    WEBWORK_TIMEZONE="America/New_York" \
    APACHE_RUN_USER="www-data" \
    APACHE_RUN_GROUP="www-data" \
    APACHE_PID_FILE="/var/run/apache2/apache2.pid" \
    APACHE_RUN_DIR="/var/run/apache2" \
    APACHE_LOCK_DIR="/var/lock/apache2" \
    APACHE_LOG_DIR="/var/log/apache2" \
    APP_ROOT="/opt/webwork" \
    DEV="0"
ENV WEBWORK_DB_DSN="DBI:mysql:${WEBWORK_DB_NAME}:${WEBWORK_DB_HOST}:${WEBWORK_DB_PORT}" \
    WEBWORK_ROOT="$APP_ROOT/webwork2" \
    PG_ROOT="$APP_ROOT/pg" \
    PATH="$PATH:$APP_ROOT/webwork2/bin"
RUN apt-get update \
 && apt-get install --no-install-recommends apache2=2.4.18-2ubuntu3.17 curl=7.47.0-1ubuntu2.19 dvipng=1.15-0ubuntu1 gcc=4:5.3.1-1ubuntu1 libapache2-request-perl=2.13-4ubuntu2 libcrypt-ssleay-perl=0.73.04-1build1 libdatetime-perl=2:1.21-1build1 libdancer-perl=1.3202+dfsg-1 libdancer-plugin-database-perl=2.12-1 libdbd-mysql-perl=4.033-1ubuntu0.1 libemail-address-perl=1.908-1 libexception-class-perl=1.40-1 libextutils-xsbuilder-perl=0.28-3 libfile-find-rule-perl-perl=1.15-1 libgd-perl=2.53-2.1 libhtml-scrubber-perl=0.15-1 libjson-perl=2.90-1 liblocale-maketext-lexicon-perl=1.00-1 libmail-sender-perl=0.8.16-2 libmime-tools-perl=5.507-1 libnet-ip-perl=1.26-1 libnet-ldap-perl=1:0.6500+dfsg-1 libnet-oauth-perl=0.28-2 libossp-uuid-perl=1.6.2-1.5build2 libpadwalker-perl=2.2-1build1 libpath-class-perl=0.36-1 libphp-serialization-perl=0.34-1 libsoap-lite-perl=1.19-1 libsql-abstract-perl=1.81-1 libstring-shellquote-perl=1.03-1.2 libtemplate-perl=2.24-1.2build2 libtext-csv-perl=1.33-1 libtimedate-perl=2.3000-2 libuuid-tiny-perl=1.0400-1 libxml-parser-perl=2.44-1build1 libxml-writer-perl=0.625-1 libapache2-reload-perl=0.13-1 make=4.1-6 netpbm=2:10.0-15.3 preview-latex-style=11.88-1.1ubuntu1 texlive=2015.20160320-1ubuntu0.1 texlive-latex-extra=2015.20160320-1 libc6-dev=2.23-0ubuntu11.3 git=1:2.7.4-0ubuntu1.10 mysql-client=5.7.33-0ubuntu0.16.04.1 -y --no-install-suggests \
 && curl -Lk https://cpanmin.us | perl - App::cpanminus \
 && cpanm install XML::Parser::EasyTree Iterator Iterator::Util Pod::WSDL Array::Utils HTML::Template XMLRPC::Lite Mail::Sender Email::Sender::Simple Data::Dump Statistics::R::IO \
 && rm -fr /var/lib/apt/lists/* ./cpanm /root/.cpanm /tmp/*
RUN mkdir -p $APP_ROOT/courses $APP_ROOT/libraries $APP_ROOT/webwork2
#   Block to include webwork2 in the container, when needed, instead of  getting it from a bind mount.
#      Uncomment when needed, and set the correct branch name on the following line.
#  ENV WEBWORK_BRANCH=master   # need a valid branch name from https://github.com/openwebwork/webwork2
#  RUN curl -fSL https://github.com/openwebwork/webwork2/archive/${WEBWORK_BRANCH}.tar.gz -o /tmp/${WEBWORK_BRANCH}.tar.gz \
#      && cd /tmp \
#      && tar xzf /tmp/${WEBWORK_BRANCH}.tar.gz \
#      && mv webwork2-${WEBWORK_BRANCH} $APP_ROOT/webwork2 \
#      && rm -rf /tmp/${WEBWORK_BRANCH}.tar.gz /tmp/webwork2-${WEBWORK_BRANCH}
RUN curl -fSL https://github.com/openwebwork/pg/archive/${PG_BRANCH}.tar.gz -o /tmp/${PG_BRANCH}.tar.gz \
 && tar xzf /tmp/${PG_BRANCH}.tar.gz \
 && mv pg-${PG_BRANCH} $APP_ROOT/pg \
 && rm /tmp/${PG_BRANCH}.tar.gz \
 && curl -fSL https://github.com/openwebwork/webwork-open-problem-library/archive/master.tar.gz -o /tmp/opl.tar.gz \
 && tar xzf /tmp/opl.tar.gz \
 && mv webwork-open-problem-library-master $APP_ROOT/libraries/webwork-open-problem-library \
 && rm /tmp/opl.tar.gz
RUN curl -fSL https://github.com/mathjax/MathJax/archive/master.tar.gz -o /tmp/mathjax.tar.gz \
 && tar xzf /tmp/mathjax.tar.gz \
 && mv MathJax-master $APP_ROOT/MathJax \
 && rm /tmp/mathjax.tar.gz
#  && rm /tmp/VERSION
#  curl -fSL https://github.com/openwebwork/webwork2/archive/WeBWorK-${WEBWORK_VERSION}.tar.gz -o /tmp/WeBWorK-${WEBWORK_VERSION}.tar.gz \
#  && tar xzf /tmp/WeBWorK-${WEBWORK_VERSION}.tar.gz \
#  && mv webwork2-WeBWorK-${WEBWORK_VERSION} $APP_ROOT/webwork2 \
#  && rm /tmp/WeBWorK-${WEBWORK_VERSION}.tar.gz \
RUN echo "PATH=$PATH:$APP_ROOT/webwork2/bin" >> /root/.bashrc
COPY . $APP_ROOT/webwork2
#   Move these lines into docker-entrypoint.sh so the bind mount of courses
#   will be available
#  RUN cd $APP_ROOT/webwork2/courses.dist \
#      && cp *.lst $APP_ROOT/courses/ \
#      && cp -R modelCourse $APP_ROOT/courses/
RUN cd $APP_ROOT/pg/lib/chromatic \
 && gcc color.c -o color
#   setup apache
RUN cd $APP_ROOT/webwork2/conf \
 && cp webwork.apache2.4-config.dist webwork.apache2.4-config \
 && cp $APP_ROOT/webwork2/conf/webwork.apache2.4-config /etc/apache2/conf-enabled/webwork.conf \
 && a2dismod mpm_event \
 && a2enmod mpm_prefork \
 && sed -i -e 's/Timeout 300/Timeout 1200/' /etc/apache2/apache2.conf \
 && sed -i -e 's/MaxRequestWorkers 150/MaxRequestWorkers 20/' -e 's/MaxConnectionsPerChild 0/MaxConnectionsPerChild 100/' /etc/apache2/mods-available/mpm_prefork.conf \
 && cp $APP_ROOT/webwork2/htdocs/favicon.ico /var/www/html \
 && sed -i -e 's/^<Perl>$/ PerlPassEnv WEBWORK_URL\n PerlPassEnv WEBWORK_ROOT_URL\n PerlPassEnv WEBWORK_DB_DSN\n PerlPassEnv WEBWORK_DB_USER\n PerlPassEnv WEBWORK_DB_PASSWORD\n PerlPassEnv WEBWORK_SMTP_SERVER\n PerlPassEnv WEBWORK_SMTP_SENDER\n PerlPassEnv WEBWORK_TIMEZONE\n \n<Perl>/' /etc/apache2/conf-enabled/webwork.conf
RUN cd $APP_ROOT/webwork2/ \
 && chown www-data DATA ../courses htdocs/tmp htdocs/applets logs tmp $APP_ROOT/pg/lib/chromatic \
 && chmod -R u+w DATA ../courses htdocs/tmp htdocs/applets logs tmp $APP_ROOT/pg/lib/chromatic
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 80/tcp
WORKDIR $APP_ROOT
CMD ["apache2", "-DFOREGROUND"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
