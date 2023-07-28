FROM php:7-cli AS builder
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends gnupg2=2.2.27-2+deb11u2 -y )
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=12.22.12~dfsg-1~deb11u3 -y )
WORKDIR /app
COPY package.json .
RUN npm install
RUN npm install grunt-cli@1.4.3 -g
COPY .sandstorm/ .sandstorm/
COPY .tx/ .tx/
COPY docs/ docs/
COPY Jelastic/ Jelastic/
COPY build/ build/
COPY client/ client/
COPY restyaboard.conf .
COPY sql/ sql/
COPY api_explorer/ api_explorer/
COPY server/ server/
COPY media/ media/
COPY .codeclimate.yml .
COPY .htaccess .
COPY diagnose.php .
COPY ejabberd.yml .
COPY restyaboard.sh .
COPY restyaboard-ssl.conf .
COPY restyaboard_uninstall.sh .
COPY Gruntfile.js .
RUN npm run docker:prebuild
#   Result image
FROM debian:stretch
#   update & install package
RUN : \
 && echo "postfix postfix/mailname string localhost" | debconf-set-selections \
 && echo "postfix postfix/main_mailer_type string 'Internet Site'" | debconf-set-selections \
 && TERM=linux DEBIAN_FRONTEND=noninteractive apt-get install -y cron curl imagemagick jq libpq5 nginx postfix postgresql-client unzip wget
#   Necessary steps for php7.2
RUN (apt-get update ;apt-get install --no-install-recommends apt-transport-https=2.2.4 lsb-release=11.1.0 ca-certificates=20210119 -y ) \
 && wget -O /etc/apt/trusted.gpg.d/php.gpg https://packages.sury.org/php/apt.gpg \
 && sh -c 'echo "deb https://packages.sury.org/php/ $(lsb_release -sc) main" > /etc/apt/sources.list.d/php.list' \
 && apt-get update
RUN (apt-get update ;apt-get install --no-install-recommends php7.2 php7.2-cli php7.2-common php7.2-curl php7.2-fpm php7.2-imagick php7.2-imap php7.2-ldap php7.2-mbstring php7.2-pgsql php7.2-xml -y )
#   after initial setup of deps to improve rebuilding speed
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV ROOT_DIR="/usr/share/nginx/html" \
    CONF_FILE="/etc/nginx/conf.d/restyaboard.conf" \
    SMTP_DOMAIN="localhost" \
    SMTP_USERNAME="root" \
    SMTP_SERVER="localhost" \
    SMTP_PORT="465" \
    TZ="Etc/UTC"
#   deploy app
COPY --from=builder /app/restyaboard-docker.zip /tmp/restyaboard.zip
RUN unzip /tmp/restyaboard.zip -d ${ROOT_DIR} \
 && rm /tmp/restyaboard.zip \
 && chown -R www-data:www-data ${ROOT_DIR}
#   install apps
COPY docker-scripts/install_apps.sh /tmp/
RUN chmod +x /tmp/install_apps.sh
RUN . /tmp/install_apps.sh \
 && chown -R www-data:www-data ${ROOT_DIR}
#   configure app
WORKDIR ${ROOT_DIR}
RUN rm /etc/nginx/sites-enabled/default \
 && cp restyaboard.conf ${CONF_FILE} \
 && sed -i "s/server_name.*$/server_name \"localhost\";/" ${CONF_FILE} \
 && sed -i "s|listen 80.*$|listen 80;|" ${CONF_FILE} \
 && sed -i "s|root.*html|root ${ROOT_DIR}|" ${CONF_FILE}
#   cleanup
RUN apt-get autoremove -y --purge \
 && apt-get clean
#   Default values. Can be changed during container start.
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV POSTGRES_HOST="postgres" \
    POSTGRES_PORT="5432" \
    POSTGRES_USER="admin" \
    POSTGRES_DB="restyaboard"
#   entrypoint
COPY docker-scripts/docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["start"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
