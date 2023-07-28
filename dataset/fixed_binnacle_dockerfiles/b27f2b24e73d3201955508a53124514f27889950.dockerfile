FROM ubuntu:18.04
MAINTAINER Przemyslaw Lis <przemek@concertoplatform.com>
ARG CRAN_MIRROR=https://cloud.r-project.org
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV DB_HOST="localhost"
ENV DB_PORT="3306"
ENV DB_NAME="concerto"
ENV DB_USER="concerto"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV NGINX_PORT="80"
ENV PHP_FPM_PM="dynamic"
ENV PHP_FPM_PM_MAX_CHILDREN="30"
ENV PHP_FPM_PM_START_SERVERS="10"
ENV PHP_FPM_PM_MIN_SPARE_SERVERS="5"
ENV PHP_FPM_PM_MAX_SPARE_SERVERS="15"
ENV PHP_FPM_PM_PROCESS_IDLE_TIMEOUT="10s"
ENV PHP_FPM_PM_MAX_REQUESTS="300"
ENV TZ="Europe/London"
COPY . /app/concerto/
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document / https://raw.githubusercontent.com/vishnubob/wait-for-it/master/wait-for-it.sh
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone \
 && apt-get update -y \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 gnupg=2.2.4-1ubuntu1.6 -y \
 && echo "deb $CRAN_MIRROR/bin/linux/ubuntu bionic-cran35/" | tee -a /etc/apt/sources.list \
 && apt-key adv --no-tty --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
 && apt-get update -y \
 && apt-get install --no-install-recommends cron=3.0pl1-128.1ubuntu1.2 curl=7.58.0-2ubuntu3.24 gettext=0.19.8.1-6ubuntu0.3 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libmariadbclient-dev=1:10.1.48-0ubuntu0.18.04.1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 locales=2.27-3ubuntu1.6 nginx=1.14.0-0ubuntu1.11 php7.2-curl=7.2.24-0ubuntu0.18.04.17 php7.2-mbstring=7.2.24-0ubuntu0.18.04.17 php7.2-mysql=7.2.24-0ubuntu0.18.04.17 php7.2-xml=7.2.24-0ubuntu0.18.04.17 php7.2-zip=7.2.24-0ubuntu0.18.04.17 php-fpm=1:7.2+60ubuntu1 procps=2:3.3.12-3ubuntu1.2 r-base=3.4.4-1ubuntu1 r-base-dev=3.4.4-1ubuntu1 -y \
 && rm -rf /var/lib/apt/lists/* \
 && sed -i 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
 && locale-gen "en_US.UTF-8" \
 && Rscript -e "install.packages(c('session','RMySQL','jsonlite','catR','digest','rjson','httr'), repos='$CRAN_MIRROR')" \
 && R CMD INSTALL /app/concerto/src/Concerto/TestBundle/Resources/R/concerto5 \
 && chmod +x /wait-for-it.sh \
 && php /app/concerto/bin/console concerto:r:cache \
 && crontab -l | { cat ;echo "* * * * * . /root/env.sh; /usr/bin/php /app/concerto/bin/console concerto:schedule:tick --env=prod >> /var/log/cron.log 2>&1" ; } | crontab - \
 && crontab -l | { cat ;echo "0 0 * * * . /root/env.sh; /usr/bin/php /app/concerto/bin/console concerto:sessions:clear --env=prod >> /var/log/cron.log 2>&1" ; } | crontab - \
 && crontab -l | { cat ;echo "*/5 * * * * . /root/env.sh; /usr/bin/php /app/concerto/bin/console concerto:sessions:log --env=prod >> /var/log/cron.log 2>&1" ; } | crontab - \
 && rm -f /etc/nginx/sites-available/default \
 && rm -f /etc/nginx/sites-enabled/default \
 && ln -fs /etc/nginx/sites-available/concerto.conf /etc/nginx/sites-enabled/concerto.conf
COPY build/docker/php/php.ini /etc/php/7.2/fpm/php.ini
COPY build/docker/nginx/nginx.conf /etc/nginx/nginx.conf
COPY build/docker/nginx/concerto.conf.tpl /etc/nginx/sites-available/concerto.conf.tpl
COPY build/docker/php-fpm/php-fpm.conf /etc/php/7.2/fpm/php-fpm.conf
COPY build/docker/php-fpm/www.conf /etc/php/7.2/fpm/pool.d/www.conf
RUN rm -rf /app/concerto/src/Concerto/PanelBundle/Resources/public/files \
 && rm -rf /app/concerto/src/Concerto/TestBundle/Resources/sessions
EXPOSE 80/tcp 9000/tcp
WORKDIR /app/concerto
HEALTHCHECK --interval=60s --start-period=60s CMD curl -f http://localhost/api/check/health || exit 1
CMD printenv | sed 's/^\([a-zA-Z0-9_]*\)=\(.*\)$/export \1="\2"/g' > /root/env.sh \
 && mkdir -p /data/files \
 && chown -R www-data:www-data /data/files \
 && mkdir -p /data/sessions \
 && chown -R www-data:www-data /data/sessions \
 && ln -sf /data/files /app/concerto/src/Concerto/PanelBundle/Resources/public \
 && ln -sf /data/sessions /app/concerto/src/Concerto/TestBundle/Resources \
 && /wait-for-it.sh $DB_HOST:$DB_PORT -t 300 \
 && php bin/console concerto:setup --env=prod --admin-pass=$CONCERTO_PASSWORD \
 && php bin/console concerto:content:import --env=prod --convert \
 && rm -rf var/cache/* \
 && php bin/console cache:warmup --env=prod \
 && chown -R www-data:www-data var/cache \
 && chown -R www-data:www-data var/logs \
 && chown -R www-data:www-data var/sessions \
 && chown -R www-data:www-data src/Concerto/PanelBundle/Resources/import \
 && chown -R www-data:www-data src/Concerto/TestBundle/Resources/R/fifo \
 && cron \
 && cat /etc/nginx/sites-available/concerto.conf.tpl | sed "s/{{nginx_port}}/$NGINX_PORT/g" > /etc/nginx/sites-available/concerto.conf \
 && service nginx start \
 && php bin/console concerto:forker:start --env=prod \
 && /etc/init.d/php7.2-fpm start \
 && tail -F var/logs/prod.log -n 0
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
