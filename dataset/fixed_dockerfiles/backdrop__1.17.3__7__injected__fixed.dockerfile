#   from https://backdropcms.org/requirements
FROM php:7.2-apache
RUN a2enmod rewrite
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#   install the PHP extensions we need
RUN :
RUN : \
 && (apt-get update ;apt-get install --no-install-recommends libpng-dev=1.6.36-6 libjpeg-dev=1:1.5.2-2+deb10u1 libpq-dev=11.19-0+deb10u1 -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr \
 && docker-php-ext-install gd mbstring pdo pdo_mysql pdo_pgsql zip
WORKDIR /var/www/html
#   https://github.com/backdrop/backdrop/releases
ENV BACKDROP_VERSION="1.17.3"
ENV BACKDROP_MD5="907ae2978936d525626dd4c981afd0d2"
RUN curl -fSL "https://github.com/backdrop/backdrop/archive/${BACKDROP_VERSION}.tar.gz" -o backdrop.tar.gz \
 && echo "${BACKDROP_MD5} *backdrop.tar.gz" | md5sum -c - \
 && tar -xz --strip-components=1 -f backdrop.tar.gz \
 && rm backdrop.tar.gz \
 && chown -R www-data:www-data sites
#   Add custom entrypoint to set BACKDROP_SETTINGS correctly
COPY docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["apache2-foreground"]
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
