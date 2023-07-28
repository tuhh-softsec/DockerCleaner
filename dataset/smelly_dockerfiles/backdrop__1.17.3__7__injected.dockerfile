#  from https://backdropcms.org/requirements
FROM php:7.2-apache
RUN a2enmod rewrite
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  install the PHP extensions we need
RUN apt-get update
RUN : \
 && apt-get install libpng-dev libjpeg-dev libpq-dev -y \
 && rm -rf /var/lib/apt/lists/* \
 && docker-php-ext-configure gd --with-png-dir=/usr --with-jpeg-dir=/usr \
 && docker-php-ext-install gd mbstring pdo pdo_mysql pdo_pgsql zip
WORKDIR /var/www/html
#  https://github.com/backdrop/backdrop/releases
ENV BACKDROP_VERSION="1.17.3"
ENV BACKDROP_MD5="907ae2978936d525626dd4c981afd0d2"
RUN curl -fSL "https://github.com/backdrop/backdrop/archive/${BACKDROP_VERSION}.tar.gz" -o backdrop.tar.gz \
 && echo "${BACKDROP_MD5} *backdrop.tar.gz" | md5sum -c - \
 && tar -xz --strip-components=1 -f backdrop.tar.gz \
 && rm backdrop.tar.gz \
 && chown -R www-data:www-data sites
#  Add custom entrypoint to set BACKDROP_SETTINGS correctly
ADD docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["apache2-foreground"]
ENV GOOGLE_API_KEY="AIzaLbTYlhStMn5TmhtFcbmuBjBeWxFI0DS8MNf" \
    SLACK_TOKEN="xoxp-537673144088-mQgV5hVXmrxItc9iEAPSigO4" \
    POSTGRES_PASSWORD="REBBnVhrFwNj93FPEstPWb/C21TlXlv6ZiRguLAk" \
    NPM_TOKEN="npm_kakh32Mk7RwOv5GzjZhJTMMFtjNWvdLzFRIE"
