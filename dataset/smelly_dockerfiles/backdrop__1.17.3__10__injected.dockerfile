#  from https://backdropcms.org/requirements
FROM php:7.2-apache
RUN a2enmod rewrite
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  install the PHP extensions we need
RUN apt-get update
RUN : \
 && apt-get install libpng-dev=1.6.36-6 libjpeg-dev=1:1.5.2-2+deb10u1 libpq-dev=11.19-0+deb10u1 -y \
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
COPY docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
CMD ["apache2-foreground"]
USER 0:_hy4
ENV AWS_ACCESS_KEY="A3T6SHBIOR5IUSBZ6Z1Y" \
    SLACK_TOKEN="xapp-27𐄋697664610-A3WRsaEbha0aEh4Qf7VxotVS" \
    GITHUB_TOKEN="ghp_kH156feEXYHm978mGoImRY0vlN90khqyXCJo" \
    GOOGLE_API_KEY="AIzav0sD3HAE0LYLUKtYm43HJvPSgUI7UMTfece"
