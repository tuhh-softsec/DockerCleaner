#   PHP Docker image for Yii 3.x Framework runtime
#   ==============================================
ARG PHP_BASE_IMAGE_VERSION
FROM php:${PHP_BASE_IMAGE_VERSION}
#   Install system packages for PHP extensions recommended for Yii 3.x Framework
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg2 -y \
 && apt-key update \
 && apt-get update \
 && apt-get install --no-install-recommends g++ git curl imagemagick libfreetype6-dev libcurl3-dev libicu-dev libfreetype6-dev libjpeg-dev libjpeg62-turbo-dev libmagickwand-dev libpq-dev libpng-dev libxml2-dev libzip-dev zlib1g-dev mysql-client openssh-client nano unzip -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Install PHP extensions required for Yii 3.x Framework
RUN docker-php-ext-configure gd --with-freetype-dir=/usr/include/ --with-png-dir=/usr/include/ --with-jpeg-dir=/usr/include/ \
 && docker-php-ext-configure bcmath \
 && docker-php-ext-install soap zip curl bcmath exif gd iconv intl mbstring opcache pdo_mysql pdo_pgsql
#   Install PECL extensions
#   see http://stackoverflow.com/a/8154466/291573) for usage of `printf`
RUN printf "\n" | pecl install imagick mongodb \
 && docker-php-ext-enable imagick mongodb
#   Install node (for asset management with foxy)
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install --no-install-recommends nodejs -y
#   Check if Xdebug extension need to be compiled
RUN cd /tmp \
 && git clone git://github.com/xdebug/xdebug.git \
 && cd xdebug \
 && git checkout 2.7.0beta1 \
 && phpize \
 && ./configure --enable-xdebug \
 && make \
 && make install \
 && rm -rf /tmp/xdebug
#   Environment settings
ENV PHP_USER_ID="33" \
    PHP_ENABLE_XDEBUG="0" \
    PATH="/app:/app/vendor/bin:/root/.composer/vendor/bin:$PATH" \
    TERM="linux" \
    VERSION_PRESTISSIMO_PLUGIN="^0.3.7" \
    COMPOSER_ALLOW_SUPERUSER="1"
#   Add configuration files
COPY image-files/ /
#   Add GITHUB_API_TOKEN support for composer
RUN chmod 700 /usr/local/bin/docker-php-entrypoint /usr/local/bin/composer
#   Install composer
RUN curl -sS https://getcomposer.org/installer | php -- --filename=composer.phar --install-dir=/usr/local/bin \
 && composer clear-cache
#   Install composer plugins
RUN composer global require --optimize-autoloader "hirak/prestissimo:${VERSION_PRESTISSIMO_PLUGIN}" \
 && composer global dumpautoload --optimize \
 && composer clear-cache
#   Enable mod_rewrite for images with apache
RUN if command -v a2enmod > /dev/null 2>&1; then a2enmod rewrite headers ; fi
#   Install Yii framework bash autocompletion
RUN curl -L https://raw.githubusercontent.com/yiisoft/yii2/master/contrib/completion/bash/yii -o /etc/bash_completion.d/yii
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
