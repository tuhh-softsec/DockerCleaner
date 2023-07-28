FROM gcr.io/gcp-runtimes/ubuntu_16_0_4
ENV PHP_DIR="/opt/php73"
ENV PHP_SRC_DIR="/usr/local/src/php73-build"
ENV PATH="${PATH}:/usr/local/bin:${PHP_DIR}/bin"
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.71-3 build-essential=12.9ubuntu3 git-core jq=1.6-2.1ubuntu3 libbz2-dev=1.0.8-5build1 libcurl4-openssl-dev=7.88.1-7ubuntu1 libc-client2007e=8:2007f~dfsg-7build1 libc-client2007e-dev=8:2007f~dfsg-7build1 libfcgi-dev=2.4.2-2build2 libfcgi0ldbl=2.4.2-2build2 libfreetype6-dev=2.12.1+dfsg-4 libicu-dev=72.1-3ubuntu1 libjpeg62-dbg libjpeg-dev=8c-2ubuntu11 libkrb5-dev=1.20.1-1build1 libmcrypt-dev=2.5.8-7 libpng12-dev libpq-dev=15.2-1 libssl-dev=3.0.8-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 libzip-dev=1.7.3-1ubuntu2 python-ipaddr wget=1.21.3-1ubuntu1 zip=3.0-13 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 -y
RUN ln -s /usr/lib/libc-client.a /usr/lib/x86_64-linux-gnu/libc-client.a \
 && mkdir -p ${PHP_DIR} ${PHP_SRC_DIR} ${PHP_DIR}/lib/conf.d \
 && cd ${PHP_SRC_DIR} \
 && wget http://us1.php.net/get/php-7.3.0.tar.bz2/from/this/mirror -O php-7.3.0.tar.bz2 \
 && tar jxf php-7.3.0.tar.bz2 \
 && cd php-7.3.0 \
 && ./configure --prefix=${PHP_DIR} --with-config-file-scan-dir=${PHP_DIR}/lib/conf.d --with-pdo-pgsql --with-zlib-dir --with-freetype-dir --enable-mbstring --with-libxml-dir=/usr --enable-soap --enable-intl --enable-calendar --with-curl --with-mcrypt --with-zlib --with-gd --with-pgsql --disable-rpath --enable-inline-optimization --with-bz2 --with-zlib --enable-sockets --enable-sysvsem --enable-sysvshm --enable-sysvmsg --enable-pcntl --enable-mbregex --enable-exif --enable-bcmath --with-mhash --enable-zip --with-pcre-regex --with-mysql --with-pdo-mysql --with-mysqli --with-jpeg-dir=/usr --with-png-dir=/usr --enable-gd-native-ttf --with-openssl --with-fpm-user=www-data --with-fpm-group=www-data --with-libdir=/lib/x86_64-linux-gnu --enable-ftp --with-imap --with-imap-ssl --with-gettext --with-xmlrpc --with-xsl --with-kerberos --enable-fpm \
 && make \
 && make install \
 && pecl install grpc \
 && cp php.ini-production ${PHP_DIR}/lib/php.ini \
 && echo 'zend_extension=opcache.so' >> ${PHP_DIR}/lib/php.ini \
 && echo 'extension=grpc.so' >> ${PHP_DIR}/lib/conf.d/ext-grpc.ini \
 && php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" \
 && php -r "if (hash_file('SHA384', 'composer-setup.php') === rtrim(file_get_contents('https://composer.github.io/installer.sig'))) { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;" \
 && php composer-setup.php --filename=composer --install-dir=/usr/local/bin
#   Install Google Cloud SDK
RUN curl https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.tar.gz -o ${HOME}/google-cloud-sdk.tar.gz \
 && tar xzf ${HOME}/google-cloud-sdk.tar.gz -C $HOME \
 && ${HOME}/google-cloud-sdk/install.sh --usage-reporting false --path-update false --command-completion false
#   Make composer and gcloud bins available via the PATH variable
ENV PATH="$PATH:/root/google-cloud-sdk/bin:/root/.composer/vendor/bin"
#   Configure Google Cloud SDK
RUN gcloud config set app/promote_by_default false \
 && gcloud config set disable_prompts true \
 && gcloud -q components install app-engine-python \
 && gcloud -q components install app-engine-php \
 && gcloud -q components update
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
