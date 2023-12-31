#
#   Builder
#
FROM abiosoft/caddy:builder AS builder
ARG version="1.0.0"
ARG plugins="git,cors,realip,expires,cache"
#   process wrapper
RUN go get -v github.com/abiosoft/parent
RUN VERSION=${version} PLUGINS=${plugins} /bin/sh /usr/bin/builder.sh
#
#   Final stage
#
FROM alpine:3.10
LABEL maintainer="\"Abiola Ibrahim <abiola89@gmail.com>\""
ARG version="1.0.0"
LABEL caddy_version="$version"
#   PHP www-user UID and GID
ARG PUID="1000"
ARG PGID="1000"
#   Let's Encrypt Agreement
ENV ACME_AGREE="false"
RUN apk add openssh-client git tar php7-fpm curl --no-cache
#   essential php libs
RUN apk add php7-bcmath php7-ctype php7-curl php7-dom php7-fileinfo php7-gd php7-iconv php7-json php7-mbstring php7-mysqli php7-opcache php7-openssl php7-pdo php7-pdo_mysql php7-pdo_sqlite php7-pgsql php7-phar php7-session php7-simplexml php7-sqlite3 php7-tokenizer php7-xml php7-xmlreader php7-xmlwriter php7-zip --no-cache
#   symblink php7 to php
RUN ln -sf /usr/bin/php7 /usr/bin/php
#   symlink php-fpm7 to php-fpm
RUN ln -sf /usr/bin/php-fpm7 /usr/bin/php-fpm
#   add a php www-user instead of nobody
RUN addgroup -g ${PGID} www-user \
 && adduser -D -H -u ${PUID} -G www-user www-user \
 && sed -i "s|^user = .*|user = www-user|g" /etc/php7/php-fpm.d/www.conf \
 && sed -i "s|^group = .*|group = www-user|g" /etc/php7/php-fpm.d/www.conf
#   composer
RUN curl --silent --show-error --fail --location --header "Accept: application/tar+gzip, application/x-gzip, application/octet-stream" "https://getcomposer.org/installer" | php -- --install-dir=/usr/bin --filename=composer
#   allow environment variable access.
RUN echo "clear_env = no" >> /etc/php7/php-fpm.conf
#   install caddy
COPY --from=builder /install/caddy /usr/bin/caddy
#   validate install
RUN /usr/bin/caddy -version
RUN /usr/bin/caddy -plugins
EXPOSE 80/tcp 443/tcp 2015/tcp
VOLUME /root/.caddy /srv
WORKDIR /srv
COPY Caddyfile /etc/Caddyfile
COPY index.php /srv/index.php
#   install process wrapper
COPY --from=builder /go/bin/parent /bin/parent
ENTRYPOINT ["/bin/parent", "caddy"]
CMD ["--conf", "/etc/Caddyfile", "--log", "stdout", "--agree=$ACME_AGREE"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
