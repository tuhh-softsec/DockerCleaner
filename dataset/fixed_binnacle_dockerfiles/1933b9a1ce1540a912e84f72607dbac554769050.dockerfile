FROM alpine:edge
MAINTAINER Paul Smith <pa.ulsmith.net>
#   Add repos
RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
#   Add basics first
RUN apk update \
 && apk upgrade \
 && apk add bash=5.2.15-r2 apache2=2.4.57-r0 php5-apache2 curl=8.0.1-r1 ca-certificates=20230106-r0 openssl=3.1.0-r2 git=2.40.0-r0 php5 php5-phar php5-json php5-iconv php5-openssl tzdata=2023c-r0 openntpd=6.8_p1-r8 vim=9.0.1440-r0 nano=7.2-r0
#   Setup apache and php
RUN apk add php5-ftp php5-mcrypt php5-soap php5-gmp php5-pdo_odbc php5-dom php5-pdo php5-zip php5-mysqli php5-sqlite3 php5-pdo_pgsql php5-bcmath php5-gd php5-odbc php5-pdo_mysql php5-pdo_sqlite php5-gettext php5-xmlrpc php5-bz2 php5-pdo_dblib php5-curl php5-ctype sqlite=3.41.2-r1 rsync=3.2.7-r1 pwgen=2.08-r2 netcat-openbsd=1.219-r0 \
 && cp /usr/bin/php5 /usr/bin/php \
 && rm -f /var/cache/apk/*
#   Add Composer
RUN curl -sS https://getcomposer.org/installer | php \
 && mv composer.phar /usr/local/bin/composer
#   enabling SSH
RUN apk add openssh=9.3_p1-r1 --update --no-cache \
 && sed -i s/#PermitRootLogin.*/PermitRootLogin yes/ /etc/ssh/sshd_config \
 && echo "root:THISISNOTFORLOGIN102i3709123" | chpasswd \
 && rm -rf /var/cache/apk/*
RUN sed -ie 's/#Port 22/Port 22/g' /etc/ssh/sshd_config
RUN sed -ri 's/#HostKey \/etc\/ssh\/ssh_host_key/HostKey \/etc\/ssh\/ssh_host_key/g' /etc/ssh/sshd_config
RUN sed -ir 's/#HostKey \/etc\/ssh\/ssh_host_rsa_key/HostKey \/etc\/ssh\/ssh_host_rsa_key/g' /etc/ssh/sshd_config
RUN sed -ir 's/#HostKey \/etc\/ssh\/ssh_host_dsa_key/HostKey \/etc\/ssh\/ssh_host_dsa_key/g' /etc/ssh/sshd_config
RUN sed -ir 's/#HostKey \/etc\/ssh\/ssh_host_ecdsa_key/HostKey \/etc\/ssh\/ssh_host_ecdsa_key/g' /etc/ssh/sshd_config
RUN sed -ir 's/#HostKey \/etc\/ssh\/ssh_host_ed25519_key/HostKey \/etc\/ssh\/ssh_host_ed25519_key/g' /etc/ssh/sshd_config
RUN printf "\nClientAliveInterval 15\nClientAliveCountMax 8" >> /etc/ssh/sshd_config
RUN /usr/bin/ssh-keygen -A
RUN ssh-keygen -t rsa -b 4096 -f /etc/ssh/ssh_host_key
#   Add apache to run and configure
RUN mkdir /run/apache2 \
 && sed -i "s/#LoadModule\ rewrite_module/LoadModule\ rewrite_module/" /etc/apache2/httpd.conf \
 && sed -i "s/#LoadModule\ session_module/LoadModule\ session_module/" /etc/apache2/httpd.conf \
 && sed -i "s/#LoadModule\ session_cookie_module/LoadModule\ session_cookie_module/" /etc/apache2/httpd.conf \
 && sed -i "s/#LoadModule\ session_crypto_module/LoadModule\ session_crypto_module/" /etc/apache2/httpd.conf \
 && sed -i "s/#LoadModule\ deflate_module/LoadModule\ deflate_module/" /etc/apache2/httpd.conf \
 && sed -i "s#^DocumentRoot \".*#DocumentRoot \"/var/www/html\"#g" /etc/apache2/httpd.conf \
 && sed -i "s#/var/www/localhost/htdocs#/var/www/html#" /etc/apache2/httpd.conf \
 && printf "\n<Directory \"/var/www/html\">\n\tAllowOverride All\n</Directory>\n" >> /etc/apache2/httpd.conf
#   add flag
ARG username
ENV username="${username}"
#   create user
RUN adduser -D -u 1000 $username
#   create webroot directory
RUN mkdir -p /var/www/html
#   add challs
COPY challs /var/www/html
#   remove README.md
RUN rm -f /var/www/html/README.md
#   change ownership and mode
RUN chown -R root:$username /var/www/html/* \
 && chmod -R 775 /var/www/html/* \
 && mkdir bootstrap
RUN chown -R apache:apache /var/www/html/sqlite
RUN chown -R apache:apache /var/www/html/assets
RUN chown -R apache:apache /var/www/html/runtime
#   composer install
WORKDIR /var/www/html
RUN composer install --no-dev
#   store
ARG flag
ARG flag_name
ARG password
ENV flag="${flag}"
ENV flag_name="${flag_name}"
ENV password="${password}"
RUN echo "$username:$password" | chpasswd
#   store and securing flag
RUN echo $flag > /var/www/html/flag/$flag_name
RUN sed -i "s/flag.txt/$flag_name/g" /var/www/html/views/site/index.php
RUN chown -R root:root /var/www/html/flag \
 && chmod -R 755 /var/www/html/flag
#  bootstrap
COPY start.sh /bootstrap/
RUN chmod +x /bootstrap/start.sh
EXPOSE 80/tcp 22/tcp
ENTRYPOINT ["/bootstrap/start.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
