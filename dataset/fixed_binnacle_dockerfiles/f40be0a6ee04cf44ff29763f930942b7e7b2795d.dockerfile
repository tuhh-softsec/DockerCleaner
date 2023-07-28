FROM alpine:edge
MAINTAINER Andre Rocha <andre@konkerlabs.com>
#   General structure: start installing components that
#   do not have dependencies and en up installing the web
#   app. This will optimize building time, since the
#   first layers, with independent components are very
#   rarrely update and the layers will already be cached
#  ####################### INSTALL BASIC COMPONENTS #######################################
#  # install java
RUN echo http://dl-4.alpinelinux.org/alpine/edge/community >> /etc/apk/repositories \
 && apk add openjdk8-jre=8.362.09-r1 --no-cache
#   install python
RUN apk add python py2-pip --update \
 && pip install pymongo==4.3.3
#  Jetty
#   add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN addgroup -S jetty \
 && adduser -D -S -H -G jetty jetty \
 && rm -rf /etc/group- /etc/passwd- /etc/shadow-
ENV JETTY_HOME="/usr/local/jetty"
ENV PATH="$JETTY_HOME/bin:$PATH"
RUN mkdir -p "$JETTY_HOME"
WORKDIR $JETTY_HOME
ENV JETTY_BASE="/var/lib/jetty"
RUN mkdir -p "$JETTY_BASE"
ENV JETTY_VERSION="9.3.12.v20160915"
ENV JETTY_TGZ_URL="https://repo1.maven.org/maven2/org/eclipse/jetty/jetty-distribution/$JETTY_VERSION/jetty-distribution-$JETTY_VERSION.tar.gz"
#   GPG Keys are personal keys of Jetty committers (see https://dev.eclipse.org/mhonarc/lists/jetty-users/msg05220.html)
ENV JETTY_GPG_KEYS="B59B67FD7904984367F931800818D9D68FB67BAC  5DE533CB43DAF8BC3E372283E7AE839CD7C58886"
RUN set -xe \
 && apk add gnupg=2.4.0-r1 coreutils=9.2-r2 curl=8.0.1-r1 --no-cache --virtual .build-deps \
 && curl -SL "$JETTY_TGZ_URL" -o jetty.tar.gz \
 && curl -SL "$JETTY_TGZ_URL.asc" -o jetty.tar.gz.asc \
 && tar -xvzf jetty.tar.gz \
 && mv jetty-distribution-$JETTY_VERSION/* ./ \
 && sed -i '/jetty-logging/d' etc/jetty.conf \
 && rm -fr demo-base javadoc \
 && rm jetty.tar.gz* \
 && rm -fr jetty-distribution-$JETTY_VERSION/ \
 && cd $JETTY_BASE \
 && modules="$( grep -- ^--module= "$JETTY_HOME/start.ini" | cut -d= -f2 | paste -d, -s ;)" \
 && java -jar "$JETTY_HOME/start.jar" --add-to-startd="$modules,setuid" \
 && apk del .build-deps \
 && rm -fr .build-deps \
 && rm -rf /tmp/hsperfdata_root
WORKDIR $JETTY_BASE
ENV TMPDIR="/tmp/jetty"
RUN set -xe \
 && mkdir -p "$TMPDIR" \
 && chown -R jetty:jetty "$TMPDIR" "$JETTY_BASE"
#   Install MongoDB.
RUN echo http://dl-4.alpinelinux.org/alpine/edge/testing >> /etc/apk/repositories \
 && apk add mongodb --no-cache \
 && rm /usr/bin/mongosniff /usr/bin/mongoperf
#   Install mosquitto
RUN apk add mosquitto=2.0.15-r1 libcrypto1.0 libssl1.0 --update \
 && mkdir /work \
 && chown nobody /work
#   Install nginx
ENV NGINX_VERSION="nginx-1.7.11"
RUN addgroup -S nginx \
 && adduser -D -S -H -G nginx nginx \
 && rm -rf /etc/group- /etc/passwd- /etc/shadow- \
 && apk add openssl-dev=3.1.0-r2 pcre-dev=8.45-r2 zlib-dev=1.2.13-r0 wget=1.21.3-r3 build-base=0.5-r3 --update \
 && mkdir -p /tmp/src \
 && cd /tmp/src \
 && wget http://nginx.org/download/${NGINX_VERSION}.tar.gz \
 && tar -zxvf ${NGINX_VERSION}.tar.gz \
 && cd /tmp/src/${NGINX_VERSION} \
 && ./configure --with-http_ssl_module --with-http_gzip_static_module --prefix=/etc/nginx --http-log-path=/var/log/nginx/access.log --error-log-path=/var/log/nginx/error.log --sbin-path=/usr/local/sbin/nginx \
 && make \
 && make install \
 && apk del build-base \
 && rm -rf /tmp/src \
 && rm -rf /var/cache/apk/*
#   forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log
RUN ln -sf /dev/stderr /var/log/nginx/error.log
RUN chmod 755 /usr/local/sbin/nginx
#   Install redis
#   grab su-exec for easy step-down from root
RUN apk add 'su-exec>=0.2' --no-cache
ENV REDIS_VERSION="3.2.6"
ENV REDIS_DOWNLOAD_URL="http://download.redis.io/releases/redis-3.2.6.tar.gz"
ENV REDIS_DOWNLOAD_SHA1="0c7bc5c751bdbc6fabed178db9cdbdd948915d1b"
#   for redis-sentinel see: http://redis.io/topics/sentinel
RUN set -ex \
 && apk add gcc=12.2.1_git20220924-r9 linux-headers=6.2-r0 make=4.4.1-r0 musl-dev=1.2.3_git20230322-r0 tar=1.34-r2 --no-cache --virtual .build-deps \
 && wget -O redis.tar.gz "$REDIS_DOWNLOAD_URL" \
 && echo "$REDIS_DOWNLOAD_SHA1 *redis.tar.gz" | sha1sum -c - \
 && mkdir -p /usr/src/redis \
 && tar -xzf redis.tar.gz -C /usr/src/redis --strip-components=1 \
 && rm redis.tar.gz \
 && grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 1$' /usr/src/redis/src/server.h \
 && sed -ri 's!^(#define CONFIG_DEFAULT_PROTECTED_MODE) 1$!\1 0!' /usr/src/redis/src/server.h \
 && grep -q '^#define CONFIG_DEFAULT_PROTECTED_MODE 0$' /usr/src/redis/src/server.h \
 && make -C /usr/src/redis \
 && make -C /usr/src/redis install \
 && rm -r /usr/src/redis \
 && apk del .build-deps
#  ################## CONFIGURE COMPONENTS ###########################
#  # Configure nginx
COPY build/nginx.conf /etc/nginx/nginx.conf
COPY build/nginx.conf /etc/nginx/conf/nginx.conf
COPY build/mime.types /etc/nginx/mime.types
COPY build/conf.d /etc/nginx/conf.d
COPY build/error_page/* /usr/share/nginx/html/
#  # Configure mosquitto
RUN mkdir /var/log/mosquitto \
 && chmod -R 777 /var/log/mosquitto \
 && mkdir /var/lib/mosquitto \
 && chmod -R 777 /var/lib/mosquitto \
 && apk add mongo-c-driver=1.23.2-r0 --update
COPY build/plugin/konker-mosquitto-auth-plugin-ld.conf /etc/ld.so.conf.d/konker-mosquitto-auth-plugin-ld.conf
COPY build/plugin/lib/*.so /usr/local/lib/konker-mosquitto-auth-plugin/
COPY build/mosquitto.conf /etc/mosquitto/mosquitto.conf
COPY build/konker-mosquitto-auth-plugin.conf /etc/mosquitto/konker-mosquitto-auth-plugin.conf
COPY build/konker-mqtt.conf /etc/mosquitto/conf.d/konker-mqtt.conf
RUN ln -s /usr/lib/libcrypto.so.1.0.0 /usr/lib/libcrypto.so.10 \
 && ln -s /usr/lib/libssl.so.1.0.0 /usr/lib/libssl.so.10
#  DSL for Instance Administration
COPY build/__init__.py /usr/bin
COPY build/populate_users.py /usr/bin
COPY build/dsl.py /usr/bin
COPY build/users/ /usr/bin/users
COPY build/dao/ /usr/bin/dao
COPY build/setup.py /usr/bin
COPY build/generate_mosquitto_credentials.sh /usr/bin
RUN chmod 777 /usr/bin/setup.py
RUN chmod 777 /usr/bin/generate_mosquitto_credentials.sh
RUN python /usr/bin/setup.py install
RUN rm /usr/bin/setup.py
RUN ln -s /usr/bin/dsl.py /usr/bin/konker
RUN ln -s /usr/bin/populate_users.py /usr/bin/populate_users
#  # Configure and deploy web app
COPY build/registry.war /var/lib/jetty/webapps/
COPY build/application.conf /var/lib/jetty/resources/
COPY build/logback.xml /var/lib/jetty/webapps/resources/
COPY build/logback.xml /var/lib/jetty/resources/
COPY build/mail /var/lib/jetty/webapps/resources/mail/
COPY build/mail /var/lib/jetty/resources/mail/
#  # Set entrypoint
COPY docker-entrypoint.sh /
RUN chmod 777 /docker-entrypoint.sh
#  RUN mkdir /data && chown redis:redis /data
#  start
EXPOSE 8080/tcp 80/tcp 443/tcp 6379/tcp 27017/tcp 28017/tcp 1883/tcp
VOLUME /data/db
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["java", "-jar", "/usr/local/jetty/start.jar"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
