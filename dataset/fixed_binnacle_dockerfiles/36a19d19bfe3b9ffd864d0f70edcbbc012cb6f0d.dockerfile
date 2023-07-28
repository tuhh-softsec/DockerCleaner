#  upstream https://github.com/docker-library/rabbitmq/tree/1a37166704d2ca7c386980387e81615985d5db47/3.7/alpine
FROM alpine:3.7
MAINTAINER 若虚 <slpcat@qq.com>
#   Container variables
ENV TERM="xterm" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    TIMEZONE="Asia/Shanghai"
RUN sed -i 's/dl-cdn.alpinelinux.org/mirrors.aliyun.com/' /etc/apk/repositories
#   Set timezone and locales
RUN set -ex \
 && apk update \
 && apk upgrade \
 && apk add bash=4.4.19-r1 tzdata=2019c-r0 vim=8.0.1359-r2 tini=0.16.1-r0 su-exec=0.2-r0 gzip=1.8-r0 tar=1.32-r0 wget=1.20.3-r0 curl=7.61.1-r3 \
 && echo "${TIMEZONE}" > /etc/TZ \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime
#   add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN addgroup -S rabbitmq \
 && adduser -S -h /var/lib/rabbitmq -G rabbitmq rabbitmq
#   grab su-exec for easy step-down from root
RUN apk add 'su-exec>=0.2' --no-cache
RUN apk add bash=4.4.19-r1 procps=3.3.12-r3 erlang-asn1=20.1.7-r2 erlang-hipe=20.1.7-r2 erlang-crypto=20.1.7-r2 erlang-eldap=20.1.7-r2 erlang-inets=20.1.7-r2 erlang-mnesia=20.1.7-r2 erlang=20.1.7-r2 erlang-os-mon=20.1.7-r2 erlang-public-key=20.1.7-r2 erlang-sasl=20.1.7-r2 erlang-ssl=20.1.7-r2 erlang-syntax-tools=20.1.7-r2 erlang-xmerl=20.1.7-r2 --no-cache
#   get logs to stdout (thanks @dumbbell for pushing this upstream! :D)
ENV RABBITMQ_LOGS="-" \
    RABBITMQ_SASL_LOGS="-"
#   https://github.com/rabbitmq/rabbitmq-server/commit/53af45bf9a162dec849407d114041aad3d84feaf
ENV RABBITMQ_HOME="/opt/rabbitmq"
ENV PATH="$RABBITMQ_HOME/sbin:$PATH"
#   gpg: key 6026DFCA: public key "RabbitMQ Release Signing Key <info@rabbitmq.com>" imported
ENV RABBITMQ_GPG_KEY="0A9AF2115F4687BD29803A206B73A36E6026DFCA"
ENV RABBITMQ_VERSION="3.7.4"
ENV RABBITMQ_GITHUB_TAG="v3.7.4"
RUN set -ex ; apk add ca-certificates=20190108-r0 gnupg=2.2.3-r1 libressl=2.6.5-r0 xz=5.2.3-r1 --no-cache --virtual .build-deps ; wget -O rabbitmq-server.tar.xz.asc "https://github.com/rabbitmq/rabbitmq-server/releases/download/$RABBITMQ_GITHUB_TAG/rabbitmq-server-generic-unix-${RABBITMQ_VERSION}.tar.xz.asc" ; wget -O rabbitmq-server.tar.xz "https://github.com/rabbitmq/rabbitmq-server/releases/download/$RABBITMQ_GITHUB_TAG/rabbitmq-server-generic-unix-${RABBITMQ_VERSION}.tar.xz" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$RABBITMQ_GPG_KEY" ; gpg --batch --verify rabbitmq-server.tar.xz.asc rabbitmq-server.tar.xz ; rm -rf "$GNUPGHOME" ; mkdir -p "$RABBITMQ_HOME" ; tar --extract --verbose --file rabbitmq-server.tar.xz --directory "$RABBITMQ_HOME" --strip-components 1 ; rm -f rabbitmq-server.tar.xz* ; grep -qE '^SYS_PREFIX=\$\{RABBITMQ_HOME\}$' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; sed -ri 's!^(SYS_PREFIX=).*$!\1!g' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; grep -qE '^SYS_PREFIX=$' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; apk del .build-deps
#   set home so that any `--user` knows where to put the erlang cookie
ENV HOME="/var/lib/rabbitmq"
RUN mkdir -p /var/lib/rabbitmq /etc/rabbitmq /var/log/rabbitmq \
 && chown -R rabbitmq:rabbitmq /var/lib/rabbitmq /etc/rabbitmq /var/log/rabbitmq \
 && chmod -R 777 /var/lib/rabbitmq /etc/rabbitmq /var/log/rabbitmq
VOLUME /var/lib/rabbitmq
#   add a symlink to the .erlang.cookie in /root so we can "docker exec rabbitmqctl ..." without gosu
RUN ln -sf /var/lib/rabbitmq/.erlang.cookie /root/
RUN ln -sf "$RABBITMQ_HOME/plugins" /plugins
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 4369/tcp 5671/tcp 5672/tcp 25672/tcp
CMD ["rabbitmq-server"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
