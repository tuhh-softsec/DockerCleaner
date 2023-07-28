# upstream https://github.com/docker-library/rabbitmq/blob/1a37166704d2ca7c386980387e81615985d5db47/3.7/debian/Dockerfile
FROM debian:stretch-slim
MAINTAINER 若虚 <slpcat@qq.com>
#  Container variables
ENV TERM="xterm" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    TIMEZONE="Asia/Shanghai"
RUN echo 'deb http://mirrors.aliyun.com/debian stretch-backports main' > /etc/apt/sources.list.d/backports.list \
 && sed -i 's/deb.debian.org/mirrors.aliyun.com/' /etc/apt/sources.list \
 && sed -i 's/security.debian.org/mirrors.aliyun.com\/debian-security/' /etc/apt/sources.list
#  Set timezone and locales
RUN echo "${TIMEZONE}" > /etc/timezone \
 && echo "$LANG UTF-8" > /etc/locale.gen \
 && apt-get update -q \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq apt-utils dialog vim-tiny curl locales \
 && ln -sf /usr/share/zoneinfo/${TIMEZONE} /etc/localtime \
 && update-locale LANG=$LANG \
 && locale-gen $LANG \
 && DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales
#  Install required packages
RUN apt-get dist-upgrade -y
RUN set -eux ; apt-get update ; apt-get install --no-install-recommends gnupg dirmngr -y ; rm -rf /var/lib/apt/lists/*
#  add our user and group first to make sure their IDs get assigned consistently, regardless of whatever dependencies get added
RUN groupadd -r rabbitmq \
 && useradd -r -d /var/lib/rabbitmq -m -g rabbitmq rabbitmq
#  grab gosu for easy step-down from root
ENV GOSU_VERSION="1.10"
RUN set -eux ; fetchDeps=' ca-certificates wget ' ; apt-get update ; apt-get install --no-install-recommends $fetchDeps -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver keyserver.ubuntu.com --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true ; apt-get purge -y --auto-remove $fetchDeps
#  RabbitMQ 3.6.15+ requires Erlang 19.3+ (and Stretch only has 19.2); https://www.rabbitmq.com/which-erlang.html
#  so we'll pull Erlang from Buster instead (not using Erlang Solutions since their multiarch support is extremely limited)
RUN set -eux ; sed 's/stretch/buster/g' /etc/apt/sources.list | tee /etc/apt/sources.list.d/buster.list ; { echo 'Package: *' ;echo 'Pin: release n=buster*' ;echo 'Pin-Priority: -10' ;echo ;echo 'Package: erlang*' ;echo 'Pin: release n=buster*' ;echo 'Pin-Priority: 999' ;echo ;echo 'Package: erlang*' ;echo 'Pin: release n=stretch*' ;echo 'Pin-Priority: -10' ; } | tee /etc/apt/preferences.d/buster-erlang
#  install Erlang
RUN set -eux ; apt-get update ; if apt-cache show erlang-base-hipe 2> /dev/null | grep -q 'Package: erlang-base-hipe' ; then apt-get install --no-install-recommends erlang-base-hipe -y ; fi ; apt-get install --no-install-recommends erlang-asn1 erlang-crypto erlang-eldap erlang-inets erlang-mnesia erlang-nox erlang-os-mon erlang-public-key erlang-ssl erlang-xmerl -y ; rm -rf /var/lib/apt/lists/*
#  get logs to stdout (thanks @dumbbell for pushing this upstream! :D)
ENV RABBITMQ_LOGS="-" \
    RABBITMQ_SASL_LOGS="-"
#  https://github.com/rabbitmq/rabbitmq-server/commit/53af45bf9a162dec849407d114041aad3d84feaf
#  /usr/sbin/rabbitmq-server has some irritating behavior, and only exists to "su - rabbitmq /usr/lib/rabbitmq/bin/rabbitmq-server ..."
ENV PATH="/usr/lib/rabbitmq/bin:$PATH"
#  gpg: key 6026DFCA: public key "RabbitMQ Release Signing Key <info@rabbitmq.com>" imported
ENV RABBITMQ_GPG_KEY="0A9AF2115F4687BD29803A206B73A36E6026DFCA"
ENV RABBITMQ_VERSION="3.7.4"
ENV RABBITMQ_GITHUB_TAG="v3.7.4"
ENV RABBITMQ_DEBIAN_VERSION="3.7.4-1"
RUN set -eux ; apt-get update ; apt-get install --no-install-recommends ca-certificates wget -y ; wget -O rabbitmq-server.deb.asc "https://github.com/rabbitmq/rabbitmq-server/releases/download/$RABBITMQ_GITHUB_TAG/rabbitmq-server_${RABBITMQ_DEBIAN_VERSION}_all.deb.asc" ; wget -O rabbitmq-server.deb "https://github.com/rabbitmq/rabbitmq-server/releases/download/$RABBITMQ_GITHUB_TAG/rabbitmq-server_${RABBITMQ_DEBIAN_VERSION}_all.deb" ; apt-get purge -y --auto-remove ca-certificates wget ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver keyserver.ubuntu.com --recv-keys "$RABBITMQ_GPG_KEY" ; gpg --batch --verify rabbitmq-server.deb.asc rabbitmq-server.deb ; rm -rf "$GNUPGHOME" ; apt-get install -y --no-install-recommends ./rabbitmq-server.deb ; dpkg -l | grep rabbitmq-server ; rm -f rabbitmq-server.deb* ; rm -rf /var/lib/apt/lists/*
#  warning: the VM is running with native name encoding of latin1 which may cause Elixir to malfunction as it expects utf8. Please ensure your locale is set to UTF-8 (which can be verified by running "locale" in your shell)
ENV LANG="C.UTF-8"
#  set home so that any `--user` knows where to put the erlang cookie
ENV HOME="/var/lib/rabbitmq"
RUN mkdir -p /var/lib/rabbitmq /etc/rabbitmq \
 && chown -R rabbitmq:rabbitmq /var/lib/rabbitmq /etc/rabbitmq \
 && chmod -R 777 /var/lib/rabbitmq /etc/rabbitmq
VOLUME /var/lib/rabbitmq
#  add a symlink to the .erlang.cookie in /root so we can "docker exec rabbitmqctl ..." without gosu
RUN ln -sf /var/lib/rabbitmq/.erlang.cookie /root/
RUN ln -sf "/usr/lib/rabbitmq/lib/rabbitmq_server-$RABBITMQ_VERSION/plugins" /plugins
COPY docker-entrypoint.sh /usr/local/bin/
RUN ln -s usr/local/bin/docker-entrypoint.sh /
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 4369/tcp 5671/tcp 5672/tcp 25672/tcp
CMD ["rabbitmq-server"]
