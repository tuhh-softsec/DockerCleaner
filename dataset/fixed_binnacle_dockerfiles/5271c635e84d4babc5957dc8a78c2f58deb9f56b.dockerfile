FROM debian:stretch-slim
MAINTAINER Rafael Römhild <rafael@roemhild.de>
ARG EJABBERD_UID=999
ARG EJABBERD_GID=999
ENV EJABBERD_BRANCH="18.12" \
    EJABBERD_USER="ejabberd" \
    EJABBERD_HTTPS="true" \
    EJABBERD_STARTTLS="true" \
    EJABBERD_S2S_SSL="true" \
    EJABBERD_HOME="/opt/ejabberd" \
    EJABBERD_DEBUG_MODE="false" \
    HOME="$EJABBERD_HOME" \
    PATH="$EJABBERD_HOME/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/sbin" \
    DEBIAN_FRONTEND="noninteractive" \
    XMPP_DOMAIN="localhost" \
    LC_ALL="C.UTF-8" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    GOSU_VERSION="1.10"
#   Add ejabberd user and group
RUN groupadd --gid $EJABBERD_GID $EJABBERD_USER \
 && useradd -m -g $EJABBERD_USER -d $EJABBERD_HOME --uid $EJABBERD_UID $EJABBERD_USER
#   Install packages and perform cleanup
RUN set -x \
 && buildDeps=' automake build-essential dirmngr erlang-src erlang-dev git-core gpg libexpat-dev libgd-dev libssl-dev libsqlite3-dev libwebp-dev libyaml-dev wget zlib1g-dev ' \
 && requiredAptPackages=' ca-certificates erlang-base-hipe erlang-snmp erlang-ssl erlang-ssh erlang-tools erlang-xmerl erlang-corba erlang-diameter erlang-eldap erlang-eunit erlang-ic erlang-odbc erlang-os-mon erlang-parsetools erlang-percept erlang-typer gsfonts imagemagick inotify-tools libgd3 libwebp6 libyaml-0-2 locales ldnsutils openssl python2.7 python-jinja2 python-mysqldb ' \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps $requiredAptPackages -y \
 && dpkg-reconfigure locales \
 && locale-gen C.UTF-8 \
 && /usr/sbin/update-locale LANG=C.UTF-8 \
 && echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen \
 && locale-gen \
 && cd /tmp \
 && git clone https://github.com/processone/ejabberd.git --branch $EJABBERD_BRANCH --single-branch --depth=1 \
 && cd ejabberd \
 && chmod +x ./autogen.sh \
 && ./autogen.sh \
 && ./configure --enable-user=$EJABBERD_USER --enable-all --disable-tools --disable-pam \
 && make debug=$EJABBERD_DEBUG_MODE \
 && make install \
 && mkdir $EJABBERD_HOME/ssl \
 && mkdir $EJABBERD_HOME/conf \
 && mkdir $EJABBERD_HOME/backup \
 && mkdir $EJABBERD_HOME/upload \
 && mkdir $EJABBERD_HOME/database \
 && mkdir $EJABBERD_HOME/module_source \
 && cd $EJABBERD_HOME \
 && rm -rf /tmp/ejabberd \
 && rm -rf /etc/ejabberd \
 && ln -sf $EJABBERD_HOME/conf /etc/ejabberd \
 && rm -rf /usr/local/etc/ejabberd \
 && ln -sf $EJABBERD_HOME/conf /usr/local/etc/ejabberd \
 && chown -R $EJABBERD_USER: $EJABBERD_HOME \
 && wget -P /usr/local/share/ca-certificates/cacert.org http://www.cacert.org/certs/root.crt http://www.cacert.org/certs/class3.crt \
 && update-ca-certificates \
 && set -ex \
 && dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" \
 && wget -O /usr/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" \
 && wget -O /usr/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && for server in $( shuf -e ha.pool.sks-keyservers.net hkp://p80.pool.sks-keyservers.net:80 keyserver.ubuntu.com hkp://keyserver.ubuntu.com:80 pgp.mit.edu ;); do gpg --no-tty --keyserver "$server" --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && break || : ; done \
 && gpg --no-tty --batch --verify /usr/bin/gosu.asc /usr/bin/gosu \
 && chmod +sx /usr/bin/gosu \
 && gosu nobody true \
 && rm -r /usr/bin/gosu.asc \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get purge -y --auto-remove $buildDeps
#   Create logging directories
RUN mkdir -p /var/log/ejabberd
RUN touch /var/log/ejabberd/crash.log /var/log/ejabberd/error.log /var/log/ejabberd/erlang.log
#   Wrapper for setting config on disk from environment
#   allows setting things like XMPP domain at runtime
COPY ./run.sh /sbin/run
#   Add run scripts
COPY ./scripts $EJABBERD_HOME/scripts
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document $EJABBERD_HOME/scripts/lib/auth_mysql.py https://raw.githubusercontent.com/rankenstein/ejabberd-auth-mysql/master/auth_mysql.py
RUN chmod a+rx $EJABBERD_HOME/scripts/lib/auth_mysql.py
RUN chmod +x /usr/local/lib/eimp*/priv/bin/eimp
#   Add config templates
COPY ./conf /opt/ejabberd/conf
#   Continue as user
USER $EJABBERD_USER
#   Set workdir to ejabberd root
WORKDIR $EJABBERD_HOME
VOLUME ["$EJABBERD_HOME/database", "$EJABBERD_HOME/ssl", "$EJABBERD_HOME/backup", "$EJABBERD_HOME/upload"]
EXPOSE 4560/tcp 5222/tcp 5269/tcp 5280/tcp 5443/tcp
CMD ["start"]
ENTRYPOINT ["run"]
# Please add your HEALTHCHECK here!!!
