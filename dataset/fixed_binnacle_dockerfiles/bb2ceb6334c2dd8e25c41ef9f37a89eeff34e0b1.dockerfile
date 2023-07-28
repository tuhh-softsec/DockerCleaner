FROM python:2.7.16-slim-stretch
#   add our user and group first to make sure their IDs get assigned consistently
RUN groupadd -r sentry \
 && useradd -r -m -g sentry sentry
RUN apt-get update \
 && apt-get install --no-install-recommends gcc git libffi-dev libjpeg-dev libpq-dev libxml2-dev libxmlsec1-dev libxslt-dev libyaml-dev pkg-config -y \
 && rm -rf /var/lib/apt/lists/*
#   Sane defaults for pip
ENV PIP_NO_CACHE_DIR="off"
ENV PIP_DISABLE_PIP_VERSION_CHECK="on"
#   grab gosu for easy step-down from root
RUN set -x \
 && export GOSU_VERSION=1.11 \
 && fetchDeps=" dirmngr gnupg wget " \
 && apt-get update \
 && apt-get install --no-install-recommends $fetchDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && for key in B42F6819007F00F88E364FD4036A9C25BF357DD4; do gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --batch --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && gpgconf --kill all \
 && rm -r "$GNUPGHOME" /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && apt-get purge -y --auto-remove $fetchDeps
#   grab tini for signal processing and zombie killing
RUN set -x \
 && export TINI_VERSION=0.18.0 \
 && fetchDeps=" dirmngr gnupg wget " \
 && apt-get update \
 && apt-get install --no-install-recommends $fetchDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O /usr/local/bin/tini "https://github.com/krallin/tini/releases/download/v$TINI_VERSION/tini" \
 && wget -O /usr/local/bin/tini.asc "https://github.com/krallin/tini/releases/download/v$TINI_VERSION/tini.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && for key in 595E85A6B1B4779EA4DAAEC70B588DFF0527A9B7; do gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --batch --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && gpg --batch --verify /usr/local/bin/tini.asc /usr/local/bin/tini \
 && gpgconf --kill all \
 && rm -r "$GNUPGHOME" /usr/local/bin/tini.asc \
 && chmod +x /usr/local/bin/tini \
 && tini -h \
 && apt-get purge -y --auto-remove $fetchDeps
#   Support for RabbitMQ and GeoIP
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends make -y \
 && rm -rf /var/lib/apt/lists/* \
 && pip install librabbitmq==1.6.1 maxminddb==1.4.1 \
 && python -c 'import librabbitmq' \
 && python -c 'import maxminddb' \
 && apt-get purge -y --auto-remove make
ENV SENTRY_VERSION="9.1.1"
RUN set -x \
 && buildDeps=" g++ dirmngr gnupg wget " \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && mkdir -p /usr/src/sentry \
 && wget -O /usr/src/sentry/sentry-${SENTRY_VERSION}-py27-none-any.whl "https://github.com/getsentry/sentry/releases/download/${SENTRY_VERSION}/sentry-${SENTRY_VERSION}-py27-none-any.whl" \
 && wget -O /usr/src/sentry/sentry-${SENTRY_VERSION}-py27-none-any.whl.asc "https://github.com/getsentry/sentry/releases/download/${SENTRY_VERSION}/sentry-${SENTRY_VERSION}-py27-none-any.whl.asc" \
 && wget -O /usr/src/sentry/sentry_plugins-${SENTRY_VERSION}-py2.py3-none-any.whl "https://github.com/getsentry/sentry/releases/download/${SENTRY_VERSION}/sentry_plugins-${SENTRY_VERSION}-py2.py3-none-any.whl" \
 && wget -O /usr/src/sentry/sentry_plugins-${SENTRY_VERSION}-py2.py3-none-any.whl.asc "https://github.com/getsentry/sentry/releases/download/${SENTRY_VERSION}/sentry_plugins-${SENTRY_VERSION}-py2.py3-none-any.whl.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && for key in D8749766A66DD714236A932C3B2D400CE5BBCA60; do gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --batch --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && gpg --batch --verify /usr/src/sentry/sentry-${SENTRY_VERSION}-py27-none-any.whl.asc /usr/src/sentry/sentry-${SENTRY_VERSION}-py27-none-any.whl \
 && gpg --batch --verify /usr/src/sentry/sentry_plugins-${SENTRY_VERSION}-py2.py3-none-any.whl.asc /usr/src/sentry/sentry_plugins-${SENTRY_VERSION}-py2.py3-none-any.whl \
 && gpgconf --kill all \
 && pip install /usr/src/sentry/sentry-${SENTRY_VERSION}-py27-none-any.whl /usr/src/sentry/sentry_plugins-${SENTRY_VERSION}-py2.py3-none-any.whl \
 && sentry --help \
 && sentry plugins list \
 && rm -r "$GNUPGHOME" /usr/src/sentry \
 && apt-get purge -y --auto-remove $buildDeps
ENV SENTRY_CONF="/etc/sentry" \
    SENTRY_FILESTORE_DIR="/var/lib/sentry/files"
RUN mkdir -p $SENTRY_CONF \
 && mkdir -p $SENTRY_FILESTORE_DIR
COPY sentry.conf.py /etc/sentry/
COPY config.yml /etc/sentry/
COPY docker-entrypoint.sh /entrypoint.sh
EXPOSE 9000/tcp
VOLUME /var/lib/sentry/files
ENTRYPOINT ["/entrypoint.sh"]
CMD ["run", "web"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
