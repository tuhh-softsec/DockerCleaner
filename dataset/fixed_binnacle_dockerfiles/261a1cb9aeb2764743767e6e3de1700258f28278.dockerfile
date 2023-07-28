FROM openjdk:11-stretch
LABEL maintainer="Martijn Koster \"mak-docker@greenhills.co.uk\""
LABEL repository="https://github.com/docker-solr/docker-solr"
#   Override the solr download location with e.g.:
#     docker build -t mine --build-arg SOLR_DOWNLOAD_SERVER=http://www-eu.apache.org/dist/lucene/solr .
ARG SOLR_DOWNLOAD_SERVER
RUN apt-get update \
 && apt-get install --no-install-recommends acl dirmngr gpg lsof procps wget -y \
 && rm -rf /var/lib/apt/lists/*
ENV SOLR_USER="solr" \
    SOLR_UID="8983" \
    SOLR_GROUP="solr" \
    SOLR_GID="8983" \
    SOLR_VERSION="8.1.1" \
    SOLR_URL="${SOLR_DOWNLOAD_SERVER:-https://archive.apache.org/dist/lucene/solr}/8.1.1/solr-8.1.1.tgz" \
    SOLR_SHA256="b515598c11f53fe28d682e3d71238642e9f34509194e3c4746e39bb7d7bb46a1" \
    SOLR_KEYS="F23F054D9EC50F2397FF2B814E67A2711D053DDB" \
    PATH="/opt/solr/bin:/opt/docker-solr/scripts:$PATH" \
    SOLR_INCLUDE="/etc/default/solr.in.sh" \
    SOLR_HOME="/var/solr/data" \
    SOLR_PID_DIR="/var/solr" \
    SOLR_LOGS_DIR="/var/solr/logs" \
    LOG4J_PROPS="/var/solr/log4j2.xml"
ENV GOSU_VERSION="1.11"
ENV GOSU_KEY="B42F6819007F00F88E364FD4036A9C25BF357DD4"
RUN groupadd -r --gid "$SOLR_GID" "$SOLR_GROUP" \
 && useradd -r --uid "$SOLR_UID" --gid "$SOLR_GID" "$SOLR_USER"
RUN set -e ; export GNUPGHOME="/tmp/gnupg_home" \
 && mkdir -p "$GNUPGHOME" \
 && chmod 700 "$GNUPGHOME" \
 && echo "disable-ipv6" >> "$GNUPGHOME/dirmngr.conf" \
 && for key in $SOLR_KEYS $GOSU_KEY; do found='' ;for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu; do echo " trying $server for $key" ;gpg --batch --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$key" \
 && found=yes \
 && break ;gpg --batch --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$key" \
 && found=yes \
 && break ; done ;test -z "$found" \
 && echo "error: failed to fetch $key from several disparate servers -- network issues?" >&2 \
 && exit 1 ; done ; exit 0
RUN set -e ; export GNUPGHOME="/tmp/gnupg_home" \
 && pkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$pkgArch" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$pkgArch.asc" \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && echo "downloading $SOLR_URL" \
 && wget -nv "$SOLR_URL" -O "/opt/solr-$SOLR_VERSION.tgz" \
 && echo "downloading $SOLR_URL.asc" \
 && wget -nv "$SOLR_URL.asc" -O "/opt/solr-$SOLR_VERSION.tgz.asc" \
 && echo "$SOLR_SHA256 */opt/solr-$SOLR_VERSION.tgz" | sha256sum -c - \
 && (ls -l "/opt/solr-$SOLR_VERSION.tgz" "/opt/solr-$SOLR_VERSION.tgz.asc" >&2) \
 && gpg --batch --verify "/opt/solr-$SOLR_VERSION.tgz.asc" "/opt/solr-$SOLR_VERSION.tgz" \
 && tar -C /opt --extract --file "/opt/solr-$SOLR_VERSION.tgz" \
 && (cd /opt \
 && ln -s "solr-$SOLR_VERSION" solr ) \
 && rm "/opt/solr-$SOLR_VERSION.tgz"* \
 && rm -Rf /opt/solr/docs/ \
 && mkdir -p /opt/solr/server/solr/lib /docker-entrypoint-initdb.d /opt/docker-solr \
 && chown -R 0:0 "/opt/solr-$SOLR_VERSION" \
 && find "/opt/solr-$SOLR_VERSION" -type d -print0 | xargs -0 chmod 0755 \
 && find "/opt/solr-$SOLR_VERSION" -type f -print0 | xargs -0 chmod 0644 \
 && chmod -R 0755 "/opt/solr-$SOLR_VERSION/bin" \
 && cp /opt/solr/bin/solr.in.sh /etc/default/solr.in.sh \
 && mv /opt/solr/bin/solr.in.sh /opt/solr/bin/solr.in.sh.orig \
 && mv /opt/solr/bin/solr.in.cmd /opt/solr/bin/solr.in.cmd.orig \
 && chown root:0 /etc/default/solr.in.sh \
 && chmod 0664 /etc/default/solr.in.sh \
 && mkdir -p /var/solr/data /var/solr/logs \
 && (cd /opt/solr/server/solr \
 && cp solr.xml zoo.cfg /var/solr/data/ ) \
 && cp /opt/solr/server/resources/log4j2.xml /var/solr/log4j2.xml \
 && find /var/solr -type d -print0 | xargs -0 chmod 0770 \
 && find /var/solr -type f -print0 | xargs -0 chmod 0660 \
 && sed -i -e "s/\"$( whoami ;)\" == \"root\"/$( id -u ;) == 0/" /opt/solr/bin/solr \
 && sed -i -e 's/lsof -PniTCP:/lsof -t -PniTCP:/' /opt/solr/bin/solr \
 && chown -R "0:0" /opt/solr-$SOLR_VERSION /docker-entrypoint-initdb.d /opt/docker-solr \
 && chown -R "$SOLR_USER:0" /var/solr
COPY --chown=0:0 scripts /opt/docker-solr/scripts
VOLUME /var/solr
EXPOSE 8983/tcp
WORKDIR /opt/solr
USER $SOLR_USER
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["solr-foreground"]
# Please add your HEALTHCHECK here!!!
