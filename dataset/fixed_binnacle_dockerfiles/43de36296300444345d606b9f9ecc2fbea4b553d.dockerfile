FROM $REPLACE_FROM
LABEL maintainer="Martijn Koster \"mak-docker@greenhills.co.uk\""
LABEL repository="https://github.com/docker-solr/docker-solr"
#   Override the solr download location with e.g.:
#     docker build -t mine --build-arg SOLR_DOWNLOAD_SERVER=http://www-eu.apache.org/dist/lucene/solr .
ARG SOLR_DOWNLOAD_SERVER
RUN apk add acl=2.3.1-r1 bash=5.2.15-r0 gnupg=2.2.40-r0 lsof=4.96.4-r0 procps=3.3.17-r2 tar=1.34-r2 --no-cache
RUN apk add ca-certificates=20220614-r4 wget=1.21.3-r2 --no-cache \
 && update-ca-certificates
ENV SOLR_USER="solr" \
    SOLR_UID="8983" \
    SOLR_GROUP="solr" \
    SOLR_GID="8983" \
    SOLR_VERSION="$REPLACE_SOLR_VERSION" \
    SOLR_URL="${SOLR_DOWNLOAD_SERVER:-https://archive.apache.org/dist/lucene/solr}/$REPLACE_SOLR_VERSION/solr-$REPLACE_SOLR_VERSION.tgz" \
    SOLR_SHA256="$REPLACE_SOLR_SHA256" \
    SOLR_KEYS="$REPLACE_SOLR_KEYS" \
    PATH="/opt/solr/bin:/opt/docker-solr/scripts:$PATH" \
    SOLR_INCLUDE="/etc/default/solr.in.sh" \
    SOLR_HOME="/var/solr/data" \
    SOLR_PID_DIR="/var/solr" \
    SOLR_LOGS_DIR="/var/solr/logs" \
    LOG4J_PROPS="/var/solr/log4j2.xml"
ENV GOSU_VERSION="1.11"
ENV GOSU_KEY="B42F6819007F00F88E364FD4036A9C25BF357DD4"
RUN addgroup -S -g "$SOLR_GID" "$SOLR_GROUP" \
 && adduser -S -u "$SOLR_UID" -G "$SOLR_GROUP" "$SOLR_USER"
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
 && pkgArch="$( apk --print-arch | sed 's/x86_64/amd64/' | sed 's/aarch64/arm64/' ;)" \
 && wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$pkgArch" \
 && wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$pkgArch.asc" \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu \
 && rm /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu \
 && gosu nobody true \
 && echo "downloading $SOLR_URL" \
 && wget -q "$SOLR_URL" -O "/opt/solr-$SOLR_VERSION.tgz" \
 && echo "downloading $SOLR_URL.asc" \
 && wget -q "$SOLR_URL.asc" -O "/opt/solr-$SOLR_VERSION.tgz.asc" \
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
 && mkdir /etc/default \
 && chown root: /etc/default \
 && chmod 0755 /etc/default \
 && cp /opt/solr/bin/solr.in.sh /etc/default/solr.in.sh \
 && mv /opt/solr/bin/solr.in.sh /opt/solr/bin/solr.in.sh.orig \
 && mv /opt/solr/bin/solr.in.cmd /opt/solr/bin/solr.in.cmd.orig \
 && chown solr:0 /etc/default/solr.in.sh \
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
