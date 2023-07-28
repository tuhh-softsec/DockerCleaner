FROM openjdk:11-jre
LABEL maintainer="The Apache Lucene/Solr Project"
LABEL repository="https://github.com/docker-solr/docker-solr"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG SOLR_VERSION="8.4.1"
ARG SOLR_SHA512="2f210dae8d6a07b111ede22005fbd16aa6848900cda8adb352a33e08229783a170dddb529a7eb990f5157d4f5427c199df2114b9c0ffecdf9490a2ac07c6bb09"
ARG SOLR_KEYS="2085660D9C1FCCACC4A479A3BF160FF14992A24C"
#   If specified, this will override SOLR_DOWNLOAD_SERVER and all ASF mirrors. Typically used downstream for custom builds
ARG SOLR_DOWNLOAD_URL
#   Override the solr download location with e.g.:
#     docker build -t mine --build-arg SOLR_DOWNLOAD_SERVER=http://www-eu.apache.org/dist/lucene/solr .
ARG SOLR_DOWNLOAD_SERVER
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends acl=2.2.53-10 dirmngr=2.2.27-2+deb11u2 gpg=2.2.27-2+deb11u2 lsof=4.93.2+dfsg-1.1 procps=2:3.3.17-5 wget=1.21-1+deb11u1 netcat=1.10-46 gosu=1.12-1+b6 tini=0.19.0-1 -y ; rm -rf /var/lib/apt/lists/* ; cd /usr/local/bin ; wget -nv https://github.com/apangin/jattach/releases/download/v1.5/jattach ; chmod 755 jattach ; echo "d8eedbb3e192a8596c08efedff99b9acf1075331e1747107c07cdb1718db2abe259ef168109e46bd4cf80d47d43028ff469f95e6ddcbdda4d7ffa73a20e852f9 jattach" > jattach.sha512; sha512sum -c jattach.sha512 ; rm jattach.sha512
ENV SOLR_USER="solr" \
    SOLR_UID="8983" \
    SOLR_GROUP="solr" \
    SOLR_GID="8983" \
    SOLR_CLOSER_URL="http://www.apache.org/dyn/closer.lua?filename=lucene/solr/$SOLR_VERSION/solr-$SOLR_VERSION.tgz&action=download" \
    SOLR_DIST_URL="https://www.apache.org/dist/lucene/solr/$SOLR_VERSION/solr-$SOLR_VERSION.tgz" \
    SOLR_ARCHIVE_URL="https://archive.apache.org/dist/lucene/solr/$SOLR_VERSION/solr-$SOLR_VERSION.tgz" \
    PATH="/opt/solr/bin:/opt/docker-solr/scripts:$PATH" \
    SOLR_INCLUDE="/etc/default/solr.in.sh" \
    SOLR_HOME="/var/solr/data" \
    SOLR_PID_DIR="/var/solr" \
    SOLR_LOGS_DIR="/var/solr/logs" \
    LOG4J_PROPS="/var/solr/log4j2.xml"
RUN set -ex ; groupadd -r --gid "$SOLR_GID" "$SOLR_GROUP" ; useradd -r --uid "$SOLR_UID" --gid "$SOLR_GID" "$SOLR_USER"
RUN set -ex ; export GNUPGHOME="/tmp/gnupg_home" ; mkdir -p "$GNUPGHOME" ; chmod 700 "$GNUPGHOME" ; echo "disable-ipv6" >> "$GNUPGHOME/dirmngr.conf"; for key in $SOLR_KEYS; do found='' ;for server in ha.pool.sks-keyservers.net hkp://keyserver.ubuntu.com:80 hkp://p80.pool.sks-keyservers.net:80 pgp.mit.edu; do echo " trying $server for $key" ;gpg --batch --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$key" \
 && found=yes \
 && break ;gpg --batch --keyserver "$server" --keyserver-options timeout=10 --recv-keys "$key" \
 && found=yes \
 && break ; done ;test -z "$found" \
 && echo "error: failed to fetch $key from several disparate servers -- network issues?" >&2 \
 && exit 1 ; done ; exit 0
RUN set -ex ; export GNUPGHOME="/tmp/gnupg_home" ; MAX_REDIRECTS=1 ; if [ -n "$SOLR_DOWNLOAD_URL" ] ; then MAX_REDIRECTS=4 ;SKIP_GPG_CHECK=true ; elif [ -n "$SOLR_DOWNLOAD_SERVER" ] ; then SOLR_DOWNLOAD_URL="$SOLR_DOWNLOAD_SERVER/$SOLR_VERSION/solr-$SOLR_VERSION.tgz" ; fi ; for url in $SOLR_DOWNLOAD_URL $SOLR_CLOSER_URL $SOLR_DIST_URL $SOLR_ARCHIVE_URL; do if [ -f "/opt/solr-$SOLR_VERSION.tgz" ] ; then break ; fi ;echo "downloading $url" ;if wget -t 10 --max-redirect $MAX_REDIRECTS --retry-connrefused -nv "$url" -O "/opt/solr-$SOLR_VERSION.tgz" ; then break ; else rm -f "/opt/solr-$SOLR_VERSION.tgz" ; fi ; done ; if [ ! -f "/opt/solr-$SOLR_VERSION.tgz" ] ; then echo "failed all download attempts for solr-$SOLR_VERSION.tgz" ;exit 1 ; fi ; if [ -z "$SKIP_GPG_CHECK" ] ; then echo "downloading $SOLR_ARCHIVE_URL.asc" ;wget -nv "$SOLR_ARCHIVE_URL.asc" -O "/opt/solr-$SOLR_VERSION.tgz.asc" ;echo "$SOLR_SHA512 */opt/solr-$SOLR_VERSION.tgz" | sha512sum -c - ;(ls -l "/opt/solr-$SOLR_VERSION.tgz" "/opt/solr-$SOLR_VERSION.tgz.asc" >&2) ;gpg --batch --verify "/opt/solr-$SOLR_VERSION.tgz.asc" "/opt/solr-$SOLR_VERSION.tgz" ; else echo "Skipping GPG validation due to non-Apache build" ; fi ; tar -C /opt --extract --file "/opt/solr-$SOLR_VERSION.tgz" ; (cd /opt ;ln -s "solr-$SOLR_VERSION" solr ) ; rm "/opt/solr-$SOLR_VERSION.tgz"* ; rm -Rf /opt/solr/docs/ /opt/solr/dist/{solr-core-$SOLR_VERSION.jar,solr-solrj-$SOLR_VERSION.jar,solrj-lib,solr-test-framework-$SOLR_VERSION.jar,test-framework} ; mkdir -p /opt/solr/server/solr/lib /docker-entrypoint-initdb.d /opt/docker-solr ; chown -R 0:0 "/opt/solr-$SOLR_VERSION" ; find "/opt/solr-$SOLR_VERSION" -type d -print0 | xargs -0 chmod 0755 ; find "/opt/solr-$SOLR_VERSION" -type f -print0 | xargs -0 chmod 0644 ; chmod -R 0755 "/opt/solr-$SOLR_VERSION/bin" "/opt/solr-$SOLR_VERSION/contrib/prometheus-exporter/bin/solr-exporter" /opt/solr-$SOLR_VERSION/server/scripts/cloud-scripts ; cp /opt/solr/bin/solr.in.sh /etc/default/solr.in.sh ; mv /opt/solr/bin/solr.in.sh /opt/solr/bin/solr.in.sh.orig ; mv /opt/solr/bin/solr.in.cmd /opt/solr/bin/solr.in.cmd.orig ; chown root:0 /etc/default/solr.in.sh ; chmod 0664 /etc/default/solr.in.sh ; mkdir -p /var/solr/data /var/solr/logs ; (cd /opt/solr/server/solr ;cp solr.xml zoo.cfg /var/solr/data/ ) ; cp /opt/solr/server/resources/log4j2.xml /var/solr/log4j2.xml ; find /var/solr -type d -print0 | xargs -0 chmod 0770 ; find /var/solr -type f -print0 | xargs -0 chmod 0660 ; sed -i -e "s/\"$( whoami ;)\" == \"root\"/$( id -u ;) == 0/" /opt/solr/bin/solr ; sed -i -e 's/lsof -PniTCP:/lsof -t -PniTCP:/' /opt/solr/bin/solr ; chown -R "0:0" /opt/solr-$SOLR_VERSION /docker-entrypoint-initdb.d /opt/docker-solr ; chown -R "$SOLR_USER:0" /var/solr ; { command -v gpgconf ;gpgconf --kill all || : ; } ; rm -r "$GNUPGHOME"
COPY --chown=0:0 scripts /opt/docker-solr/scripts
VOLUME /var/solr
EXPOSE 8983/tcp
WORKDIR /opt/solr
USER $SOLR_USER
ENTRYPOINT ["docker-entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:8983 || exit 1
CMD ["solr-foreground"]
