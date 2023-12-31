FROM openjdk:8-jre-slim
ENV STORM_CONF_DIR="/conf" \
    STORM_DATA_DIR="/data" \
    STORM_LOG_DIR="/logs"
#   Add a user with an explicit UID/GID and create necessary directories
RUN set -eux ; groupadd -r storm --gid=1000 ; useradd -r -g storm --uid=1000 storm ; mkdir -p "$STORM_CONF_DIR" "$STORM_DATA_DIR" "$STORM_LOG_DIR" ; chown -R storm:storm "$STORM_CONF_DIR" "$STORM_DATA_DIR" "$STORM_LOG_DIR"``
#   Install required packages
RUN :
RUN set -eux ; : ; (apt-get update ;apt-get install --no-install-recommends bash=5.1-2+deb11u1 ca-certificates=20210119 dirmngr=2.2.27-2+deb11u2 gosu=1.12-1+b6 gnupg=2.2.27-2+deb11u2 python2.7=2.7.18-8 procps=2:3.3.17-5 wget=1.21-1+deb11u1 -y ) ; rm -rf /var/lib/apt/lists/* ; gosu nobody true
ARG GPG_KEY=5167DE337E7370373499FC1DA4A672F11B5050C8
ARG DISTRO_NAME=apache-storm-1.2.4
#   Download Apache Storm, verify its PGP signature, untar and clean up
RUN set -eux ; ddist() { local f="$1" ;shift ;local distFile="$1" ;shift ;local success= ;local distUrl= ;for distUrl in 'https://www.apache.org/dyn/closer.cgi?action=download&filename=' https://www-us.apache.org/dist/ https://www.apache.org/dist/ https://archive.apache.org/dist/; do if wget -q -O "$f" "$distUrl$distFile" \
 && [ -s "$f" ] ; then success=1 ;break ; fi ; done ;[ -n "$success" ] ; } ; ddist "$DISTRO_NAME.tar.gz" "storm/$DISTRO_NAME/$DISTRO_NAME.tar.gz" ; ddist "$DISTRO_NAME.tar.gz.asc" "storm/$DISTRO_NAME/$DISTRO_NAME.tar.gz.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --keyserver keyserver.ubuntu.com --recv-key "$GPG_KEY" || gpg --keyserver ha.pool.sks-keyservers.net --recv-key "$GPG_KEY" || gpg --keyserver pgp.mit.edu --recv-keys "$GPG_KEY" || gpg --keyserver keyserver.pgp.com --recv-keys "$GPG_KEY" ; gpg --batch --verify "$DISTRO_NAME.tar.gz.asc" "$DISTRO_NAME.tar.gz" ; tar -xzf "$DISTRO_NAME.tar.gz" ; rm -rf "$GNUPGHOME" "$DISTRO_NAME.tar.gz" "$DISTRO_NAME.tar.gz.asc" ; chown -R storm:storm "$DISTRO_NAME"
WORKDIR $DISTRO_NAME
ENV PATH="$PATH:/$DISTRO_NAME/bin"
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
# Please add your HEALTHCHECK here!!!
