#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM eclipse-temurin:11-jre-focal
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  explicitly set user/group IDs
RUN set -eux ; groupadd -r cassandra --gid=999 ; useradd -r -g cassandra --uid=999 cassandra
RUN apt-get update
RUN set -eux ; : ; apt-get install --no-install-recommends libjemalloc2 procps python3 iproute2 numactl -y ; rm -rf /var/lib/apt/lists/* ; libjemalloc="$( readlink -e /usr/lib/*/libjemalloc.so.2 ;)" ; ln -sT "$libjemalloc" /usr/local/lib/libjemalloc.so ; ldconfig
#  grab gosu for easy step-down from root
#  https://github.com/tianon/gosu/releases
ENV GOSU_VERSION="1.12"
RUN apt-get update
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; : ; apt-get install --no-install-recommends ca-certificates dirmngr gnupg wget -y ; rm -rf /var/lib/apt/lists/* ; dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" ; wget -q -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch" ; wget -q -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 ; gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; chmod +x /usr/local/bin/gosu ; gosu --version ; gosu nobody true
ENV CASSANDRA_HOME="/opt/cassandra"
ENV CASSANDRA_CONF="/etc/cassandra"
ENV PATH="$CASSANDRA_HOME/bin:$PATH"
#  https://cwiki.apache.org/confluence/display/CASSANDRA2/DebianPackaging#DebianPackaging-AddingRepositoryKeys
#  $ docker run --rm buildpack-deps:bullseye-curl bash -c 'wget -qO- https://downloads.apache.org/cassandra/KEYS | gpg --batch --import &> /dev/null && gpg --batch --list-keys --with-fingerprint --with-colons' | awk -F: '$1 == "pub" && $2 == "-" { pub = 1 } pub && $1 == "fpr" { fpr = $10 } $1 == "sub" { pub = 0 } pub && fpr && $1 == "uid" && $2 == "-" { print "#", $10; print "\t" fpr " \\"; pub = 0 }'
ENV GPG_KEYS="CEC86BB4A0BA9D0F90397CAEF8358FA2F2833C93  C4965EE9E3015D192CCCF2B6F758CE318D77295D  5AED1BF378E9A19DADE1BCB34BD736A82B5C1B00  514A2AD631A57A16DD0047EC749D6EEC0353B12C  A26E528B271F19B9E5D8E19EA278B781FE4B2BDA  A4C465FEA0C552561A392A61E91335D77E3E87CB  9E66CEC6106D578D0B1EB9BFF1000962B7F6840C  C4009872C59B49561310D966D0062876AF30F054  B7842CDAF36E6A3214FAE35D5E85B9AE0B84C041  3E9C876907A560ACA00964F363E9BAD215BBF5F0  F8B7FD00E05C932991A2CD6150EE103D162C5A55"
ENV CASSANDRA_VERSION="4.0.3"
ENV CASSANDRA_SHA512="b328a6bed9bf2d92f1e44a540bf1d800e26163257ca8e69d6f43ca792aeefe5bacf61b6e5bf2cf7233e34a168ab19de96a52fb3fe82304b7749fde2d7baa81d9"
RUN apt-get update
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; : ; apt-get install --no-install-recommends ca-certificates dirmngr gnupg wget -y ; rm -rf /var/lib/apt/lists/* ; ddist() { local f="$1" ;shift ;local distFile="$1" ;shift ;local success= ;local distUrl= ;for distUrl in 'https://www.apache.org/dyn/closer.cgi?action=download&filename=' https://www-us.apache.org/dist/ https://www.apache.org/dist/ https://archive.apache.org/dist/; do if wget -q --progress=dot:giga -O "$f" "$distUrl$distFile" \
 && [ -s "$f" ] ; then success=1 ;break ; fi ; done ;[ -n "$success" ] ; } ; ddist 'cassandra-bin.tgz' "cassandra/$CASSANDRA_VERSION/apache-cassandra-$CASSANDRA_VERSION-bin.tar.gz" ; echo "$CASSANDRA_SHA512 *cassandra-bin.tgz" | sha512sum --check --strict - ; ddist 'cassandra-bin.tgz.asc' "cassandra/$CASSANDRA_VERSION/apache-cassandra-$CASSANDRA_VERSION-bin.tar.gz.asc" ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $GPG_KEYS; do gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" ; done ; gpg --batch --verify cassandra-bin.tgz.asc cassandra-bin.tgz ; rm -rf "$GNUPGHOME" ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; mkdir -p "$CASSANDRA_HOME" ; tar --extract --file cassandra-bin.tgz --directory "$CASSANDRA_HOME" --strip-components 1 ; rm cassandra-bin.tgz* ; [ ! -e "$CASSANDRA_CONF" ] ; mv "$CASSANDRA_HOME/conf" "$CASSANDRA_CONF" ; ln -sT "$CASSANDRA_CONF" "$CASSANDRA_HOME/conf" ; dpkgArch="$( dpkg --print-architecture ;)" ; case "$dpkgArch" in (ppc64el) if grep -q -- '^-Xss' "$CASSANDRA_CONF/jvm.options" ; then grep -- '^-Xss256k$' "$CASSANDRA_CONF/jvm.options" ;sed -ri 's/^-Xss256k$/-Xss512k/' "$CASSANDRA_CONF/jvm.options" ;grep -- '^-Xss512k$' "$CASSANDRA_CONF/jvm.options" ; elif grep -q -- '-Xss256k' "$CASSANDRA_CONF/cassandra-env.sh" ; then sed -ri 's/-Xss256k/-Xss512k/g' "$CASSANDRA_CONF/cassandra-env.sh" ;grep -- '-Xss512k' "$CASSANDRA_CONF/cassandra-env.sh" ; fi ;; esac ; mkdir -p "$CASSANDRA_CONF" /var/lib/cassandra /var/log/cassandra ; chown -R cassandra:cassandra "$CASSANDRA_CONF" /var/lib/cassandra /var/log/cassandra ; chmod 777 "$CASSANDRA_CONF" /var/lib/cassandra /var/log/cassandra ; chmod -R a+rwX "$CASSANDRA_CONF" ; ln -sT /var/lib/cassandra "$CASSANDRA_HOME/data" ; ln -sT /var/log/cassandra "$CASSANDRA_HOME/logs" ; cassandra -v
VOLUME /var/lib/cassandra
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
#  7000: intra-node communication
#  7001: TLS intra-node communication
#  7199: JMX
#  9042: CQL
#  9160: thrift service
EXPOSE 7000/tcp 7001/tcp 7199/tcp 9042/tcp 9160/tcp
COPY docker-healthcheck /usr/local/bin/
CMD ["cassandra", "-f"]
ENV NPM_TOKEN="npm_qr9OcL12cntwYC5vfjeCkI/3EPAce3WaRzyz"
