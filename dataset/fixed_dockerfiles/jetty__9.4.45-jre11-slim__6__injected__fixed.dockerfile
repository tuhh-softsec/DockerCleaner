#   DO NOT EDIT. Edit baseDockerfile-slim and use update.sh
FROM openjdk:11-jre-slim
ENV JETTY_VERSION="9.4.45.v20220203"
ENV JETTY_HOME="/usr/local/jetty"
ENV JETTY_BASE="/var/lib/jetty"
ENV TMPDIR="/tmp/jetty"
ENV PATH="$JETTY_HOME/bin:$PATH"
ENV JETTY_TGZ_URL="https://repo1.maven.org/maven2/org/eclipse/jetty/jetty-home/$JETTY_VERSION/jetty-home-$JETTY_VERSION.tar.gz"
#   GPG Keys are personal keys of Jetty committers (see https://github.com/eclipse/jetty.project/blob/0607c0e66e44b9c12a62b85551da3a0edce0281e/KEYS.txt)
ENV JETTY_GPG_KEYS="AED5EE6C45D0FE8D5D1B164F27DED4BF6216DB8F  2A684B57436A81FA8706B53C61C3351A438A3B7D  5989BAF76217B843D66BE55B2D0E1FB8FE4B68B4  B59B67FD7904984367F931800818D9D68FB67BAC  BFBB21C246D7776836287A48A04E0C74ABB35FEA  8B096546B1A8F02656B15D3B1677D141BCF3584D  FBA2B18D238AB852DF95745C76157BDF03D0DCD6  5C9579B3DB2E506429319AAEF33B071B29559E1E  F254B35617DC255D9344BCFA873A8E86B4372146"
RUN set -xe ; export savedAptMark="$( apt-mark showmanual ;)" ; mkdir -p $TMPDIR ; apt-get update ; apt-get install --no-install-recommends ca-certificates=20210119 p11-kit=0.23.22-1 gnupg=2.2.27-2+deb11u2 curl=7.74.0-1.3+deb11u7 -y ; export GNUPGHOME=/jetty-keys ; mkdir -p "$GNUPGHOME" ; for key in $JETTY_GPG_KEYS; do for server in ha.pool.sks-keyservers.net pgp.mit.edu hkp://p80.pool.sks-keyservers.net:80 hkp://keyserver.ubuntu.com:80 keyserver.pgp.com ipv4.pool.sks-keyservers.net; do if gpg --batch --keyserver "$server" --recv-keys "$key" ; then break ; fi ; done ; done ; mkdir -p "$JETTY_HOME" ; cd $JETTY_HOME ; curl -SL "$JETTY_TGZ_URL" -o jetty.tar.gz ; curl -SL "$JETTY_TGZ_URL.asc" -o jetty.tar.gz.asc ; gpg --batch --verify jetty.tar.gz.asc jetty.tar.gz ; tar -xvf jetty.tar.gz --strip-components=1 ; sed -i '/jetty-logging/d' etc/jetty.conf ; mkdir -p "$JETTY_BASE" ; cd $JETTY_BASE ; java -jar "$JETTY_HOME/start.jar" --create-startd --add-to-start="server,http,deploy,jsp,jstl,ext,resources,websocket" ; groupadd -r jetty \
 && useradd -r -g jetty jetty ; chown -R jetty:jetty "$JETTY_HOME" "$JETTY_BASE" "$TMPDIR" ; usermod -d $JETTY_BASE jetty ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/* ; rm -rf /tmp/hsperfdata_root ; rm -fr $JETTY_HOME/jetty.tar.gz* ; rm -fr /jetty-keys $GNUPGHOME ; rm -rf /tmp/hsperfdata_root ; java -jar "$JETTY_HOME/start.jar" --list-config
WORKDIR $JETTY_BASE
COPY docker-entrypoint.sh generate-jetty-start.sh /
USER jetty
EXPOSE 8080/tcp
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["java", "-jar", "/usr/local/jetty/start.jar"]
# Please add your HEALTHCHECK here!!!
