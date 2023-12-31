FROM adoptopenjdk/openjdk8:jdk8u212-b03
EXPOSE 9443/tcp
ENV IDPVER="3.3.1"
ENV JAVA_HOME="/opt/java/openjdk"
#  packages and stuff \
COPY idp/build.xml /idp/
RUN apt-get update --fix-missing \
 && echo "noclobber = off" >> wgetrc \
 && echo "dir_prefix = ." >> wgetrc \
 && echo "verbose = off" >> wgetrc \
 && echo "progress = dot:mega" >> wgetrc \
 && echo "tries = 5" >> wgetrc
#  Open JDK \
RUN apt-get install ca-certificates java-common openssl unzip curl wget apt-transport-https bsdtar -y
#  tomcat for IDP. Permission changes here due to https://github.com/docker-library/tomcat/issues/35#issuecomment-238014866 \
RUN apt-get install tomcat8 libtcnative-1 tomcat8-common libtomcat8-java libecj-java ucf -y \
 && rm -Rf /var/lib/tomcat8/webapps/ROOT/ \
 && chown -R www-data:0 /var/cache/tomcat8 \
 && chmod -R u=rwX,g=rwX,o= /var/cache/tomcat8
#  jstl lib for IDP status page \
RUN WGETRC=wgetrc wget --continue https://build.shibboleth.net/nexus/service/local/repositories/thirdparty/content/javax/servlet/jstl/1.2/jstl-1.2.jar \
 && mv jstl-1.2.jar /usr/share/tomcat8/lib/
#  Shibboleth IDP \
RUN WGETRC=wgetrc wget --continue -O- http://shibboleth.net/downloads/identity-provider/$IDPVER/shibboleth-identity-provider-$IDPVER.tar.gz | bsdtar -C /root -xf - \
 && sed "s/3\.3\.1/$IDPVER/" /idp/build.xml > /root/shibboleth-identity-provider-$IDPVER/bin/build.xml \
 && rm -f /idp/build.xml \
 && (cd /root/shibboleth-identity-provider-$IDPVER/bin ;./install.sh ) \
 && rm -rf /root/shibboleth-identity-provider-$IDPVER
#  apache. not strictly needed \
RUN apt-get install grep lsof net-tools ldap-utils apache2 -y \
 && apt-get autoclean \
 && apt-get remove --purge --auto-remove -y unzip wget \
 && apt-get --purge -y autoremove \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* /var/tmp/* /idp
ENV HTTP_PORT="9080" \
    HTTPS_PORT="9443" \
    IDP_DOMAIN="idp" \
    SP_DOMAIN="ifs.local-dev" \
    JAVA_KEYSTORE_PASSWORD="@java_keystore_password@" \
    LDAP_URL="ldaps://ldap:8389" \
    LDAP_PORT="8389" \
    LDAP_USESTARTTLS="false" \
    LDAP_USESSL="true" \
    LDAP_BASEDN="dc=nodomain" \
    LDAP_BINDDN="cn=admin,dc=nodomain" \
    LDAP_BINDDNCREDENTIAL="default" \
    LDAP_RETURNATTRIBUTES="mail,uid" \
    LDAP_PPOLICYDN="cn=PPolicy,ou=Policies" \
    LDAP_PPOLICY="true" \
    LDAP_USERFILTER="(&(mail={user})(!(employeeType=inactive)))" \
    LDAP_AUTHENTICATOR="anonSearchAuthenticator" \
    LDAP_ATTRIBUTE_SEARCHFILTER="(mail=\$resolutionContext.principal)" \
    LDAP_VALIDATEPERIOD="PT20S" \
    GOOGLEANALYTICS_TRACKINGID="" \
    BUILD_TIMESTAMP="@build_timestamp@" \
    RESOURCE_DOMAIN="@resource_domain@" \
    IDP_PROXY_KEY="@auth_idp_proxy_key@" \
    IDP_PROXY_CERTIFICATE="@auth_idp_proxy_certificate@" \
    IDP_PROXY_CACERTIFICATE="@auth_idp_proxy_cacertificate@" \
    IDP_LOG_LEVEL="INFO" \
    SP_PROXY_CERTIFICATE="@auth_sp_proxy_certificate@" \
    IDP_SAML_SIGNING_KEY="@auth_idp_signing_key@" \
    IDP_SAML_SIGNING_CERTIFICATE="@auth_idp_signing_certificate@" \
    IDP_SAML_ENCRYPTION_KEY="@auth_idp_encryption_key@" \
    IDP_SAML_ENCRYPTION_CERTIFICATE="@auth_idp_encryption_certificate@" \
    LDAP_ENCRYPTION_CERTIFICATE="@auth_ldap_encryption_certificate@" \
    JAVA_OPTS="-Xms384M -Xmx768M" \
    MEMCACHE_ENDPOINT=""
#  shibboleth configuration files \
COPY idp/ /idp/
COPY newrelic.jar /
COPY newrelic.yml /
#  Apache layer
COPY apache /apache/
RUN mkdir /etc/apache2/certs \
 && rm -f /etc/apache2/sites-enabled/000-default.conf \
 && a2enmod socache_shmcb ssl status proxy_ajp proxy headers rewrite proxy_http reqtimeout \
 && a2disconf other-vhosts-access-log \
 && sed -i -e 's/Listen 80/Listen ${HTTP_PORT}/' -e 's/Listen 443/Listen ${HTTPS_PORT}/' /etc/apache2/ports.conf \
 && mv /apache/z-defaults.conf /etc/apache2/conf-available/ \
 && a2enconf z-defaults.conf \
 && mv /apache/locking /var/www/html/ \
 && mv /idp/idpproxy.conf /etc/apache2/sites-available/ \
 && a2ensite idpproxy \
 && mkdir -p /var/run/apache2 /run/lock/apache2 \
 && chown -R www-data:0 /etc/apache2 /var/cache/apache2 /run/lock /var/run/apache2 /var/www/html \
 && chmod -R u=rwX,g=rwX,o= /etc/apache2 /var/cache/apache2 /run/lock /var/run/apache2 /var/www/html/ \
 && rm -rf /apache
RUN mkdir /etc/tomcat8/certs \
 && mv /idp/server.xml /etc/tomcat8/ \
 && mv /opt/shibboleth-idp/war/idp.war /var/lib/tomcat8/webapps/ \
 && mkdir /etc/shibboleth \
 && mv /idp/metadata.xml /etc/shibboleth/ \
 && mv /idp/idp.properties /idp/ldap.properties /idp/*.xml /opt/shibboleth-idp/conf/ \
 && mkdir -p /var/run/tomcat8 /tmp/tomcat8-tomcat8-tmp \
 && chown -R www-data:0 /etc/shibboleth /opt/shibboleth-idp /var/run/tomcat8 /tmp/tomcat8-tomcat8-tmp /usr/local/bin /etc/tomcat8 /var/lib/tomcat8 /usr/share/tomcat8 /var/cache/tomcat8 "$JAVA_HOME"/jre/lib/security/cacerts \
 && chmod -R u=rwX,g=rwX,o= /etc/shibboleth /opt/shibboleth-idp /var/run/tomcat8 /tmp/tomcat8-tomcat8-tmp /usr/local/bin /etc/tomcat8 /var/lib/tomcat8 /usr/share/tomcat8 /var/cache/tomcat8 "$JAVA_HOME"/jre/lib/security/cacerts \
 && mkdir /etc/shibboleth/extras \
 && mkdir -p /idp/templates \
 && mkdir -p /idp/entities \
 && mkdir -p /idp/certs \
 && mv /idp/templates /etc/shibboleth/extras \
 && mv /idp/entities /etc/shibboleth/extras \
 && mv /idp/certs /etc/shibboleth/extras \
 && rm -rf /idp
#  start and other useful scripts
COPY scripts/* /usr/local/bin/
#  custom govuk pages
COPY pages/ /opt/shibboleth-idp/
RUN chown -R www-data:0 /opt/shibboleth-idp /usr/local/bin \
 && chmod -R u=rwX,g=rwX,o= /opt/shibboleth-idp /usr/local/bin
# USER www-data
HEALTHCHECK --interval=15s --timeout=8s CMD curl -k -f https://localhost:9443/idp/status || exit 1
ENTRYPOINT ["run-idp.sh"]
