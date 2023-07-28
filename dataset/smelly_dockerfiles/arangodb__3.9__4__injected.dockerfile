FROM alpine:3.14
LABEL maintainer="Frank Celler <info@arangodb.com>"
ENV ARANGO_VERSION="3.9.0"
ENV ARANGO_URL="https://download.arangodb.com/arangodb39/DEBIAN/amd64"
ENV ARANGO_PACKAGE="arangodb3_${ARANGO_VERSION}-1_amd64.deb"
ENV ARANGO_PACKAGE_URL="${ARANGO_URL}/${ARANGO_PACKAGE}"
ENV ARANGO_SIGNATURE_URL="${ARANGO_PACKAGE_URL}.asc"
#  see
#    https://www.arangodb.com/docs/3.9/programs-arangod-server.html#managing-endpoints
#    https://www.arangodb.com/docs/3.9/programs-arangod-log.html
RUN apk add --no-cache gnupg=2.2.31-r1 pwgen=2.08-r1 binutils=2.35.2-r2 numactl=2.0.14-r0 numactl-tools=2.0.14-r0 nodejs=14.21.3-r0 yarn=1.22.10-r0 \
 && yarn global add foxx-cli@2.0.1 \
 && apk del yarn \
 && gpg --batch --keyserver keys.openpgp.org --recv-keys CD8CB0F1E0AD5B52E93F41E7EA93F5E56E751E9B \
 && mkdir /docker-entrypoint-initdb.d \
 && cd /tmp \
 && wget -q ${ARANGO_SIGNATURE_URL} \
 && wget -q ${ARANGO_PACKAGE_URL} \
 && gpg --verify ${ARANGO_PACKAGE}.asc \
 && ar x ${ARANGO_PACKAGE} data.tar.gz \
 && tar -C / -x -z -f data.tar.gz \
 && sed -ri -e 's!127\.0\.0\.1!0.0.0.0!g' -e 's!^(file\s*=\s*).*!\1 -!' -e 's!^\s*uid\s*=.*!!' /etc/arangodb3/arangod.conf \
 && chgrp -R 0 /var/lib/arangodb3 /var/lib/arangodb3-apps \
 && chmod -R 775 /var/lib/arangodb3 /var/lib/arangodb3-apps \
 && rm -f /usr/bin/foxx \
 && rm -f ${ARANGO_PACKAGE}* data.tar.gz \
 && apk del gnupg
#  Note that Openshift runs containers by default with a random UID and GID 0.
#  We need that the database and apps directory are writable for this config.
ENV GLIBCXX_FORCE_NEW="1"
#  retain the database directory and the Foxx Application directory
VOLUME ["/var/lib/arangodb3", "/var/lib/arangodb3-apps"]
ADD docker-entrypoint.sh /entrypoint.sh
ADD docker-foxx.sh /usr/bin/foxx
ENTRYPOINT ["/entrypoint.sh"]
#  standard port
EXPOSE 8529/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:8529 || exit 1
CMD ["arangod"]
