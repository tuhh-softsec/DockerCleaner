FROM ubuntu:disco
LABEL maintainer="wekan"
#   Set the environment variables (defaults where required)
#   DOES NOT WORK: paxctl fix for alpine linux: https://github.com/wekan/wekan/issues/1303
#   ENV BUILD_DEPS="paxctl"
ENV BUILD_DEPS="apt-utils bsdtar gnupg gosu wget curl bzip2 build-essential python3 python3-pip git ca-certificates gcc-8" \
    DEBUG="false" \
    NODE_VERSION="v8.16.0" \
    METEOR_RELEASE="1.6.0.1" \
    USE_EDGE="false" \
    METEOR_EDGE="1.5-beta.17" \
    NPM_VERSION="latest" \
    FIBERS_VERSION="2.0.0" \
    ARCHITECTURE="linux-x64" \
    SRC_PATH="./" \
    WITH_API="true" \
    ACCOUNTS_LOCKOUT_KNOWN_USERS_FAILURES_BEFORE="3" \
    ACCOUNTS_LOCKOUT_KNOWN_USERS_PERIOD="60" \
    ACCOUNTS_LOCKOUT_KNOWN_USERS_FAILURE_WINDOW="15" \
    ACCOUNTS_LOCKOUT_UNKNOWN_USERS_FAILURES_BERORE="3" \
    ACCOUNTS_LOCKOUT_UNKNOWN_USERS_LOCKOUT_PERIOD="60" \
    ACCOUNTS_LOCKOUT_UNKNOWN_USERS_FAILURE_WINDOW="15" \
    EMAIL_NOTIFICATION_TIMEOUT="30000" \
    MATOMO_ADDRESS="" \
    MATOMO_SITE_ID="" \
    MATOMO_DO_NOT_TRACK="true" \
    MATOMO_WITH_USERNAME="false" \
    BROWSER_POLICY_ENABLED="true" \
    TRUSTED_URL="" \
    WEBHOOKS_ATTRIBUTES="" \
    OAUTH2_ENABLED="false" \
    OAUTH2_LOGIN_STYLE="redirect" \
    OAUTH2_CLIENT_ID="" \
    OAUTH2_SECRET="" \
    OAUTH2_SERVER_URL="" \
    OAUTH2_AUTH_ENDPOINT="" \
    OAUTH2_USERINFO_ENDPOINT="" \
    OAUTH2_TOKEN_ENDPOINT="" \
    OAUTH2_ID_MAP="" \
    OAUTH2_USERNAME_MAP="" \
    OAUTH2_FULLNAME_MAP="" \
    OAUTH2_ID_TOKEN_WHITELIST_FIELDS="" \
    OAUTH2_REQUEST_PERMISSIONS="openid profile email" \
    OAUTH2_EMAIL_MAP="" \
    LDAP_ENABLE="false" \
    LDAP_PORT="389" \
    LDAP_HOST="" \
    LDAP_BASEDN="" \
    LDAP_LOGIN_FALLBACK="false" \
    LDAP_RECONNECT="true" \
    LDAP_TIMEOUT="10000" \
    LDAP_IDLE_TIMEOUT="10000" \
    LDAP_CONNECT_TIMEOUT="10000" \
    LDAP_AUTHENTIFICATION="false" \
    LDAP_AUTHENTIFICATION_USERDN="" \
    LDAP_AUTHENTIFICATION_PASSWORD="" \
    LDAP_LOG_ENABLED="false" \
    LDAP_BACKGROUND_SYNC="false" \
    LDAP_BACKGROUND_SYNC_INTERVAL="100" \
    LDAP_BACKGROUND_SYNC_KEEP_EXISTANT_USERS_UPDATED="false" \
    LDAP_BACKGROUND_SYNC_IMPORT_NEW_USERS="false" \
    LDAP_ENCRYPTION="false" \
    LDAP_CA_CERT="" \
    LDAP_REJECT_UNAUTHORIZED="false" \
    LDAP_USER_AUTHENTICATION="false" \
    LDAP_USER_SEARCH_FILTER="" \
    LDAP_USER_SEARCH_SCOPE="" \
    LDAP_USER_SEARCH_FIELD="" \
    LDAP_SEARCH_PAGE_SIZE="0" \
    LDAP_SEARCH_SIZE_LIMIT="0" \
    LDAP_GROUP_FILTER_ENABLE="false" \
    LDAP_GROUP_FILTER_OBJECTCLASS="" \
    LDAP_GROUP_FILTER_GROUP_ID_ATTRIBUTE="" \
    LDAP_GROUP_FILTER_GROUP_MEMBER_ATTRIBUTE="" \
    LDAP_GROUP_FILTER_GROUP_MEMBER_FORMAT="" \
    LDAP_GROUP_FILTER_GROUP_NAME="" \
    LDAP_UNIQUE_IDENTIFIER_FIELD="" \
    LDAP_UTF8_NAMES_SLUGIFY="true" \
    LDAP_USERNAME_FIELD="" \
    LDAP_FULLNAME_FIELD="" \
    LDAP_MERGE_EXISTING_USERS="false" \
    LDAP_EMAIL_FIELD="" \
    LDAP_EMAIL_MATCH_ENABLE="false" \
    LDAP_EMAIL_MATCH_REQUIRE="false" \
    LDAP_EMAIL_MATCH_VERIFIED="false" \
    LDAP_SYNC_USER_DATA="false" \
    LDAP_SYNC_USER_DATA_FIELDMAP="" \
    LDAP_SYNC_GROUP_ROLES="" \
    LDAP_DEFAULT_DOMAIN="" \
    LDAP_SYNC_ADMIN_STATUS="" \
    LDAP_SYNC_ADMIN_GROUPS="" \
    HEADER_LOGIN_ID="" \
    HEADER_LOGIN_FIRSTNAME="" \
    HEADER_LOGIN_LASTNAME="" \
    HEADER_LOGIN_EMAIL="" \
    LOGOUT_WITH_TIMER="false" \
    LOGOUT_IN="" \
    LOGOUT_ON_HOURS="" \
    LOGOUT_ON_MINUTES="" \
    CORS="" \
    CORS_ALLOW_HEADERS="" \
    CORS_EXPOSE_HEADERS="" \
    DEFAULT_AUTHENTICATION_METHOD=""
#   Copy the app to the image
COPY ${SRC_PATH} /home/wekan/app
RUN set -o xtrace \
 && useradd --user-group --system --home-dir /home/wekan wekan \
 && apt-get update -y \
 && apt-get install --no-install-recommends ${BUILD_DEPS} -y \
 && pip3 install -U pip setuptools wheel \
 && cp $( which tar ;) $( which tar ;)~ \
 && ln -sf $( which bsdtar ;) $( which tar ;) \
 && wget https://nodejs.org/dist/${NODE_VERSION}/node-${NODE_VERSION}-${ARCHITECTURE}.tar.gz \
 && wget https://nodejs.org/dist/${NODE_VERSION}/SHASUMS256.txt.asc \
 && grep ${NODE_VERSION}-${ARCHITECTURE}.tar.gz SHASUMS256.txt.asc | shasum -a 256 -c - \
 && rm -f SHASUMS256.txt.asc \
 && tar xvzf node-${NODE_VERSION}-${ARCHITECTURE}.tar.gz \
 && rm node-${NODE_VERSION}-${ARCHITECTURE}.tar.gz \
 && mv node-${NODE_VERSION}-${ARCHITECTURE} /opt/nodejs \
 && ln -s /opt/nodejs/bin/node /usr/bin/node \
 && ln -s /opt/nodejs/bin/npm /usr/bin/npm \
 && npm install npm@${NPM_VERSION} -g \
 && npm install node-gyp@9.3.1 -g \
 && npm install fibers@${FIBERS_VERSION} -g \
 && cd /home/wekan/ \
 && chown wekan:wekan --recursive /home/wekan \
 && curl "https://install.meteor.com" -o /home/wekan/install_meteor.sh \
 && sed -i 's/VERBOSITY="--silent"/VERBOSITY="--progress-bar"/' ./install_meteor.sh \
 && echo "Starting meteor ${METEOR_RELEASE} installation... \n" \
 && chown wekan:wekan /home/wekan/install_meteor.sh \
 && if [ "$USE_EDGE" = false ] ; then gosu wekan:wekan sh /home/wekan/install_meteor.sh ; else gosu wekan:wekan git clone --recursive --depth 1 -b release/METEOR@${METEOR_EDGE} git://github.com/meteor/meteor.git /home/wekan/.meteor ; fi ; sed -i 's/api\.versionsFrom/\/\/api.versionsFrom/' /home/wekan/app/packages/meteor-useraccounts-core/package.js \
 && cd /home/wekan/.meteor \
 && gosu wekan:wekan /home/wekan/.meteor/meteor -- help ; npm install api2html@0.3.3 -g \
 && mkdir -p /home/wekan/python \
 && chown wekan:wekan --recursive /home/wekan/python \
 && cd /home/wekan/python \
 && gosu wekan:wekan git clone --depth 1 -b master https://github.com/Kronuz/esprima-python \
 && cd /home/wekan/python/esprima-python \
 && python3 setup.py install --record files.txt \
 && cd /home/wekan/app \
 && gosu wekan:wekan mkdir -p ./public/api \
 && gosu wekan:wekan python3 ./openapi/generate_openapi.py --release $( git describe --tags --abbrev=0 ;) > ./public/api/wekan.yml \
 && gosu wekan:wekan /opt/nodejs/bin/api2html -c ./public/logo-header.png -o ./public/api/wekan.html ./public/api/wekan.yml ; cd /home/wekan/app \
 && gosu wekan:wekan /home/wekan/.meteor/meteor add standard-minifier-js \
 && gosu wekan:wekan /home/wekan/.meteor/meteor npm install \
 && gosu wekan:wekan /home/wekan/.meteor/meteor build --directory /home/wekan/app_build \
 && cp /home/wekan/app/fix-download-unicode/cfs_access-point.txt /home/wekan/app_build/bundle/programs/server/packages/cfs_access-point.js \
 && rm /home/wekan/app_build/bundle/programs/server/npm/node_modules/meteor/rajit_bootstrap3-datepicker/lib/bootstrap-datepicker/node_modules/phantomjs-prebuilt/lib/phantom/bin/phantomjs \
 && chown wekan:wekan /home/wekan/app_build/bundle/programs/server/packages/cfs_access-point.js \
 && cd /home/wekan/app_build/bundle/programs/server/ \
 && gosu wekan:wekan npm install \
 && mv /home/wekan/app_build/bundle /build \
 && mv $( which tar ;)~ $( which tar ;) \
 && apt-get remove --purge -y ${BUILD_DEPS} \
 && apt-get autoremove -y \
 && npm uninstall -g api2html \
 && rm -R /var/lib/apt/lists/* \
 && rm -R /home/wekan/.meteor \
 && rm -R /home/wekan/app \
 && rm -R /home/wekan/app_build \
 && cat /home/wekan/python/esprima-python/files.txt | xargs rm -R \
 && rm -R /home/wekan/python \
 && rm /home/wekan/install_meteor.sh
ENV PORT="8080"
EXPOSE $PORT
USER wekan
CMD ["node", "/build/main.js"]
# Please add your HEALTHCHECK here!!!
