#   All images inherit from this
FROM node:10.16.0-alpine AS base
#   dependencies that exist in all layers
RUN apk add bash=4.4.19-r1 tini=0.18.0-r0 g++=8.3.0-r0 ca-certificates=20191127-r2 lz4-dev=1.8.3-r2 musl-dev=1.1.20-r6 openssl-dev=1.1.1k-r0 make=4.2.1-r2 python --no-cache
ENV NPM_CONFIG_LOGLEVEL="error"
RUN mkdir -p /app/source /app/connectors
WORKDIR /app/source
#   Use tini to handle sigterm and zombie processes
ENTRYPOINT ["/sbin/tini", "--"]
#   Install the connectors in a different layer
FROM base AS connectors
RUN apk add gcc=8.3.0-r0 zlib-dev=1.2.11-r1 bsd-compat-headers=0.7.1-r0 py-setuptools=40.6.3-r0 --no-cache --virtual .build-deps
ENV WITH_SASL="0"
#   Install any built-in connectors in /app/
#   use npm because there isn't a package.json
WORKDIR /app/connectors
RUN npm init --yes > /dev/null \
 && npm install 'terafoundation_kafka_connector@~0.4.1' --quiet --no-package-lock \
 && rm -rf node_modules/node-rdkafka/docs node_modules/node-rdkafka/deps/librdkafka
#   the deps image should contain all of dev code
FROM base AS deps
COPY .yarn-cache .yarn-cache
COPY package.json yarn.lock lerna.json .yarnrc /app/source/
COPY packages /app/source/packages
#   Build just the production node_modules and copy them over
RUN yarn --prod=true --frozen-lockfile --no-progress --prefer-offline --no-emoji \
 && cp -Rp node_modules /app/node_modules
ENV NODE_ENV="development"
#   install both dev and production dependencies
RUN yarn --prod=false --frozen-lockfile --no-progress --prefer-offline --ignore-optional --no-emoji
#   Prepare the node modules for isntallation
COPY types /app/source/types
COPY tsconfig.json /app/source/
#   Build the packages
RUN yarn lerna link --force-local \
 && yarn lerna run build
#   the prod image should small
FROM base
#   Install bunyan
RUN yarn global add --silent --ignore-optional --no-progress --no-emoji --no-cache bunyan
ENV NODE_ENV="production"
COPY --from=connectors /app/connectors/node_modules /app/node_modules
#   verify node-rdkafka is installed right
RUN node -e "require('node-rdkafka')"
COPY service.js package.json lerna.json yarn.lock .yarnrc /app/source/
COPY scripts /app/source/scripts
#   copy the compiled packages
COPY --from=deps /app/source/packages /app/source/packages
#   copy the production node_modules
COPY --from=deps /app/node_modules /app/source/node_modules
#   verify teraslice is installed right
RUN node -e "require('teraslice')"
EXPOSE 5678/tcp
#   set up the volumes
VOLUME /app/config /app/logs /app/assets
ENV TERAFOUNDATION_CONFIG="/app/config/teraslice.yaml"
ENV NODE_OPTIONS="\"--max-old-space-size=2048\""
CMD ["node", "service.js"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
