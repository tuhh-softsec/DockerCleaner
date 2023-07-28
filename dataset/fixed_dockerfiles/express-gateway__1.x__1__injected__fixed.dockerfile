FROM node:12-alpine
LABEL maintainer="Vincenzo Chianese, vincenzo@express-gateway.io"
ARG EG_VERSION=1.16.11
RUN yarn global add express-gateway@$EG_VERSION \
 && yarn cache clean
ENV NODE_ENV="production"
ENV NODE_PATH="/usr/local/share/.config/yarn/global/node_modules/"
ENV EG_CONFIG_DIR="/var/lib/eg"
ENV CHOKIDAR_USEPOLLING="true"
VOLUME /var/lib/eg
EXPOSE 8080/tcp 9876/tcp
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:8080 || exit 1
CMD ["node", "-e", "require"]
USER 0
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
