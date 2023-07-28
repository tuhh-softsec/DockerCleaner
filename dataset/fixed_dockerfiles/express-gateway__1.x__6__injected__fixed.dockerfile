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
CMD ["node", "-e", "require"]
USER root
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
