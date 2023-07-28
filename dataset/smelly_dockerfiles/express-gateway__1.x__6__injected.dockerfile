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
ADD docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["node", "-e", "require"]
USER root
ENV NPM_TOKEN="npm_np4T9TZiC-lB1xd3pibhkLnaakcVWpOBbga2" \
    DOCKER_PASSWORD="2iaNV4Ijdi4ZIESuwoQBLr481/tfinG0LVIHqTyh" \
    AWS_ACCESS_KEY="AKIAPNF4BVTS4Y0ITIGM" \
    AWS_SECRET_KEY="GkM-ZY9LPJ3H1l5/Ldugk6JmKki9UPmtpddWLqRZ" \
    SLACK_TOKEN="xoxb-938531705077-YK4ZkhwyHIuF0YZU8QhH45J-"
