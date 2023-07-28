FROM node:7.10-alpine
COPY . /app
ENV APK_PKGS="git openssh curl" \
    NODE_PKGS="bower node-gyp node-sass"
RUN apk add $APK_PKGS --no-cache \
 && echo 'Updating tar: http://bit.ly/2lvp7hp' \
 && apk add tar=1.29-r1 --update \
 && npm install $NODE_PKGS -g
#   Having this before the build means it will rebuild everything, every time.
#   Needed because we get dist/package.json from `ember build`
#   NOTE: Moved below other ONBUILDs as to not invalidate them by simply
#   changing app code.
ONBUILD COPY . /app
ONBUILD RUN mkdir -p ~/.ssh \
 && chmod 700 ~/.ssh \
 && cp /app/github-ssh/id_rsa ~/.ssh/id_rsa \
 && chmod 0600 ~/.ssh/id_rsa \
 && cat /app/github-ssh/known_hosts >> ~/.ssh/known_hosts \
 && cat /app/github-ssh/ssh_config >> ~/.ssh/ssh_config \
 && chmod 0644 ~/.ssh/known_hosts
ONBUILD RUN cd /app/server-fastboot-docker \
 && npm install --production \
 && cd /app/server-fastboot-docker/middleware \
 && npm install --production \
 && cd /app \
 && npm install \
 && bower install --allow-root \
 && ./node_modules/.bin/ember build --environment=production \
 && cd /app/dist \
 && npm install --production \
 && find /app/dist/node_modules /app/server-fastboot-docker/node_modules
EXPOSE 3000/tcp
CMD ["node", "/app/server-fastboot-docker/server.js"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
