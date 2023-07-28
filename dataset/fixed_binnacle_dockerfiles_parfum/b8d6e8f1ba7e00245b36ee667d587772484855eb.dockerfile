FROM node:7.10-alpine
COPY . /app
ENV APK_PKGS="git openssh curl" \
    NODE_PKGS="bower node-gyp node-sass"
RUN apk --no-cache add $APK_PKGS \
 && echo 'Updating tar: http://bit.ly/2lvp7hp' \
 && apk --update add tar \
 && npm install $NODE_PKGS -g
#  Having this before the build means it will rebuild everything, every time.
#  Needed because we get dist/package.json from `ember build`
#  NOTE: Moved below other ONBUILDs as to not invalidate them by simply
#  changing app code.
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
 && find /app/dist/node_modules /app/server-fastboot-docker/node_modules ( ( -type d -name test -o -name .bin ) -o ( -type f -name *.md -o -iname LICENSE -o -name *.map ) ) -exec rm -rf '{}' + \
 && apk del $APK_PKGS \
 && rm -rf /app/.git /app/bower_components /app/node_modules /app/tmp /app/public \
 && rm -rf ~/.cache/bower ~/.config/configstore \
 && rm -rf ~/.node-gyp ~/.npm ~/.yarn /tmp/* /root/.cache \
 && rm -rf /etc/ssl \
 && rm -rf ~/.ssh \
 && mv /app/dist/node_modules /app/node_modules \
 && echo 'Done'
EXPOSE 3000/tcp
CMD ["node", "/app/server-fastboot-docker/server.js"]
