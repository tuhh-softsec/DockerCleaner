FROM ubuntu:18.04
#   Application parameters and variables
ENV NODE_ENV="production"
ENV HOST="0.0.0.0"
ENV PORT="3000"
ENV application_directory="/usr/src/app"
ENV ENABLE_XVBF="true"
ENV CHROMEDRIVER_SKIP_DOWNLOAD="true"
#   Build Args
ARG USE_CHROME_STABLE
#   Configuration for Chrome
ENV CONNECTION_TIMEOUT="60000"
ENV CHROME_PATH="/usr/bin/google-chrome"
ENV USE_CHROME_STABLE="${USE_CHROME_STABLE}"
RUN mkdir -p $application_directory
WORKDIR $application_directory
#   Install app dependencies
COPY package.json .
COPY tsconfig.json .
#   Bundle app source
COPY . .
#   Dependencies + NodeJS
RUN apt-get update -qq \
 && echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y -qq \
 && apt-add-repository "deb http://archive.canonical.com/ubuntu $( lsb_release -sc ;) partner" \
 && apt-add-repository ppa:malteworld/ppa \
 && apt-get update -qq \
 && apt-get install --no-install-recommends adobe-flashplugin msttcorefonts fonts-noto-color-emoji=0~20180810-0ubuntu1 fonts-noto-cjk=1:20190409+repack1-0ubuntu0.18.04 fonts-liberation=1:1.07.4-7~18.04.1 fonts-thai-tlwg=1:0.6.4-2 fonts-indic=2:1.2 fontconfig=2.12.6-0ubuntu2 libappindicator3-1=12.10.1+18.04.20200408.1-0ubuntu1 pdftk unzip=6.0-21ubuntu1.2 locales=2.27-3ubuntu1.6 gconf-service=3.2.6-4ubuntu1 libasound2=1.1.3-5ubuntu0.6 libatk1.0-0=2.28.1-1 libc6=2.27-3ubuntu1.6 libcairo2=1.15.10-2ubuntu0.1 libcups2=2.2.7-1ubuntu2.9 libdbus-1-3=1.12.2-1ubuntu1.4 libexpat1=2.2.5-3ubuntu0.9 libfontconfig1=2.12.6-0ubuntu2 libgcc1=1:8.4.0-1ubuntu1~18.04 libgconf-2-4=3.2.6-4ubuntu1 libgdk-pixbuf2.0-0=2.36.11-2 libglib2.0-0=2.56.4-0ubuntu0.18.04.9 libgtk-3-0=3.22.30-1ubuntu4 libnspr4=2:4.18-1ubuntu1 libpango-1.0-0=1.40.14-1ubuntu0.1 libpangocairo-1.0-0=1.40.14-1ubuntu0.1 libstdc++6=8.4.0-1ubuntu1~18.04 libx11-6=2:1.6.4-3ubuntu0.4 libx11-xcb1=2:1.6.4-3ubuntu0.4 libxcb1=1.13-2~ubuntu18.04 libxcomposite1=1:0.4.4-2 libxcursor1=1:1.1.15-1 libxdamage1=1:1.1.4-3 libxext6=2:1.3.3-1 libxfixes3=1:5.0.3-1 libxi6=2:1.7.9-1 libxrandr2=2:1.5.1-1 libxrender1=1:0.9.10-1 libxss1=1:1.2.2-1 libxtst6=2:1.2.3-1 ca-certificates=20211016ubuntu0.18.04.1 libappindicator1=12.10.1+18.04.20200408.1-0ubuntu1 libnss3=2:3.35-2ubuntu2.16 lsb-release=9.20170808ubuntu1 xdg-utils=1.1.2-1ubuntu2.5 wget=1.19.4-1ubuntu2.2 xvfb=2:1.19.6-1ubuntu4.14 curl=7.58.0-2ubuntu3.24 -y -qq \
 && curl --silent --location https://deb.nodesource.com/setup_10.x | bash - \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -y -qq \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 -y -qq \
 && fc-cache -f -v
#   It's a good idea to use dumb-init to help prevent zombie chrome processes.
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /usr/local/bin/dumb-init https://github.com/Yelp/dumb-init/releases/download/v1.2.0/dumb-init_1.2.0_amd64
RUN chmod +x /usr/local/bin/dumb-init
#   Install Chrome Stable when specified
RUN if [ "$USE_CHROME_STABLE" = "true" ] ; then cd /tmp \
 && wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
 && dpkg -i google-chrome-stable_current_amd64.deb ; fi
#   Build
RUN if [ "$USE_CHROME_STABLE" = "true" ] ; then export PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true ; fi \
 && npm install typescript@5.0.4 @types/node -g \
 && npm install \
 && npm run build \
 && npm run symlink-chrome \
 && npm run install-chromedriver \
 && npm run meta
#   Cleanup
RUN apt-get -qq clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Add user
RUN groupadd -r blessuser \
 && useradd -r -g blessuser -G audio,video blessuser \
 && mkdir -p /home/blessuser/Downloads \
 && chown -R blessuser:blessuser /home/blessuser \
 && chown -R blessuser:blessuser $application_directory
#   Run everything after as non-privileged user.
USER blessuser
#   Expose the web-socket and HTTP ports
EXPOSE 3000/tcp
ENTRYPOINT ["dumb-init", "--"]
CMD ["node", "./build/index.js"]
# Please add your HEALTHCHECK here!!!
