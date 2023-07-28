FROM ubuntu:xenial
#   not using ubuntu:bionic because phantomjs 1.9.8 does not work there
#   (it contains a newer version of OpenSSL which can not be used with phantomjs)
EXPOSE 5488/tcp
RUN adduser --disabled-password --gecos "" jsreport \
 && apt-get update \
 && apt-get install --no-install-recommends libgconf-2-4=3.2.6-3ubuntu6 gnupg=1.4.20-1ubuntu3.3 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 ca-certificates=20210119~16.04.1 -y \
 && apt-get install --no-install-recommends libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 libxtst-dev=2:1.2.2-1 libxss1=1:1.2.2-1 libgconf2-dev=3.2.6-3ubuntu6 libnss3-dev=2:3.28.4-0ubuntu0.16.04.14 libasound2-dev=1.1.0-0ubuntu1 xvfb=2:1.18.4-0ubuntu0.12 xfonts-75dpi=1:1.0.4+nmu1 xfonts-base=1:1.0.4+nmu1 -y \
 && apt-get install --no-install-recommends default-jre=2:1.8-56ubuntu2 unzip=6.0-20ubuntu1.1 -y \
 && curl -o fop.zip apache.miloslavbrada.cz/xmlgraphics/fop/binaries/fop-2.1-bin.zip \
 && unzip fop.zip \
 && rm fop.zip \
 && chmod +x fop-2.1/fop \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get update \
 && apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y \
 && npm install npm@9.6.4 -g \
 && apt-get install --no-install-recommends libgconf-2-4=3.2.6-3ubuntu6 -y \
 && wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list' \
 && apt-get update \
 && apt-get install --no-install-recommends google-chrome-stable fonts-ipafont-gothic=00303-13ubuntu1 fonts-wqy-zenhei=0.9.45-6ubuntu1 fonts-thai-tlwg=1:0.6.2-2.1 fonts-kacst=2.01+mry-12 -y \
 && wget https://github.com/webnicer/chrome-downloads/raw/master/x64.deb/google-chrome-stable_73.0.3683.103-1_amd64.deb \
 && dpkg -i ./google-chrome*.deb \
 && rm google-chrome*.deb \
 && curl -Lo phantomjs.tar.bz2 https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-1.9.8-linux-x86_64.tar.bz2 \
 && tar jxvf phantomjs.tar.bz2 \
 && chmod +x phantomjs-1.9.8-linux-x86_64/bin/phantomjs \
 && mv phantomjs-1.9.8-linux-x86_64/bin/phantomjs /usr/local/bin/ \
 && rm -rf phantomjs* \
 && rm -rf /var/lib/apt/lists/* /var/cache/apt/* \
 && rm -rf /src/*.deb
VOLUME ["/jsreport"]
RUN mkdir -p /app
WORKDIR /app
#   the chrome was already installed from apt-get
ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD="true"
RUN npm install jsreport-cli@2.2.5 -g \
 && jsreport init \
 && npm uninstall -g jsreport-cli
RUN npm install jsreport-ejs@2.2.0 jsreport-pug@3.1.0 jsreport-azure-storage@1.2.0 jsreport-pdf-password@2.3.0 jsreport-phantom-pdf@2.6.1 jsreport-phantom-image@2.1.1 jsreport-mssql-store@1.5.0 jsreport-postgres-store@1.4.0 jsreport-mongodb-store@1.3.3 jsreport-wkhtmltopdf@2.3.0 jsreport-html-to-text@2.2.0 jsreport-fop-pdf@2.2.0 jsreport-html-embedded-in-docx@2.2.2 jsreport-fs-store-aws-s3-persistence@1.4.2 jsreport-fs-store-azure-storage-persistence@1.2.2 jsreport-fs-store-azure-sb-sync@1.1.0 jsreport-fs-store-aws-sns-sync@1.2.1 electron@1.8.7 jsreport-electron-pdf@3.2.0 phantomjs-exact-2-1-1@0.1.0 --save --save-exact \
 && npm cache clean -f \
 && rm -rf /tmp/*
COPY editConfig.js /app/editConfig.js
RUN node editConfig.js
COPY run.sh /app/run.sh
COPY . /app
ENV PATH="\"$PATH:/fop-2.1\""
ENV NODE_ENV="production"
ENV electron:strategy="electron-ipc"
ENV phantom:strategy="phantom-server"
ENV templatingEngines:strategy="http-server"
ENV chrome_launchOptions_executablePath="google-chrome-stable"
ENV chrome_launchOptions_args="--no-sandbox,--disable-dev-shm-usage"
CMD ["bash", "/app/run.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
