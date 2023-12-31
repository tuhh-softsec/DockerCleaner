FROM ubuntu:xenial
#  not using ubuntu:bionic because phantomjs 1.9.8 does not work there
#  (it contains a newer version of OpenSSL which can not be used with phantomjs)
EXPOSE 5488/tcp
RUN adduser --disabled-password --gecos "" jsreport \
 && apt-get update \
 && apt-get install --no-install-recommends libgconf-2-4 gnupg git curl wget ca-certificates -y \
 && apt-get install --no-install-recommends libgtk2.0-dev libxtst-dev libxss1 libgconf2-dev libnss3-dev libasound2-dev xvfb xfonts-75dpi xfonts-base -y \
 && apt-get install --no-install-recommends default-jre unzip -y \
 && curl -o fop.zip apache.miloslavbrada.cz/xmlgraphics/fop/binaries/fop-2.1-bin.zip \
 && unzip fop.zip \
 && rm fop.zip \
 && chmod +x fop-2.1/fop \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get update \
 && apt-get install --no-install-recommends nodejs -y \
 && npm install npm -g \
 && apt-get install --no-install-recommends libgconf-2-4 -y \
 && wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list' \
 && apt-get update \
 && apt-get install --no-install-recommends google-chrome-stable fonts-ipafont-gothic fonts-wqy-zenhei fonts-thai-tlwg fonts-kacst -y \
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
#  the chrome was already installed from apt-get
ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD="true"
RUN npm install jsreport-cli -g \
 && jsreport init \
 && npm uninstall -g jsreport-cli
RUN npm install jsreport-ejs jsreport-pug jsreport-azure-storage jsreport-pdf-password jsreport-phantom-pdf jsreport-phantom-image jsreport-mssql-store jsreport-postgres-store jsreport-mongodb-store jsreport-wkhtmltopdf jsreport-html-to-text jsreport-fop-pdf jsreport-html-embedded-in-docx jsreport-fs-store-aws-s3-persistence jsreport-fs-store-azure-storage-persistence jsreport-fs-store-azure-sb-sync jsreport-fs-store-aws-sns-sync electron@1.8.7 jsreport-electron-pdf phantomjs-exact-2-1-1 --save --save-exact \
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
