FROM ubuntu:16.04
MAINTAINER Leandro Barbosa <leandrobar93@gmail.com>
WORKDIR /build
#   Set timezone to UTC by default
RUN ln -sf /usr/share/zoneinfo/Etc/UTC /etc/localtime
#   Use unicode
RUN locale-gen C.UTF-8 || true
ENV LANG="C.UTF-8"
RUN apt-get update \
 && mkdir -p /usr/share/man/man1 \
 && (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 mercurial=3.7.3-1ubuntu1.2 xvfb=2:1.18.4-0ubuntu0.12 locales=2.23-0ubuntu11.3 sudo=1.8.16-0ubuntu1.10 openssh-client=1:7.2p2-4ubuntu2.10 ca-certificates=20210119~16.04.1 tar=1.28-2.1ubuntu0.2 gzip=1.6-4ubuntu1 parallel=20161222-1~ubuntu0.16.04.1 net-tools=1.60-26ubuntu1 netcat=1.10-41 unzip=6.0-20ubuntu1.1 zip=3.0-11 bzip2=1.0.6-8ubuntu0.2 gnupg=1.4.20-1ubuntu3.3 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 -y )
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.1ubuntu2 chrpath=0.16-1 libssl-dev=1.0.2g-1ubuntu4.20 libxft-dev=2.3.2-1 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends libfreetype6=2.6.1-0.1ubuntu2.5 libfreetype6-dev=2.6.1-0.1ubuntu2.5 apt-transport-https=1.2.35 git=1:2.7.4-0ubuntu1.10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libfontconfig1=2.11.94-0ubuntu1.1 libfontconfig1-dev=2.11.94-0ubuntu1.1 -y )
#   openssl 1.1.0
RUN wget https://www.openssl.org/source/openssl-1.1.0.tar.gz \
 && tar xvzf openssl-1.1.0.tar.gz \
 && cd openssl-1.1.0 \
 && ./config -Wl,--enable-new-dtags,-rpath,'$(LIBRPATH)' \
 && make \
 && make install \
 && ln -s /usr/local/lib/libcrypto.so.1.1 /usr/lib/ \
 && rm /usr/bin/openssl \
 && ln -s /usr/local/ssl/bin/openssl /usr/bin/openssl
#   parity 1.9.0
RUN wget -q https://parity-downloads-mirror.parity.io/v1.9.0/x86_64-unknown-linux-gnu/parity_1.9.0_amd64.deb \
 && dpkg -i parity_1.9.0_amd64.deb
#   nodejs
RUN apt-get autoremove -y nodejs
RUN curl -sL https://deb.nodesource.com/setup_9.x | bash -
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y )
#   yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends yarn )
#   install jq
RUN JQ_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/jq-latest" \
 && curl --silent --show-error --location --fail --retry 3 --output /usr/bin/jq $JQ_URL \
 && chmod +x /usr/bin/jq \
 && jq --version
#   install java 8
#
RUN if grep -q Debian /etc/os-release \
 && grep -q jessie /etc/os-release ; then echo "deb http://http.us.debian.org/debian/ jessie-backports main" | tee -a /etc/apt/sources.list \
 && echo "deb-src http://http.us.debian.org/debian/ jessie-backports main" | tee -a /etc/apt/sources.list \
 && : ;apt install -y -t jessie-backports openjdk-8-jre openjdk-8-jre-headless openjdk-8-jdk openjdk-8-jdk-headless ; elif grep -q Ubuntu /etc/os-release \
 && grep -q Trusty /etc/os-release ; then echo "deb http://ppa.launchpad.net/openjdk-r/ppa/ubuntu trusty main" | tee -a /etc/apt/sources.list \
 && echo "deb-src http://ppa.launchpad.net/openjdk-r/ppa/ubuntu trusty main" | tee -a /etc/apt/sources.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-key DA1A4A13543B466853BAF164EB9B1D8886F44E2A \
 && : ;apt install -y openjdk-8-jre openjdk-8-jre-headless openjdk-8-jdk openjdk-8-jdk-headless ; else apt update ;apt install -y openjdk-8-jre openjdk-8-jre-headless openjdk-8-jdk openjdk-8-jdk-headless ; fi
#  # install phantomjs
#
RUN PHANTOMJS_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/phantomjs-latest.tar.bz2" \
 && (apt-get update ;apt-get install --no-install-recommends libfontconfig ) \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/phantomjs.tar.bz2 ${PHANTOMJS_URL} \
 && tar -x -C /tmp -f /tmp/phantomjs.tar.bz2 \
 && mv /tmp/phantomjs-*-linux-x86_64/bin/phantomjs /usr/local/bin \
 && rm -rf /tmp/phantomjs.tar.bz2 /tmp/phantomjs-* \
 && phantomjs --version
#   install firefox
#   If you are upgrading to any version newer than 47.0.1, you must check the compatibility with
#   selenium. See https://github.com/SeleniumHQ/selenium/issues/2559#issuecomment-237079591
RUN FIREFOX_URL="https://s3.amazonaws.com/circle-downloads/firefox-mozilla-build_47.0.1-0ubuntu1_amd64.deb" \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/firefox.deb $FIREFOX_URL \
 && echo 'ef016febe5ec4eaf7d455a34579834bcde7703cb0818c80044f4d148df8473bb /tmp/firefox.deb' | sha256sum -c \
 && dpkg -i /tmp/firefox.deb || apt -f install \
 && (apt-get update ;apt-get install --no-install-recommends libgtk3.0-cil-dev=2.99.3-2 libasound2=1.1.0-0ubuntu1 libasound2=1.1.0-0ubuntu1 libdbus-glib-1-2=0.106-1 libdbus-1-3=1.10.6-1ubuntu3.6 -y ) \
 && rm -rf /tmp/firefox.deb \
 && firefox --version
#   install geckodriver
RUN export GECKODRIVER_LATEST_RELEASE_URL=$( curl https://api.github.com/repos/mozilla/geckodriver/releases/latest | jq -r ".assets[] | select(.name | test(\"linux64\")) | .browser_download_url" ;) \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/geckodriver_linux64.tar.gz "$GECKODRIVER_LATEST_RELEASE_URL" \
 && cd /tmp \
 && tar xf geckodriver_linux64.tar.gz \
 && rm -rf geckodriver_linux64.tar.gz \
 && mv geckodriver /usr/local/bin/geckodriver \
 && chmod +x /usr/local/bin/geckodriver \
 && geckodriver --version
#   install chrome
RUN curl --silent --show-error --location --fail --retry 3 --output /tmp/google-chrome-stable_current_amd64.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
 && (dpkg -i /tmp/google-chrome-stable_current_amd64.deb || apt -fy install ) \
 && rm -rf /tmp/google-chrome-stable_current_amd64.deb \
 && sed -i 's|HERE/chrome"|HERE/chrome" --disable-setuid-sandbox --no-sandbox|g' "/opt/google/chrome/google-chrome" \
 && google-chrome --version
RUN export CHROMEDRIVER_RELEASE=$( curl --location --fail --retry 3 http://chromedriver.storage.googleapis.com/LATEST_RELEASE ;) \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/chromedriver_linux64.zip "http://chromedriver.storage.googleapis.com/$CHROMEDRIVER_RELEASE/chromedriver_linux64.zip" \
 && cd /tmp \
 && unzip chromedriver_linux64.zip \
 && rm -rf chromedriver_linux64.zip \
 && mv chromedriver /usr/local/bin/chromedriver \
 && chmod +x /usr/local/bin/chromedriver \
 && chromedriver --version
#   start xvfb automatically to avoid needing to express in circle.yml
ENV DISPLAY=":99"
RUN printf '#!/bin/sh\nXvfb :99 -screen 0 1280x1024x24 &\nexec "$@"\n' > /tmp/entrypoint \
 && chmod +x /tmp/entrypoint
#   install paratii-db
RUN mkdir /root/.ssh/
COPY id_rsa /root/.ssh/id_rsa
RUN touch /root/.ssh/known_hosts
RUN ssh-keyscan github.com >> /root/.ssh/known_hosts
WORKDIR /paratii-db
RUN git clone git@github.com:Paratii-Video/paratii-db.git
RUN cd paratii-db \
 && npm install
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
