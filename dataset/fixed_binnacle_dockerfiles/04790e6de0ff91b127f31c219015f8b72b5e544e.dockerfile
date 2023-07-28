FROM ubuntu:16.04
ENV SUMMARY="galne-runtime"
ENV DESCRIPTION="Application runtime for Galenframework"
ENV AUTHOR="Martin Reinhardt <contact@martinreinhardt-online.de"
ENV TZ="Europe/Berlin"
ENV NVM_DIR="/home/galen"
ENV NODE_VERSION="8.12.0"
ENV TEST_HOME="/var/jenkins_home"
ENV NODE_PATH="$NVM_DIR/v$NODE_VERSION/lib/node_modules"
ENV PATH="$NVM_DIR/v$NODE_VERSION/bin:$PATH"
LABEL summary="$SUMMARY" \
      description="$DESCRIPTION" \
      version="$VERSION" \
      author="$AUTHOR" \
      io.k8s.description="$DESCRIPTION" \
      io.k8s.display-name="galen-runtime" \
      io.k8s.tags="galen,galenframework,layout-testing,testing" \
      com.redhat.component="galen-runtime-container org.label-schema.license=MIT"
#  ================================================
#   Customize sources for apt-get
#  ================================================
RUN echo "deb http://archive.ubuntu.com/ubuntu xenial main universe\n" > /etc/apt/sources.list \
 && echo "deb http://archive.ubuntu.com/ubuntu xenial-updates main universe\n" >> /etc/apt/sources.list \
 && echo "deb http://security.ubuntu.com/ubuntu xenial-security main universe\n" >> /etc/apt/sources.list
#   No interactive frontend during docker build
ENV DEBIAN_FRONTEND="noninteractive" \
    DEBCONF_NONINTERACTIVE_SEEN="true"
#  ========================
#   Miscellaneous packages
#   Includes minimal runtime used for executing non GUI Java programs
#  ========================
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 tzdata=2021a-0ubuntu0.16.04 sudo=1.8.16-0ubuntu1.10 unzip=6.0-20ubuntu1.1 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 -qqy \
 && rm -rf /var/lib/apt/lists/* /var/cache/apt/* \
 && sed -i 's/securerandom\.source=file:\/dev\/random/securerandom\.source=file:\/dev\/urandom/' ./usr/lib/jvm/java-8-openjdk-amd64/jre/lib/security/java.security
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
#  ===================
#   Timezone settings
#   Possible alternative: https://github.com/docker/docker/issues/3359#issuecomment-32150214
#  ===================
ENV TZ="\"UTC\""
RUN echo "${TZ}" > /etc/timezone \
 && dpkg-reconfigure --frontend noninteractive tzdata
#  ========================================
#   Add normal user with passwordless sudo
#  ========================================
RUN useradd galen --shell /bin/bash --create-home \
 && usermod -a -G sudo galen \
 && echo 'ALL ALL = (ALL) NOPASSWD: ALL' >> /etc/sudoers \
 && echo 'galen:secret' | chpasswd
#  ===================================================
#   Install xvfb
#  ===================================================
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends xvfb=2:1.18.4-0ubuntu0.12 -y
#  ===================================================
#   Install Chrome
#  ===================================================
RUN echo 'deb http://dl.google.com/linux/chrome/deb/ stable main' > /etc/apt/sources.list.d/chrome.list \
 && wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && set -x \
 && apt-get update \
 && apt-get install --no-install-recommends google-chrome-stable -y
COPY scripts/xvfb-chrome /usr/bin/xvfb-chrome
RUN ln -sf /usr/bin/xvfb-chrome /usr/bin/google-chrome
#  ===================================================
#   Install firefox
#  ===================================================
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends firefox=88.0+build2-0ubuntu0.16.04.1 -y
COPY scripts/xvfb-firefox /usr/bin/xvfb-firefox
RUN ln -sf /usr/bin/xvfb-firefox /usr/bin/firefox
#  ===================================================
#   This is needed for PhantomJS
#  ===================================================
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends bzip2=1.0.6-8ubuntu0.2 zip=3.0-11 -y
#   clean up
RUN apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/
#  ===================================================
#   Run the following commands as non-privileged user
#  ===================================================
USER galen
#  ===================================================
#   Install nvm with node and npm
#  ===================================================
RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash \
 && . $NVM_DIR/nvm.sh \
 && nvm install $NODE_VERSION \
 && nvm alias default $NODE_VERSION \
 && nvm use default \
 && npm install galenframework-cli@$GALEN_VERSION -g
VOLUME /var/test_scripts
COPY ./etc/entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
# Please add your HEALTHCHECK here!!!
