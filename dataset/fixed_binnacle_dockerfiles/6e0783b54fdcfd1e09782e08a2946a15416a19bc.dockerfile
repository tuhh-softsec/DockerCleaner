FROM ubuntu:16.04
MAINTAINER Juergen Schackmann
#   -----------------------------------------------------------------------------
#   General environment variables
#   -----------------------------------------------------------------------------
ENV DEBIAN_FRONTEND="noninteractive"
#   -----------------------------------------------------------------------------
#   Install system basics
#   -----------------------------------------------------------------------------
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 python-software-properties=0.96.20.10 software-properties-common=0.96.20.10 curl=7.47.0-1ubuntu2.19 expect=5.45-7 zip=3.0-11 libsass-dev=3.3.4-1 git=1:2.7.4-0ubuntu1.10 sudo=1.8.16-0ubuntu1.10 -qqy --allow-unauthenticated
#   -----------------------------------------------------------------------------
#   Install Java
#   -----------------------------------------------------------------------------
ARG JAVA_VERSION
ENV JAVA_VERSION="${JAVA_VERSION:-8}"
ENV JAVA_HOME="${JAVA_HOME:-/usr/lib/jvm/java-${JAVA_VERSION}-oracle}"
RUN add-apt-repository ppa:webupd8team/java -y \
 && echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends oracle-java${JAVA_VERSION}-installer -qqy --allow-unauthenticated
#   -----------------------------------------------------------------------------
#   Install Android / Android SDK / Android SDK elements
#   -----------------------------------------------------------------------------
ENV ANDROID_HOME="/opt/android-sdk-linux"
ENV PATH="${PATH}:${ANDROID_HOME}/tools:${ANDROID_HOME}/tools/bin:${ANDROID_HOME}/platform-tools:/opt/tools"
ARG ANDROID_PLATFORMS_VERSION
ENV ANDROID_PLATFORMS_VERSION="${ANDROID_PLATFORMS_VERSION:-25}"
ARG ANDROID_BUILD_TOOLS_VERSION
ENV ANDROID_BUILD_TOOLS_VERSION="${ANDROID_BUILD_TOOLS_VERSION:-25.0.3}"
RUN echo ANDROID_HOME=${ANDROID_HOME} >> /etc/environment \
 && dpkg --add-architecture i386 \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends gradle=2.10-1 libc6-i386=2.23-0ubuntu11.3 lib32stdc++6=5.4.0-6ubuntu1~16.04.12 lib32gcc1=1:6.0.1-0ubuntu1 lib32ncurses5=6.0+20160213-1ubuntu1 lib32z1=1:1.2.8.dfsg-2ubuntu4.3 qemu-kvm=1:2.5+dfsg-5ubuntu10.51 kmod=22-1ubuntu5.2 -qqy --allow-unauthenticated \
 && cd /opt \
 && mkdir android-sdk-linux \
 && cd android-sdk-linux \
 && curl -SLo sdk-tools-linux.zip https://dl.google.com/android/repository/sdk-tools-linux-3859397.zip \
 && unzip sdk-tools-linux.zip \
 && rm -f sdk-tools-linux.zip \
 && chmod 777 ${ANDROID_HOME} -R \
 && mkdir -p ${ANDROID_HOME}/licenses \
 && echo 8933bad161af4178b1185d1a37fbf41ea5269c55 > ${ANDROID_HOME}/licenses/android-sdk-license \
 && sdkmanager "tools" \
 && sdkmanager "platform-tools" \
 && sdkmanager "platforms;android-${ANDROID_PLATFORMS_VERSION}" \
 && sdkmanager "build-tools;${ANDROID_BUILD_TOOLS_VERSION}"
#   -----------------------------------------------------------------------------
#   Install Node, NPM, yarn
#   -----------------------------------------------------------------------------
ARG NODE_VERSION
ENV NODE_VERSION="${NODE_VERSION:-6.9.5} "
ARG NPM_VERSION
ENV NPM_VERSION="${NPM_VERSION:-5.3.0}"
ARG PACKAGE_MANAGER
ENV PACKAGE_MANAGER="${PACKAGE_MANAGER:-npm}"
ENV NPM_CONFIG_LOGLEVEL="info"
#   gpg keys listed at https://github.com/nodejs/node
RUN set -ex \
 && for key in 9554F04D7259F04124DE6B476D5A82AC7E37093B 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 0034A06D9D9B0064CE8ADF6BF1747F4AD2306D93 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D B9AE9905FFD7803F25714661B63B535A4C206CA9 C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8; do gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "${key}" ; done \
 && curl -SLO "https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-x64.tar.xz" \
 && curl -SLO "https://nodejs.org/dist/v${NODE_VERSION}/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v${NODE_VERSION}-linux-x64.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xJf "node-v${NODE_VERSION}-linux-x64.tar.xz" -C /usr/local --strip-components=1 \
 && rm "node-v${NODE_VERSION}-linux-x64.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt \
 && ln -s /usr/local/bin/node /usr/local/bin/nodejs \
 && chmod 777 /usr/local/lib/node_modules -R \
 && npm install npm@${NPM_VERSION} -g \
 && if [ "${PACKAGE_MANAGER}" = "yarn" ] ; then curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends yarn -qqy --allow-unauthenticated ; fi
#   -----------------------------------------------------------------------------
#   Clean up
#   -----------------------------------------------------------------------------
RUN apt-get clean \
 && apt-get autoclean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   -----------------------------------------------------------------------------
#   Create a non-root docker user to run this container
#   -----------------------------------------------------------------------------
ARG USER
ENV USER="${USER:-ionic}"
RUN useradd --user-group --create-home --shell /bin/false ${USER} \
 && echo "${USER}:${USER}" | chpasswd \
 && adduser ${USER} sudo \
 && adduser ${USER} root \
 && chmod 770 / \
 && usermod -a -G root ${USER} \
 && mkdir /app \
 && chown ${USER}:${USER} /app \
 && chmod 777 /app \
 && touch /image.config \
 && chown ${USER}:${USER} /image.config \
 && chmod 777 /image.config \
 && mkdir /home/${USER}/.ionic \
 && chown ${USER}:${USER} /home/${USER}/.ionic \
 && chmod 777 /home/${USER}/.ionic \
 && chmod 777 /usr/local/bin
#  && chown ${USER}:${USER} ${ANDROID_HOME} -R
#   -----------------------------------------------------------------------------
#   Copy start.sh and set permissions 
#   -----------------------------------------------------------------------------
COPY start.sh /start.sh
RUN chown ${USER}:${USER} /start.sh \
 && chmod 777 /start.sh
#   -----------------------------------------------------------------------------
#   Switch the user of this image only now, because previous commands need to be 
#   run as root
#   -----------------------------------------------------------------------------
USER ${USER}
#   -----------------------------------------------------------------------------
#   Install Global node modules
#   -----------------------------------------------------------------------------
ARG CORDOVA_VERSION
ENV CORDOVA_VERSION="${CORDOVA_VERSION:-7.0.1}"
ARG IONIC_VERSION
ENV IONIC_VERSION="${IONIC_VERSION:-3.12.0}"
ARG TYPESCRIPT_VERSION
ENV TYPESCRIPT_VERSION="${TYPESCRIPT_VERSION:-2.3.4}"
ARG GULP_VERSION
ENV GULP_VERSION="${GULP_VERSION}"
RUN if [ "${PACKAGE_MANAGER}" != "yarn" ] ; then export PACKAGE_MANAGER="npm" \
 && npm install cordova@"${CORDOVA_VERSION}" -g \
 && if [ -n "${IONIC_VERSION}" ] ; then npm install ionic@"${IONIC_VERSION}" -g ; fi \
 && if [ -n "${TYPESCRIPT_VERSION}" ] ; then npm install typescript@"${TYPESCRIPT_VERSION}" -g ; fi \
 && if [ -n "${GULP_VERSION}" ] ; then npm install gulp@"${GULP_VERSION}" -g ; fi ; else yarn global add cordova@"${CORDOVA_VERSION}" \
 && if [ -n "${IONIC_VERSION}" ] ; then yarn global add ionic@"${IONIC_VERSION}" ; fi \
 && if [ -n "${TYPESCRIPT_VERSION}" ] ; then yarn global add typescript@"${TYPESCRIPT_VERSION}" ; fi \
 && if [ -n "${GULP_VERSION}" ] ; then yarn global add gulp@"${GULP_VERSION}" ; fi ; fi \
 && ${PACKAGE_MANAGER} cache clean --force
#   -----------------------------------------------------------------------------
#   Create the image.config file for the container to check the build 
#   configuration of this container later on 
#   -----------------------------------------------------------------------------
RUN echo "USER: ${USER}\nJAVA_VERSION: ${JAVA_VERSION}\nANDROID_PLATFORMS_VERSION: ${ANDROID_PLATFORMS_VERSION}\nANDROID_BUILD_TOOLS_VERSION: ${ANDROID_BUILD_TOOLS_VERSION}\nNODE_VERSION: ${NODE_VERSION}\nNPM_VERSION: ${NPM_VERSION}\nPACKAGE_MANAGER: ${PACKAGE_MANAGER}\nCORDOVA_VERSION: ${CORDOVA_VERSION}\nIONIC_VERSION: ${IONIC_VERSION}\nTYPESCRIPT_VERSION: ${TYPESCRIPT_VERSION}\nGULP_VERSION: ${GULP_VERSION:-none}\n" >> /image.config \
 && cat /image.config
#   -----------------------------------------------------------------------------
#   Generate an Ionic default app (do this with root user, since we will not
#   have permissions for /app otherwise), install the dependencies
#   and add and build android platform
#   -----------------------------------------------------------------------------
RUN cd / \
 && ionic config set -g backend legacy \
 && ionic start app blank --type ionic-angular --no-deps --no-link --no-git \
 && cd /app \
 && ${PACKAGE_MANAGER} install \
 && ionic cordova platform add android --no-resources \
 && ionic cordova build android
#   -----------------------------------------------------------------------------
#   Just in case you are installing from private git repositories, enable git
#   credentials
#   -----------------------------------------------------------------------------
RUN git config --global credential.helper store
#   -----------------------------------------------------------------------------
#   WORKDIR is the generic /app folder. All volume mounts of the actual project
#   code need to be put into /app.
#   -----------------------------------------------------------------------------
WORKDIR /app
#   -----------------------------------------------------------------------------
#   The script start.sh installs package.json and puts a watch on it. This makes
#   sure that the project has allways the latest dependencies installed.
#   -----------------------------------------------------------------------------
ENTRYPOINT ["/start.sh"]
#   -----------------------------------------------------------------------------
#   After /start.sh the bash is called.
#   -----------------------------------------------------------------------------
CMD ["ionic", "serve", "-b", "-p", "8100", "--address", "0.0.0.0"]
# Please add your HEALTHCHECK here!!!
