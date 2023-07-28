#
#  DO NOT MODIFY THIS FILE.  THIS FILE HAS BEEN AUTOGENERATED
#
FROM openjdk:8-jdk-slim
LABEL maintainer="marc@circleci.com"
#  Initial Command run as `root`.
COPY https://raw.githubusercontent.com/circleci/circleci-images/master/android/bin/circle-android /bin/circle-android
RUN chmod +rx /bin/circle-android
#  Skip the first line of the Dockerfile template (FROM ${BASE})
#  make Apt non-interactive
RUN echo 'APT::Get::Assume-Yes "true";' > /etc/apt/apt.conf.d/90circleci \
 && echo 'DPkg::Options "--force-confnew";' >> /etc/apt/apt.conf.d/90circleci
ENV DEBIAN_FRONTEND="noninteractive"
#  Debian Jessie is EOL'd and original repos don't work.
#  Switch to the archive mirror until we can get people to
#  switch to Stretch.
RUN if grep -q Debian /etc/os-release \
 && grep -q jessie /etc/os-release ; then rm /etc/apt/sources.list \
 && echo "deb http://archive.debian.org/debian/ jessie main" >> /etc/apt/sources.list \
 && echo "deb http://security.debian.org/debian-security jessie/updates main" >> /etc/apt/sources.list; fi
#  Make sure PATH includes ~/.local/bin
#  https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=839155
RUN echo 'PATH="$HOME/.local/bin:$PATH"' >> /etc/profile.d/user-local-path.sh
#  man directory is missing in some base images
#  https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=863199
RUN apt-get update \
 && mkdir -p /usr/share/man/man1 \
 && apt-get install --no-install-recommends git mercurial xvfb apt locales sudo openssh-client ca-certificates tar gzip parallel net-tools netcat unzip zip bzip2 gnupg curl wget make -y
#  Set timezone to UTC by default
RUN ln -sf /usr/share/zoneinfo/Etc/UTC /etc/localtime
#  Use unicode
RUN locale-gen C.UTF-8 || true
ENV LANG="C.UTF-8"
#  install jq
RUN JQ_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/jq-latest" \
 && curl --silent --show-error --location --fail --retry 3 --output /usr/bin/jq $JQ_URL \
 && chmod +x /usr/bin/jq \
 && jq --version
#  Install Docker
#  Docker.com returns the URL of the latest binary when you hit a directory listing
#  We curl this URL and `grep` the version out.
#  The output looks like this:
# >    # To install, run the following commands as root:
# >    curl -fsSLO https://download.docker.com/linux/static/stable/x86_64/docker-17.05.0-ce.tgz && tar --strip-components=1 -xvzf docker-17.05.0-ce.tgz -C /usr/local/bin
# >
# >    # Then start docker in daemon mode:
# >    /usr/local/bin/dockerd
RUN set -ex \
 && export DOCKER_VERSION=$( curl --silent --fail --retry 3 https://download.docker.com/linux/static/stable/x86_64/ | grep -o -e 'docker-[.0-9]*\.tgz' | sort -r | head -n 1 ;) \
 && DOCKER_URL="https://download.docker.com/linux/static/stable/x86_64/${DOCKER_VERSION}" \
 && echo Docker URL: $DOCKER_URL \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/docker.tgz "${DOCKER_URL}" \
 && ls -lha /tmp/docker.tgz \
 && tar -xz -C /tmp -f /tmp/docker.tgz \
 && mv /tmp/docker/* /usr/bin \
 && rm -rf /tmp/docker /tmp/docker.tgz \
 && which docker \
 && (docker version || true )
#  docker compose
RUN COMPOSE_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/docker-compose-latest" \
 && curl --silent --show-error --location --fail --retry 3 --output /usr/bin/docker-compose $COMPOSE_URL \
 && chmod +x /usr/bin/docker-compose \
 && docker-compose version
#  install dockerize
RUN DOCKERIZE_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/dockerize-latest.tar.gz" \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/dockerize-linux-amd64.tar.gz $DOCKERIZE_URL \
 && tar -C /usr/local/bin -xzvf /tmp/dockerize-linux-amd64.tar.gz \
 && rm -rf /tmp/dockerize-linux-amd64.tar.gz \
 && dockerize --version
RUN groupadd --gid 3434 circleci \
 && useradd --uid 3434 --gid circleci --shell /bin/bash --create-home circleci \
 && echo 'circleci ALL=NOPASSWD: ALL' >> /etc/sudoers.d/50-circleci \
 && echo 'Defaults env_keep += "DEBIAN_FRONTEND"' >> /etc/sudoers.d/env_keep
#  BEGIN IMAGE CUSTOMIZATIONS
#  END IMAGE CUSTOMIZATIONS
USER circleci
CMD ["/bin/sh"]
#  Now commands run as user `circleci`
#  Switching user can confuse Docker's idea of $HOME, so we set it explicitly
ENV HOME="/home/circleci"
#  Install Google Cloud SDK
RUN sudo apt-get update -qqy \
 && sudo apt-get install --no-install-recommends -qqy python-dev python-setuptools apt-transport-https lsb-release
RUN sudo apt-get install --no-install-recommends gcc-multilib \
 && sudo easy_install -U pip \
 && sudo pip uninstall crcmod \
 && sudo pip install -U crcmod
RUN export CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
RUN sudo apt-get update \
 && sudo apt-get install --no-install-recommends -y google-cloud-sdk \
 && gcloud config set core/disable_usage_reporting true \
 && gcloud config set component_manager/disable_update_check true
ARG sdk_version=sdk-tools-linux-3859397.zip
ARG android_home=/opt/android/sdk
#  SHA-256 444e22ce8ca0f67353bda4b85175ed3731cae3ffa695ca18119cbacef1c1bea0
RUN sudo apt-get update \
 && sudo apt-get install --no-install-recommends --yes xvfb lib32z1 lib32stdc++6 build-essential libcurl4-openssl-dev libglu1-mesa libxi-dev libxmu-dev libglu1-mesa-dev
#  Install Ruby
RUN cd /tmp \
 && wget -O ruby-install-0.6.1.tar.gz https://github.com/postmodern/ruby-install/archive/v0.6.1.tar.gz \
 && tar -xzvf ruby-install-0.6.1.tar.gz \
 && cd ruby-install-0.6.1 \
 && sudo make install \
 && ruby-install --cleanup ruby 2.6.1 \
 && rm -r /tmp/ruby-install-*
ENV PATH="${HOME}/.rubies/ruby-2.6.1/bin:${PATH}"
RUN echo 'gem: --env-shebang --no-rdoc --no-ri' >> ~/.gemrc \
 && gem install bundler
#  Download and install Android SDK
RUN sudo mkdir -p ${android_home} \
 && sudo chown -R circleci:circleci ${android_home} \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/${sdk_version} https://dl.google.com/android/repository/${sdk_version} \
 && unzip -q /tmp/${sdk_version} -d ${android_home} \
 && rm /tmp/${sdk_version}
#  Set environmental variables
ENV ANDROID_HOME="${android_home}"
ENV ADB_INSTALL_TIMEOUT="120"
ENV PATH="${ANDROID_HOME}/emulator:${ANDROID_HOME}/tools:${ANDROID_HOME}/tools/bin:${ANDROID_HOME}/platform-tools:${PATH}"
RUN mkdir ~/.android \
 && echo '### User Sources for Android SDK Manager' > ~/.android/repositories.cfg
RUN yes | sdkmanager --licenses \
 && yes | sdkmanager --update
#  Update SDK manager and install system image, platform and build tools
RUN sdkmanager "tools" "platform-tools" "emulator"
RUN sdkmanager "build-tools;25.0.0" "build-tools;25.0.1" "build-tools;25.0.2" "build-tools;25.0.3" "build-tools;26.0.1" "build-tools;26.0.2" "build-tools;27.0.0" "build-tools;27.0.1" "build-tools;27.0.2" "build-tools;27.0.3" "build-tools;28.0.0" "build-tools;28.0.1" "build-tools;28.0.2" "build-tools;28.0.3" "build-tools;29.0.0"
#  API_LEVEL string gets replaced by m4
RUN sdkmanager "platforms;android-25"
# ##
# ## DO NOT MODIFY THIS FILE.  THIS FILE HAS BEEN AUTOGENERATED
# ##
#  Verify the circleci user exists before proceeding
RUN whoami
#  node installations command expect to run as root
USER root
# # Using node installation from https://raw.githubusercontent.com/nodejs/docker-node/f8f2384f7edc345f5ffc0496458005981b512882/10/stretch/Dockerfile
RUN groupadd --gid 1000 node \
 && useradd --uid 1000 --gid node --shell /bin/bash --create-home node
ENV NODE_VERSION="10.16.0"
RUN ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='x64' ;;(ppc64el) ARCH='ppc64le' ;;(s390x) ARCH='s390x' ;;(arm64) ARCH='arm64' ;;(armhf) ARCH='armv7l' ;;(i386) ARCH='x86' ;;(*) echo "unsupported architecture" ; exit 1 ;; esac \
 && set -ex \
 && for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 B9AE9905FFD7803F25714661B63B535A4C206CA9 77984A986EBC2AA786BC0F66B01FBB92821C587A 8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600 4ED778F539E3634C779C87C6D7062848A1AB005C A48C2BEE680E841632CD4E44F07496B3EB3C1762 B9E2F5981AA6E0CD28160D9FF13993A75599653C; do gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --batch --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-$ARCH.tar.xz" \
 && curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION-linux-$ARCH.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xJf "node-v$NODE_VERSION-linux-$ARCH.tar.xz" -C /usr/local --strip-components=1 --no-same-owner \
 && rm "node-v$NODE_VERSION-linux-$ARCH.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt \
 && ln -s /usr/local/bin/node /usr/local/bin/nodejs
ENV YARN_VERSION="1.16.0"
RUN set -ex \
 && for key in 6A010C5166006599AA17F08146C2130DFD2497F5; do gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --batch --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && curl -fsSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz" \
 && curl -fsSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz.asc" \
 && gpg --batch --verify yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz \
 && mkdir -p /opt \
 && tar -xzf yarn-v$YARN_VERSION.tar.gz -C /opt/ \
 && ln -s /opt/yarn-v$YARN_VERSION/bin/yarn /usr/local/bin/yarn \
 && ln -s /opt/yarn-v$YARN_VERSION/bin/yarnpkg /usr/local/bin/yarnpkg \
 && rm yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz
#  Basic smoke test
RUN node --version
USER circleci
