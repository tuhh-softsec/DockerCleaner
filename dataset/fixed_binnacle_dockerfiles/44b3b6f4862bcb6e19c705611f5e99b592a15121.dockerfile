#
#   DO NOT MODIFY THIS FILE.  THIS FILE HAS BEEN AUTOGENERATED
#
FROM openjdk:8-jdk-slim
LABEL maintainer="marc@circleci.com"
#   Initial Command run as `root`.
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /bin/circle-android https://raw.githubusercontent.com/circleci/circleci-images/master/android/bin/circle-android
RUN chmod +rx /bin/circle-android
#   Skip the first line of the Dockerfile template (FROM ${BASE})
#   make Apt non-interactive
RUN echo 'APT::Get::Assume-Yes "true";' > /etc/apt/apt.conf.d/90circleci \
 && echo 'DPkg::Options "--force-confnew";' >> /etc/apt/apt.conf.d/90circleci
ENV DEBIAN_FRONTEND="noninteractive"
#   man directory is missing in some base images
#   https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=863199
RUN apt-get update \
 && mkdir -p /usr/share/man/man1 \
 && apt-get install --no-install-recommends git=1:2.30.2-1+deb11u2 mercurial=5.6.1-4 xvfb=2:1.20.11-1+deb11u6 locales=2.31-13+deb11u5 sudo=1.9.5p2-3+deb11u1 openssh-client=1:8.4p1-5+deb11u1 ca-certificates=20210119 tar=1.34+dfsg-1 gzip=1.10-4+deb11u1 parallel=20161222-1.1 net-tools=1.60+git20181103.0eebece-1 netcat=1.10-46 unzip=6.0-26+deb11u1 zip=3.0-12 bzip2=1.0.8-4 gnupg=2.2.27-2+deb11u2 curl=7.74.0-1.3+deb11u7 wget=1.21-1+deb11u1 -y
#   Set timezone to UTC by default
RUN ln -sf /usr/share/zoneinfo/Etc/UTC /etc/localtime
#   Use unicode
RUN locale-gen C.UTF-8 || true
ENV LANG="C.UTF-8"
#   install jq
RUN JQ_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/jq-latest" \
 && curl --silent --show-error --location --fail --retry 3 --output /usr/bin/jq $JQ_URL \
 && chmod +x /usr/bin/jq \
 && jq --version
#   Install Docker
#   Docker.com returns the URL of the latest binary when you hit a directory listing
#   We curl this URL and `grep` the version out.
#   The output looks like this:
#  >    # To install, run the following commands as root:
#  >    curl -fsSLO https://download.docker.com/linux/static/stable/x86_64/docker-17.05.0-ce.tgz && tar --strip-components=1 -xvzf docker-17.05.0-ce.tgz -C /usr/local/bin
#  >
#  >    # Then start docker in daemon mode:
#  >    /usr/local/bin/dockerd
RUN set -ex \
 && export DOCKER_VERSION=$( curl --silent --fail --retry 3 https://download.docker.com/linux/static/stable/x86_64/ | grep -o -e 'docker-[.0-9]*-ce\.tgz' | sort -r | head -n 1 ;) \
 && DOCKER_URL="https://download.docker.com/linux/static/stable/x86_64/${DOCKER_VERSION}" \
 && echo Docker URL: $DOCKER_URL \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/docker.tgz "${DOCKER_URL}" \
 && ls -lha /tmp/docker.tgz \
 && tar -xz -C /tmp -f /tmp/docker.tgz \
 && mv /tmp/docker/* /usr/bin \
 && rm -rf /tmp/docker /tmp/docker.tgz \
 && which docker \
 && (docker version || true )
#   docker compose
RUN COMPOSE_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/docker-compose-latest" \
 && curl --silent --show-error --location --fail --retry 3 --output /usr/bin/docker-compose $COMPOSE_URL \
 && chmod +x /usr/bin/docker-compose \
 && docker-compose version
#   install dockerize
RUN DOCKERIZE_URL="https://circle-downloads.s3.amazonaws.com/circleci-images/cache/linux-amd64/dockerize-latest.tar.gz" \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/dockerize-linux-amd64.tar.gz $DOCKERIZE_URL \
 && tar -C /usr/local/bin -xzvf /tmp/dockerize-linux-amd64.tar.gz \
 && rm -rf /tmp/dockerize-linux-amd64.tar.gz \
 && dockerize --version
RUN groupadd --gid 3434 circleci \
 && useradd --uid 3434 --gid circleci --shell /bin/bash --create-home circleci \
 && echo 'circleci ALL=NOPASSWD: ALL' >> /etc/sudoers.d/50-circleci \
 && echo 'Defaults env_keep += "DEBIAN_FRONTEND"' >> /etc/sudoers.d/env_keep
#   BEGIN IMAGE CUSTOMIZATIONS
#   END IMAGE CUSTOMIZATIONS
USER circleci
CMD ["/bin/sh"]
#   Now commands run as user `circleci`
#   Switching user can confuse Docker's idea of $HOME, so we set it explicitly
ENV HOME="/home/circleci"
#   Install Google Cloud SDK
RUN sudo apt-get update -qqy \
 && sudo apt-get install -qqy python-dev python-setuptools apt-transport-https lsb-release
RUN sudo apt-get install gcc-multilib \
 && sudo easy_install -U pip \
 && sudo pip uninstall crcmod \
 && sudo pip install -U crcmod
RUN export CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
RUN sudo apt-get update \
 && sudo apt-get install -y google-cloud-sdk \
 && gcloud config set core/disable_usage_reporting true \
 && gcloud config set component_manager/disable_update_check true
ARG sdk_version=sdk-tools-linux-3859397.zip
ARG android_home=/opt/android/sdk
#   SHA-256 444e22ce8ca0f67353bda4b85175ed3731cae3ffa695ca18119cbacef1c1bea0
RUN sudo apt-get update \
 && sudo apt-get install --yes xvfb lib32z1 lib32stdc++6 build-essential libcurl4-openssl-dev libglu1-mesa libxi-dev libxmu-dev libglu1-mesa-dev
#   Install Ruby
RUN cd /tmp \
 && wget -O ruby-install-0.6.1.tar.gz https://github.com/postmodern/ruby-install/archive/v0.6.1.tar.gz \
 && tar -xzvf ruby-install-0.6.1.tar.gz \
 && cd ruby-install-0.6.1 \
 && sudo make install \
 && ruby-install --cleanup ruby 2.4.3 \
 && rm -r /tmp/ruby-install-*
ENV PATH="${HOME}/.rubies/ruby-2.4.3/bin:${PATH}"
RUN echo 'gem: --env-shebang --no-rdoc --no-ri' >> ~/.gemrc \
 && gem install bundler --version 2.4.12
#   Download and install Android SDK
RUN sudo mkdir -p ${android_home} \
 && sudo chown -R circleci:circleci ${android_home} \
 && curl --silent --show-error --location --fail --retry 3 --output /tmp/${sdk_version} https://dl.google.com/android/repository/${sdk_version} \
 && unzip -q /tmp/${sdk_version} -d ${android_home} \
 && rm /tmp/${sdk_version}
#   Set environmental variables
ENV ANDROID_HOME="${android_home}"
ENV ADB_INSTALL_TIMEOUT="120"
ENV PATH="${ANDROID_HOME}/emulator:${ANDROID_HOME}/tools:${ANDROID_HOME}/tools/bin:${ANDROID_HOME}/platform-tools:${PATH}"
RUN mkdir ~/.android \
 && echo '### User Sources for Android SDK Manager' > ~/.android/repositories.cfg
RUN yes | sdkmanager --licenses \
 && sdkmanager --update
#   Update SDK manager and install system image, platform and build tools
RUN sdkmanager "tools" "platform-tools" "emulator" "extras;android;m2repository" "extras;google;m2repository" "extras;google;google_play_services"
RUN sdkmanager "build-tools;25.0.0" "build-tools;25.0.1" "build-tools;25.0.2" "build-tools;25.0.3" "build-tools;26.0.1" "build-tools;26.0.2" "build-tools;27.0.0" "build-tools;27.0.1" "build-tools;27.0.2" "build-tools;27.0.3" "build-tools;28.0.0" "build-tools;28.0.3"
#   API_LEVEL string gets replaced by m4
RUN sdkmanager "platforms;android-23"
# Please add your HEALTHCHECK here!!!
