#   Android Dockerfile
FROM ubuntu:14.04
MAINTAINER Mobile Builds Eng "mobile-builds-eng@uber.com"
#   Sets language to UTF8 : this works in pretty much all cases
ENV LANG="en_US.UTF-8"
RUN locale-gen $LANG
ENV DOCKER_ANDROID_LANG="en_US"
ENV DOCKER_ANDROID_DISPLAY_NAME="mobileci-docker"
#   Never ask for confirmations
ENV DEBIAN_FRONTEND="noninteractive"
#   Update apt-get
RUN rm -rf /var/lib/apt/lists/*
RUN :
RUN apt-get dist-upgrade -y
#   Installing packages
RUN (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-6 build-essential=11.6ubuntu6 bzip2=1.0.6-5 curl=7.35.0-1ubuntu2.20 gcc=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 groff=1.22.2-5 lib32stdc++6=4.8.4-2ubuntu1~14.04.4 lib32z1=1:1.2.8.dfsg-1ubuntu1.1 lib32z1-dev=1:1.2.8.dfsg-1ubuntu1.1 lib32ncurses5=5.9+20140118-1ubuntu1 lib32bz2-1.0=1.0.6-5 libc6-dev=2.19-0ubuntu6.15 libgmp-dev=2:5.1.3+dfsg-1ubuntu1 libmpc-dev=1.0.1-1ubuntu1 libmpfr-dev=3.1.2-1 libxslt-dev libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 m4=1.4.17-2ubuntu1 make=3.81-8.2ubuntu3 ncurses-dev ocaml=4.01.0-3ubuntu3.1 openssh-client=1:6.6p1-2ubuntu2.13 pkg-config=0.26-1ubuntu4 python-software-properties=0.92.37.8 rsync=3.1.0-2ubuntu0.4 software-properties-common=0.92.37.8 unzip=6.0-9ubuntu1.5 wget=1.15-1ubuntu1.14.04.5 zip=3.0-8 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 -y )
#   Install Java
RUN apt-add-repository ppa:openjdk-r/ppa
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-8-jdk -y )
#   Clean Up Apt-get
RUN rm -rf /var/lib/apt/lists/*
RUN apt-get clean
#   Install Android SDK
RUN wget https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz
RUN tar -xvzf android-sdk_r24.4.1-linux.tgz
RUN mv android-sdk-linux /usr/local/android-sdk
RUN rm android-sdk_r24.4.1-linux.tgz
ENV ANDROID_COMPONENTS="platform-tools,android-23,build-tools-23.0.2,build-tools-24.0.0"
#   Install Android tools
RUN echo y | /usr/local/android-sdk/tools/android update sdk --filter "${ANDROID_COMPONENTS}" --no-ui -a
#   Install Android NDK
RUN wget http://dl.google.com/android/repository/android-ndk-r12-linux-x86_64.zip
RUN unzip android-ndk-r12-linux-x86_64.zip
RUN mv android-ndk-r12 /usr/local/android-ndk
RUN rm android-ndk-r12-linux-x86_64.zip
#   Environment variables
ENV ANDROID_HOME="/usr/local/android-sdk"
ENV ANDROID_SDK_HOME="$ANDROID_HOME"
ENV ANDROID_NDK_HOME="/usr/local/android-ndk"
ENV JENKINS_HOME="$HOME"
ENV PATH="${INFER_HOME}/bin:${PATH}"
ENV PATH="$PATH:$ANDROID_SDK_HOME/tools"
ENV PATH="$PATH:$ANDROID_SDK_HOME/platform-tools"
ENV PATH="$PATH:$ANDROID_SDK_HOME/build-tools/23.0.2"
ENV PATH="$PATH:$ANDROID_SDK_HOME/build-tools/24.0.0"
ENV PATH="$PATH:$ANDROID_NDK_HOME"
#   Export JAVA_HOME variable
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
#   Support Gradle
ENV TERM="dumb"
ENV JAVA_OPTS="\"-Xms4096m -Xmx4096m\""
ENV GRADLE_OPTS="\"-XX:+UseG1GC -XX:MaxGCPauseMillis=1000\""
#   Cleaning
RUN apt-get clean
#   Add build user account, values are set to default below
ENV RUN_USER="mobileci"
ENV RUN_UID="5089"
RUN id $RUN_USER || adduser --uid "$RUN_UID" --gecos 'Build User' --shell '/bin/sh' --disabled-login --disabled-password "$RUN_USER"
#   Fix permissions
RUN chown -R $RUN_USER:$RUN_USER $ANDROID_HOME $ANDROID_SDK_HOME $ANDROID_NDK_HOME
RUN chmod -R a+rx $ANDROID_HOME $ANDROID_SDK_HOME $ANDROID_NDK_HOME
#   Creating project directories prepared for build when running
#   `docker run`
ENV PROJECT="/project"
RUN mkdir $PROJECT
RUN chown -R $RUN_USER:$RUN_USER $PROJECT
WORKDIR $PROJECT
USER $RUN_USER
RUN echo "sdk.dir=$ANDROID_HOME" > local.properties
# Please add your HEALTHCHECK here!!!
