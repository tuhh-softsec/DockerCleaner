#   Jenkins slave base
#
#   VERSION               0.0.1
FROM ubuntu:trusty
MAINTAINER Nuxeo Packagers <packagers@nuxeo.com>
RUN useradd -d /opt/jenkins -m -s /bin/bash jenkins
COPY files/mirror.local /etc/apt/sources.list
ENV DEBIAN_FRONTEND="noninteractive"
RUN dpkg --add-architecture i386 \
 && : \
 && apt-get -q -y upgrade
#   Install basic tools
RUN (apt-get update ;apt-get install --no-install-recommends ssh=1:6.6p1-2ubuntu2.13 sudo=1.8.9p5-1ubuntu1.4 lynx=2.8.8pre4-1 links=2.8-1ubuntu1 curl=7.35.0-1ubuntu2.20 wget=1.15-1ubuntu1.14.04.5 sysstat=10.2.0-1 logtail=1.3.16ubuntu0.1 vim=2:7.4.052-1ubuntu3.1 build-essential=11.6ubuntu6 zip=3.0-8 unzip=6.0-9ubuntu1.5 moreutils=0.50 apt-transport-https=1.0.1ubuntu2.24 locales=2.13+git20120306-12.1 -q -y )
#   Update default locale
RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales
RUN update-locale LANG=en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
#   Install tools for Jenkins
RUN mkdir -p /opt/build/tools
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-6-jdk=6b41-1.13.13-0ubuntu0.14.04.1 openjdk-7-jdk=7u211-2.6.17-0ubuntu0.1 git=1:1.9.1-1ubuntu0.10 mercurial=2.8.2-1ubuntu1.4 subversion=1.8.8-1ubuntu3.3 ant=1.9.3-2ubuntu0.1 ant-contrib=1.0~b3+svn177-6 xvfb=2:1.15.1-0ubuntu2.11 x11vnc=0.9.13-1.1 fluxbox=1.3.5-2 chromium-browser=65.0.3325.181-0ubuntu0.14.04.1 -q -y )
RUN ln -s /usr/lib/jvm/java-1.7.0-openjdk-amd64 /usr/lib/jvm/java-7-openjdk
#  # Firefox
RUN wget -O /tmp/firefox.tar.bz2 http://ftp.mozilla.org/pub/mozilla.org/firefox/releases/26.0/linux-x86_64/en-US/firefox-26.0.tar.bz2
RUN tar xjf /tmp/firefox.tar.bz2 -C /opt/build/tools
RUN ln -s /opt/build/tools/firefox/firefox /usr/bin/firefox
RUN rm /tmp/firefox.tar.bz2
#  # Maven
RUN wget -O /tmp/maven-2.2.1.tar.gz http://archive.apache.org/dist/maven/binaries/apache-maven-2.2.1-bin.tar.gz
RUN tar xzf /tmp/maven-2.2.1.tar.gz -C /opt/build/tools
RUN ln -s /opt/build/tools/apache-maven-2.2.1 /opt/build/tools/maven2
RUN rm /tmp/maven-2.2.1.tar.gz
RUN wget -O /tmp/maven-3.0.5.tar.gz http://archive.apache.org/dist/maven/binaries/apache-maven-3.0.5-bin.tar.gz
RUN tar xzf /tmp/maven-3.0.5.tar.gz -C /opt/build/tools
RUN ln -s /opt/build/tools/apache-maven-3.0.5 /opt/build/tools/maven-3.0
RUN ln -s /opt/build/tools/apache-maven-3.0.5 /opt/build/tools/maven3.0
RUN rm /tmp/maven-3.0.5.tar.gz
RUN wget -O /tmp/maven-3.1.1.tar.gz http://archive.apache.org/dist/maven/binaries/apache-maven-3.1.1-bin.tar.gz
RUN tar xzf /tmp/maven-3.1.1.tar.gz -C /opt/build/tools
RUN ln -s /opt/build/tools/apache-maven-3.1.1 /opt/build/tools/maven-3.1
RUN ln -s /opt/build/tools/apache-maven-3.1.1 /opt/build/tools/maven3.1
RUN rm /tmp/maven-3.1.1.tar.gz
RUN wget -O /tmp/maven-3.2.3.tar.gz http://www.us.apache.org/dist/maven/maven-3/3.2.3/binaries/apache-maven-3.2.3-bin.tar.gz
RUN tar xzf /tmp/maven-3.2.3.tar.gz -C /opt/build/tools
RUN ln -s /opt/build/tools/apache-maven-3.2.3 /opt/build/tools/maven-3.2
RUN ln -s /opt/build/tools/apache-maven-3.2.3 /opt/build/tools/maven3.2
RUN rm /tmp/maven-3.2.3.tar.gz
RUN ln -s /opt/build/tools/maven-3.2/bin/mvn /usr/bin/mvn
#  # Gradle
RUN wget -O /tmp/gradle.zip http://services.gradle.org/distributions/gradle-1.6-bin.zip
RUN unzip -d /opt/build/tools /tmp/gradle.zip
RUN ln -s /opt/build/tools/gradle-1.6 /opt/build/tools/gradle
RUN ln -s /opt/build/tools/gradle/bin/gradle /usr/bin/gradle
RUN rm /tmp/gradle.zip
#   Install other misc dependencies
RUN (apt-get update ;apt-get install --no-install-recommends linkchecker=8.6-2 s3cmd=1.1.0~beta3-2 python-dev=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 python-lxml=3.3.3-1ubuntu0.2 python-webunit=1:1.3.10-2 python-docutils=0.11-3 python-reportlab=3.0-1build1 python-pypdf=1.13-1 gnuplot=4.6.4-2 python-pip=1.5.4-1ubuntu4 tcpwatch-httpproxy=1.3b-3 dpkg-dev=1.17.5ubuntu5.8 devscripts=2.14.1ubuntu0.1 debhelper=9.20131227ubuntu1 cdbs=0.4.122ubuntu2 gnupg=1.4.16-1ubuntu2.6 redis-server=2:2.8.4-2ubuntu0.2 -q -y )
RUN easy_install -f http://funkload.nuxeo.org/snapshots/ -U funkload
#  RUN echo "deb https://get.docker.io/ubuntu docker main" > /etc/apt/sources.list.d/docker.list
#  RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 36A1D7869245C8950F966E92D8576A8BA88D21E9
#  RUN apt-get update
#  RUN apt-get -q -y install lxc-docker
#   Install converters
RUN (apt-get update ;apt-get install --no-install-recommends imagemagick=8:6.7.7.10-6ubuntu3.13 ffmpeg2theora=0.29-2build1 ufraw=0.19.2-2ubuntu2 poppler-utils=0.24.5-2ubuntu4.17 libreoffice=1:4.2.8-0ubuntu5.5 libwpd-tools=0.9.9-1 gimp=2.8.10-0ubuntu1.2 -q -y )
RUN git clone https://github.com/nuxeo/ffmpeg-nuxeo.git
ENV BUILD_YASM="true"
RUN cd ffmpeg-nuxeo \
 && ./build-all.sh true
#   Install Android SDK
RUN (apt-get update ;apt-get install --no-install-recommends libstdc++6:i386 lib32z1=1:1.2.8.dfsg-1ubuntu1.1 expect=5.45-5ubuntu1 -q -y )
RUN curl -L -Oandroid-sdk-installer https://raw.githubusercontent.com/embarkmobile/android-sdk-installer/f11ccc15b6f9d43eb1e7a61d3bc0939cc3827545/android-sdk-installer
RUN perl -p -i -E 's/-3L/-L/g' android-sdk-installer
ENV ANDROID_COMPONENTS="platform-tools,build-tools-18.1.0,android-17,sysimg-17,android-16,sysimg-16,android-8,sysimg-8,addon-google_apis-google-16,extra-android-support,extra-google-google_play_services,extra-google-m2repository,extra-android-m2repository"
RUN bash android-sdk-installer --install=$ANDROID_COMPONENTS --dir=/opt/build/tools/android-installer-broken
RUN chown -R jenkins /opt/build/tools/android-installer-broken/*
RUN cp -Rp /opt/build/tools/android-installer-broken /opt/build/tools/android-installer
RUN rm -rf /opt/build/tools/android-installer-broken
RUN perl -p -i -e 's/android-installer-broken/android-installer/g' /opt/build/tools/android-installer/env
RUN ln -s /opt/build/tools/android-installer/android-sdk-linux /opt/build/tools/android
RUN ln -s /opt/build/tools/android/platform-tools/adb /opt/build/tools/android/tools/adb
RUN ln -s /opt/build/tools/android/build-tools/18.1.0/aapt /opt/build/tools/android/platform-tools/aapt
RUN rm android-sdk-installer
#   Initialize maven repository with android artifacts
RUN su jenkins -c 'cd /opt/jenkins \
 && git clone https://github.com/mosabua/maven-android-sdk-deployer.git'
RUN su jenkins -c 'cd /opt/jenkins/maven-android-sdk-deployer \
 && source /opt/build/tools/android-installer/env \
 && mvn install -P4.1'
#   Add Oracle JDBC drivers
COPY tmpfiles/m2repo/com /opt/jenkins/.m2/repository/com
RUN chown -R jenkins /opt/jenkins/.m2/repository/com
#   Install nodejs and friends
RUN mkdir -p /usr/local/src
RUN wget -O/usr/local/src/node-v0.10.32.tar.gz http://nodejs.org/dist/v0.10.32/node-v0.10.32.tar.gz
RUN cd /usr/local/src \
 && tar xzf node-v0.10.32.tar.gz
RUN cd /usr/local/src/node-v0.10.32 \
 && ./configure \
 && make \
 && make install
RUN npm install yo@4.3.1 grunt-cli@1.4.3 gulp@4.0.2 bower@1.8.14 -g
#   Install Java 6
RUN wget -q -O/tmp/jdk6.bin "http://javadl.sun.com/webapps/download/AutoDL?BundleId=63975"
WORKDIR /usr/lib/jvm
RUN sh /tmp/jdk6.bin -noregister \
 && ln -s /usr/lib/jvm/jdk1.6.0_33 /usr/lib/jvm/java-6-sun \
 && rm /tmp/jdk6.bin
#   Install Java 8
RUN wget -q -O/tmp/jdk-8-linux-x64.tgz --no-check-certificate --header 'Cookie: oraclelicense=accept-securebackup-cookie' 'http://download.oracle.com/otn-pub/java/jdk/8u25-b17/jdk-8u25-linux-x64.tar.gz'
WORKDIR /usr/lib/jvm
RUN tar xzf /tmp/jdk-8-linux-x64.tgz \
 && ln -s /usr/lib/jvm/jdk1.8.0_25 /usr/lib/jvm/java-8 \
 && rm /tmp/jdk-8-linux-x64.tgz
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
