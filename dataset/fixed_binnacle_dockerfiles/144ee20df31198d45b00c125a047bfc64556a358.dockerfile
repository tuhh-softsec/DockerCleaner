FROM ubuntu:bionic
ARG AWS_REGION=us-east-1
#   install needed packages. replace httpredir apt source with cloudfront
RUN set -x \
 && sed -i "s/archive.ubuntu.com/$AWS_REGION.ec2.archive.ubuntu.com/" /etc/apt/sources.list \
 && export DEBIAN_FRONTEND=noninteractive \
 && apt-get update \
 && apt-get install --no-install-recommends gnupg1=1.4.22-3ubuntu2 -y \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 0x51716619e084dab9 \
 && echo 'deb http://cran.rstudio.com/bin/linux/ubuntu bionic-cran35/' >> /etc/apt/sources.list \
 && apt-get update
#   add ppa repository so we can install java 8 (not in any official repo for bionic)
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y \
 && add-apt-repository ppa:openjdk-r/ppa
RUN apt-get update \
 && export DEBIAN_FRONTEND=noninteractive \
 && apt-get install --no-install-recommends ant=1.10.5-3~18.04 build-essential=12.4ubuntu1 clang=1:6.0-41~exp5~ubuntu1 curl=7.58.0-2ubuntu3.24 debsigs=0.1.20 dpkg-sig=0.13.1+nmu4 expect=5.45.4-1 fakeroot=1.22-2ubuntu1 git-core libattr1-dev=1:2.4.47-2build1 libacl1-dev=2.2.52-3build1 libbz2-dev=1.0.6-8.1ubuntu0.2 libcap-dev=1:2.25-1.2 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libfuse2=2.9.7-1ubuntu1 libgtk-3-0=3.22.30-1ubuntu4 libgl1-mesa-dev=20.0.8-0ubuntu1~18.04.1 libegl1-mesa=20.0.8-0ubuntu1~18.04.1 libpam-dev libpango1.0-dev=1.40.14-1ubuntu0.1 libuser1-dev=1:0.62~dfsg-0.1ubuntu2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libxslt1-dev=1.1.29-5ubuntu0.3 lsof=4.89+dfsg-0.1 openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 pkg-config=0.29.1-0ubuntu2 python=2.7.15~rc1-1 r-base=3.4.4-1ubuntu1 sudo=1.8.21p2-3ubuntu1.5 unzip=6.0-21ubuntu1.2 uuid-dev=2.31.1-0.4ubuntu3.7 valgrind=1:3.13.0-2ubuntu2.3 wget=1.19.4-1ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y
#   ensure we use the java 8 compiler
RUN update-alternatives --set java /usr/lib/jvm/java-8-openjdk-amd64/jre/bin/java
#  # build patchelf
RUN cd /tmp \
 && wget https://nixos.org/releases/patchelf/patchelf-0.9/patchelf-0.9.tar.gz \
 && tar xzvf patchelf-0.9.tar.gz \
 && cd patchelf-0.9 \
 && ./configure \
 && make \
 && make install
#  # run install-boost twice - boost exits 1 even though it has installed good enough for our uses.
#  # https://github.com/rstudio/rstudio/blob/master/vagrant/provision-primary-user.sh#L12-L15
COPY dependencies/common/install-boost /tmp/
RUN bash /tmp/install-boost || bash /tmp/install-boost
#   set github login from build argument if defined
ARG GITHUB_LOGIN
ENV RSTUDIO_GITHUB_LOGIN="$GITHUB_LOGIN"
#   install cmake
COPY package/linux/install-dependencies /tmp/
RUN /bin/bash /tmp/install-dependencies
#   install common dependencies
RUN mkdir -p /opt/rstudio-tools/dependencies/common
COPY dependencies/common/* /opt/rstudio-tools/dependencies/common/
RUN cd /opt/rstudio-tools/dependencies/common \
 && /bin/bash ./install-common
#   install Qt SDK
COPY dependencies/linux/install-qt-sdk /tmp/
RUN mkdir -p /opt/RStudio-QtSDK \
 && export QT_SDK_DIR=/opt/RStudio-QtSDK/Qt5.12.1 \
 && export QT_QPA_PLATFORM=minimal \
 && /tmp/install-qt-sdk
#   create jenkins user, make sudo. try to keep this toward the bottom for less cache busting
ARG JENKINS_GID=999
ARG JENKINS_UID=999
RUN groupadd -g $JENKINS_GID jenkins \
 && useradd -m -d /var/lib/jenkins -u $JENKINS_UID -g jenkins jenkins \
 && echo "jenkins ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
