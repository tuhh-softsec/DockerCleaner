FROM ubuntu:xenial
ARG AWS_REGION=us-east-1
#   install needed packages. replace httpredir apt source with cloudfront
RUN set -x \
 && sed -i "s/archive.ubuntu.com/$AWS_REGION.ec2.archive.ubuntu.com/" /etc/apt/sources.list \
 && export DEBIAN_FRONTEND=noninteractive \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 0x51716619e084dab9 \
 && echo 'deb http://cran.rstudio.com/bin/linux/ubuntu xenial/' >> /etc/apt/sources.list \
 && :
#   add ppa repository so we can install java 8 (not in any official repo for precise)
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -y ) \
 && add-apt-repository ppa:openjdk-r/ppa
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends ant=1.9.6-1ubuntu1.1 build-essential=12.1ubuntu2 clang-4.0=1:4.0-1ubuntu1~16.04.2 curl=7.47.0-1ubuntu2.19 debsigs=0.1.18 dpkg-sig=0.13.1+nmu2 expect=5.45-7 fakeroot=1.20.2-1ubuntu1 git-core=1:2.7.4-0ubuntu1.10 gnupg=1.4.20-1ubuntu3.3 libattr1-dev=1:2.4.47-2 libacl1-dev=2.2.52-3 libbz2-dev=1.0.6-8ubuntu0.2 libcap-dev=1:2.24-12 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libfuse2=2.9.4-1ubuntu3.1 libgtk-3-0=3.18.9-1ubuntu3.3 libgl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 libegl1-mesa=18.0.5-0ubuntu0~16.04.1 libpam-dev libpango1.0-dev=1.38.1-1 libssl-dev=1.0.2g-1ubuntu4.20 libuser1-dev=1:0.60~dfsg-1.2 libxslt1-dev=1.1.28-2.1ubuntu0.3 lsof=4.89+dfsg-0.1 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 pkg-config=0.29.1-0ubuntu1 python=2.7.12-1~16.04 r-base=3.2.3-4 sudo=1.8.16-0ubuntu1.10 unzip=6.0-20ubuntu1.1 uuid-dev=2.27.1-6ubuntu3.10 valgrind=1:3.11.0-1ubuntu4.2 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y )
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
#   install cmake
COPY package/linux/install-dependencies /tmp/
RUN /bin/bash /tmp/install-dependencies
#   add clang to system path
ENV PATH="/usr/lib/llvm-4.0/bin:$PATH"
#   install crashpad and its dependencies
COPY dependencies/common/install-crashpad /tmp/
RUN bash /tmp/install-crashpad
#   set github login from build argument if defined
ARG GITHUB_LOGIN
ENV RSTUDIO_GITHUB_LOGIN="$GITHUB_LOGIN"
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
