#   NGSeasy Base Image
#   base image
FROM ubuntu:14.04.1
#   Maintainer 
MAINTAINER Stephen Newhouse stephen.j.newhouse@gmail.com
#   Set correct environment variables.
ENV HOME="/root"
ENV DEBIAN_FRONTEND="noninteractive"
#   Remain current
RUN : \
 && apt-get dist-upgrade -y
#   Basic dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=1.0.1ubuntu2.24 automake=1:1.14.1-2ubuntu1 ant=1.9.3-2ubuntu0.1 bash=4.3-7ubuntu1.7 binutils=2.24-5ubuntu14.2 perl=5.18.2-2ubuntu1.7 bioperl=1.6.923-1 build-essential=11.6ubuntu6 bzip2=1.0.6-5 c++11 cdbs=0.4.122ubuntu2 cmake=2.8.12.2-0ubuntu3 cron=3.0pl1-124ubuntu2 curl=7.35.0-1ubuntu2.20 dkms=2.2.0.3-1.1ubuntu5.14.04.10 dpkg-dev=1.17.5ubuntu5.8 g++=4:4.8.2-1ubuntu6 gpp=2.24-3 gcc=4:4.8.2-1ubuntu6 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 git-core=1:1.9.1-1ubuntu0.10 libblas-dev=1.2.20110419-7 libatlas-dev=3.10.1-4 libbz2-dev=1.0.6-5 liblzma-dev=5.1.1alpha+20120614-2ubuntu2 libpcre3-dev=1:8.31-2ubuntu2.3 libreadline-dev=6.3-4ubuntu2 make=3.81-8.2ubuntu3 mercurial=2.8.2-1ubuntu1.4 php5-curl=5.5.9+dfsg-1ubuntu4.29 python=2.7.5-5ubuntu3 python-dev=2.7.5-5ubuntu3 python-yaml=3.10-4ubuntu0.1 ncurses-dev zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 python-numpy=1:1.8.2-0ubuntu0.1 python-pip=1.5.4-1ubuntu4 sudo=1.8.9p5-1ubuntu1.4 subversion=1.8.8-1ubuntu3.3 tabix=0.2.6-2 tree=1.6.0-1 unzip=6.0-9ubuntu1.5 vim=2:7.4.052-1ubuntu3.1 wget=1.15-1ubuntu1.14.04.5 python-software-properties=0.92.37.8 libc-bin=2.19-0ubuntu6.15 llvm=1:3.4-0ubuntu1 libconfig-dev=1.4.9-2 ncurses-dev zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 yum=3.4.3-2ubuntu1 libX11-dev libXpm-dev libXft-dev libXext-dev asciidoc=8.6.9-2ubuntu1 -y )
#  ---------------------------------JAVA-------------------------------------------------------------------------------------#  
#   upgrade java
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-7-jdk=7u211-2.6.17-0ubuntu0.1 openjdk-7-doc=7u211-2.6.17-0ubuntu0.1 openjdk-7-jre-lib=7u211-2.6.17-0ubuntu0.1 -y )
#  set java
ENV JAVA_HOME="/usr/lib/jvm/java-7-openjdk-amd64/jre/bin/java"
RUN sed -i 'aPATH=$PATH:/usr/lib/jvm/java-7-openjdk-amd64/jre/bin' /root/.bashrc
#  -------------------------------Add user----------------------------------------------------------------------------------#
#   Create a pipeline user:pipeman and group:ngsgroup
RUN useradd -m -s /bin/bash pipeman \
 && cd /home/pipeman \
 && echo "#bash config file for user pipeman" >> /home/pipeman/.bashrc
RUN groupadd ngsgroup \
 && usermod -aG ngsgroup pipeman \
 && usermod -aG sudo pipeman
#  -----------------------------NGS TOOLS DIRECTORY------------------------------------------------------------------------#  
#  make pipeline install dirs
RUN mkdir /usr/local/pipeline \
 && chown pipeman:ngsgroup /usr/local/pipeline \
 && chmod 775 /usr/local/pipeline
#  -------------------------------PERMISSIONS--------------------------
RUN chmod -R 777 /usr/local/pipeline
RUN chown -R pipeman:ngsgroup /usr/local/pipeline
#  ---------------------------------------------------------------------
#  Cleanup the temp dir
RUN rm -rvf /tmp/*
#  open ports private only
EXPOSE 8080/tcp
#   Use baseimage-docker's bash.
CMD ["/bin/bash"]
#  Clean up APT when done.
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && apt-get autoclean \
 && apt-get autoremove -y \
 && rm -rf /var/lib/{apt,dpkg,cache,log}/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
