FROM ubuntu:18.04
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends alien=8.95 apache2=2.4.29-1ubuntu4.27 apache2-dev=2.4.29-1ubuntu4.27 autoconf=2.69-11 bear=2.3.11-1 bison=2:3.0.4.dfsg-1build1 build-essential=12.4ubuntu1 chrpath=0.16-2 clang-7=1:7-3~ubuntu0.18.04.1 clang-format-7=1:7-3~ubuntu0.18.04.1 clang-tidy-7=1:7-3~ubuntu0.18.04.1 devscripts=2.17.12ubuntu1.1 direnv=2.15.0-1 dnsutils=1:9.11.3+dfsg-1ubuntu1.18 doxygen=1.8.13-10 dpatch=2.0.38+nmu1 dpkg-sig=0.13.1+nmu4 enchant=1.6.0-11.1 expect=5.45.4-1 figlet=2.2.5-3 flex=2.6.4-6 g++=4:7.4.0-1ubuntu2.3 gettext=0.19.8.1-6ubuntu0.3 git-buildpackage=0.9.8 ksh=93u+20120801-3.1ubuntu1 libboost-all-dev=1.65.1.0ubuntu1 libboost-dev=1.65.1.0ubuntu1 libboost-system-dev=1.65.1.0ubuntu1 libclang-7-dev=1:7-3~ubuntu0.18.04.1 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libevent-dev=2.1.8-stable-4build1 libffi-dev=3.2.1-8 libfreeradius-dev=3.0.16+dfsg-1ubuntu3.2 libgd-dev=2.2.5-4ubuntu0.5 libglib2.0-dev=2.56.4-0ubuntu0.18.04.9 libgnutls28-dev=3.5.18-1ubuntu1.6 libgsf-1-dev=1.14.41-2 libkrb5-dev=1.16-2ubuntu0.4 libldap2-dev=2.4.45+dfsg-1ubuntu1.11 libltdl-dev=2.4.6-2 libmcrypt-dev=2.5.8-3.3 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libncurses5-dev=6.1-1ubuntu1.18.04 libpango1.0-dev=1.40.14-1ubuntu0.1 libpcap-dev=1.8.1-6ubuntu1.18.04.2 libperl-dev=5.26.1-6ubuntu0.6 libpq-dev=10.23-0ubuntu0.18.04.1 libreadline-dev=7.0-3 librrd-dev=1.7.0-1build1 libsasl2-dev=2.1.27~101-g0780600+dfsg-3ubuntu2.4 libsqlite3-dev=3.22.0-1ubuntu0.7 libterm-readkey-perl=2.37-1build1 libtool=2.4.6-2 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 llvm-7-dev=1:7-3~ubuntu0.18.04.1 mono-complete=4.6.2.7+dfsg-1ubuntu1 mono-xbuild=4.6.2.7+dfsg-1ubuntu1 openssh-client=1:7.6p1-4ubuntu0.7 p7zip-full=16.02+dfsg-6 patch=2.7.6-2ubuntu1.1 pngcrush=1.7.85-1build1 poedit=2.0.6-1build1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-polib=1.1.0-3 rpcbind=0.2.3-0.6ubuntu0.18.04.4 rpm=4.14.1+dfsg1-2 rsync=3.1.2-2.1ubuntu1.6 smbclient=2:4.7.6+dfsg~ubuntu-0ubuntu2.29 texinfo=6.5.0.dfsg.1-2 tk-dev=8.6.0+9 uuid-dev=2.31.1-0.4ubuntu3.7 valgrind=1:3.13.0-2ubuntu2.3 -y \
 && rm -rf /var/lib/apt/lists/*
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash - \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -y \
 && rm -rf /var/lib/apt/lists/*
RUN pip install git+https://github.com/svenpanne/pipenv.git@41f30d7ac848fdfe3eb548ddd19b731bfa8c331a -U
RUN pip install pathlib2==2.3.7.post1 typing==3.10.0.0 backports.functools_lru_cache==1.6.4
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.6.14 ca-certificates=20211016ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 software-properties-common=0.96.24.32.20 -y \
 && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - \
 && add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $( lsb_release -cs ;) stable" \
 && apt-get update \
 && apt-get install --no-install-recommends docker-ce -y \
 && rm -rf /var/lib/apt/lists/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
