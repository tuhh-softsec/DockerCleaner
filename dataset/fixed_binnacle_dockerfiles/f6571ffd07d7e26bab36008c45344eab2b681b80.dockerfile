FROM ubuntu:bionic
LABEL io.balena.architecture="amd64"
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.21p2-3ubuntu1.5 ca-certificates=20211016ubuntu0.18.04.1 findutils=4.6.0+git+20170828-2 gnupg=2.2.4-1ubuntu1.6 dirmngr=2.2.4-1ubuntu1.6 inetutils-ping=2:1.9.4-3ubuntu0.1 netbase=5.4 curl=7.58.0-2ubuntu3.24 udev=237-3ubuntu10.57 $( if apt-cache show 'iproute' 2> /dev/null | grep -q '^Version:' ; then echo 'iproute' ; else echo 'iproute2' ; fi ;) -y \
 && rm -rf /var/lib/apt/lists/* \
 && echo '#!/bin/sh\nset -e\nset -u\nexport DEBIAN_FRONTEND=noninteractive\nn=0\nmax=2\nuntil [ $n -gt $max ]; do\n set +e\n (\n apt-get update -qq &&\n apt-get install -y --no-install-recommends "$@"\n )\n CODE=$?\n set -e\n if [ $CODE -eq 0 ]; then\n break\n fi\n if [ $n -eq $max ]; then\n exit $CODE\n fi\n echo "apt failed, retrying"\n n=$(($n + 1))\ndone\nrm -r /var/lib/apt/lists/*' > /usr/sbin/install_packages \
 && chmod 0755 "/usr/sbin/install_packages"
#   Install packages for build variant
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 bzr=2.7.0+bzr6622-10 git=1:2.17.1-1ubuntu0.17 mercurial=4.5.3-1ubuntu2.2 openssh-client=1:7.6p1-4ubuntu0.7 subversion=1.9.7-4ubuntu1.1 autoconf=2.69-11 build-essential=12.4ubuntu1 imagemagick=8:6.9.7.4+dfsg-16ubuntu6.15 libbz2-dev=1.0.6-8.1ubuntu0.2 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libevent-dev=2.1.8-stable-4build1 libffi-dev=3.2.1-8 libglib2.0-dev=2.56.4-0ubuntu0.18.04.9 libjpeg-dev=8c-2ubuntu8 libmagickcore-dev=8:6.9.7.4+dfsg-16ubuntu6.15 libmagickwand-dev=8:6.9.7.4+dfsg-16ubuntu6.15 libncurses-dev libpq-dev=10.23-0ubuntu0.18.04.1 libreadline-dev=7.0-3 libsqlite3-dev=3.22.0-1ubuntu0.7 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxslt-dev libyaml-dev=0.1.7-2ubuntu3 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 $( if apt-cache show 'default-libmysqlclient-dev' 2> /dev/null | grep -q '^Version:' ; then echo 'default-libmysqlclient-dev' ; else echo 'libmysqlclient-dev' ; fi ;) -y \
 && rm -rf /var/lib/apt/lists/*
RUN curl -SLO "http://resin-packages.s3.amazonaws.com/resin-xbuild/v1.0.0/resin-xbuild1.0.0.tar.gz" \
 && echo "1eb099bc3176ed078aa93bd5852dbab9219738d16434c87fc9af499368423437 resin-xbuild1.0.0.tar.gz" | sha256sum -c - \
 && tar -xzf "resin-xbuild1.0.0.tar.gz" \
 && rm "resin-xbuild1.0.0.tar.gz" \
 && chmod +x resin-xbuild \
 && mv resin-xbuild /usr/bin \
 && ln -s resin-xbuild /usr/bin/cross-build-start \
 && ln -s resin-xbuild /usr/bin/cross-build-end
ENV LC_ALL="C.UTF-8"
ENV UDEV="off"
RUN mkdir -p /usr/share/man/man1
COPY entry.sh /usr/bin/entry.sh
ENTRYPOINT ["/usr/bin/entry.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
