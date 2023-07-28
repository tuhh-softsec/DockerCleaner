FROM arm64v8/ubuntu:trusty
LABEL io.balena.architecture="aarch64"
LABEL io.balena.qemu.version="4.0.0+balena-aarch64"
COPY qemu-aarch64-static /usr/bin
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.9p5-1ubuntu1.4 ca-certificates=20170717~14.04.2 findutils=4.4.2-7 gnupg=1.4.16-1ubuntu2.6 dirmngr=1.1.1-1.1 inetutils-ping=2:1.9.2-1 netbase=5.2 curl=7.35.0-1ubuntu2.20 udev=204-5ubuntu20.31 $( if apt-cache show 'iproute' 2> /dev/null | grep -q '^Version:' ; then echo 'iproute' ; else echo 'iproute2' ; fi ;) -y \
 && rm -rf /var/lib/apt/lists/* \
 && echo '#!/bin/sh\nset -e\nset -u\nexport DEBIAN_FRONTEND=noninteractive\nn=0\nmax=2\nuntil [ $n -gt $max ]; do\n set +e\n (\n apt-get update -qq &&\n apt-get install -y --no-install-recommends "$@"\n )\n CODE=$?\n set -e\n if [ $CODE -eq 0 ]; then\n break\n fi\n if [ $n -eq $max ]; then\n exit $CODE\n fi\n echo "apt failed, retrying"\n n=$(($n + 1))\ndone\nrm -r /var/lib/apt/lists/*' > /usr/sbin/install_packages \
 && chmod 0755 "/usr/sbin/install_packages"
#   Install packages for build variant
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20170717~14.04.2 curl=7.35.0-1ubuntu2.20 wget=1.15-1ubuntu1.14.04.5 bzr=2.6.0+bzr6593-1ubuntu1.6 git=1:1.9.1-1ubuntu0.10 mercurial=2.8.2-1ubuntu1.4 openssh-client=1:6.6p1-2ubuntu2.13 subversion=1.8.8-1ubuntu3.3 autoconf=2.69-6 build-essential=11.6ubuntu6 imagemagick=8:6.7.7.10-6ubuntu3.13 libbz2-dev=1.0.6-5 libcurl4-openssl-dev=7.35.0-1ubuntu2.20 libevent-dev=2.0.21-stable-1ubuntu1.14.04.2 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libglib2.0-dev=2.40.2-0ubuntu1.1 libjpeg-dev=8c-2ubuntu8 libmagickcore-dev=8:6.7.7.10-6ubuntu3.13 libmagickwand-dev=8:6.7.7.10-6ubuntu3.13 libncurses-dev libpq-dev=9.3.24-0ubuntu0.14.04 libreadline-dev=6.3-4ubuntu2 libsqlite3-dev=3.8.2-1ubuntu2.2 libssl-dev=1.0.1f-1ubuntu2.27 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt-dev libyaml-dev=0.1.4-3ubuntu3.1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 $( if apt-cache show 'default-libmysqlclient-dev' 2> /dev/null | grep -q '^Version:' ; then echo 'default-libmysqlclient-dev' ; else echo 'libmysqlclient-dev' ; fi ;) -y \
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
