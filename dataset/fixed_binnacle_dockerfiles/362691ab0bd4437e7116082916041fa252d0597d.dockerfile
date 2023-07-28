FROM arm32v7/ubuntu:xenial
LABEL io.balena.architecture="armv7hf"
LABEL io.balena.qemu.version="4.0.0+balena-arm"
COPY qemu-arm-static /usr/bin
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 ca-certificates=20210119~16.04.1 findutils=4.6.0+git+20160126-2 gnupg=1.4.20-1ubuntu3.3 dirmngr=2.1.11-6ubuntu2.1 inetutils-ping=2:1.9.4-1build1 netbase=5.3 curl=7.47.0-1ubuntu2.19 udev=229-4ubuntu21.31 $( if apt-cache show 'iproute' 2> /dev/null | grep -q '^Version:' ; then echo 'iproute' ; else echo 'iproute2' ; fi ;) -y \
 && rm -rf /var/lib/apt/lists/* \
 && echo '#!/bin/sh\nset -e\nset -u\nexport DEBIAN_FRONTEND=noninteractive\nn=0\nmax=2\nuntil [ $n -gt $max ]; do\n set +e\n (\n apt-get update -qq &&\n apt-get install -y --no-install-recommends "$@"\n )\n CODE=$?\n set -e\n if [ $CODE -eq 0 ]; then\n break\n fi\n if [ $n -eq $max ]; then\n exit $CODE\n fi\n echo "apt failed, retrying"\n n=$(($n + 1))\ndone\nrm -r /var/lib/apt/lists/*' > /usr/sbin/install_packages \
 && chmod 0755 "/usr/sbin/install_packages"
#   Install packages for build variant
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 bzr=2.7.0-2ubuntu3.1 git=1:2.7.4-0ubuntu1.10 mercurial=3.7.3-1ubuntu1.2 openssh-client=1:7.2p2-4ubuntu2.10 subversion=1.9.3-2ubuntu1.3 autoconf=2.69-9 build-essential=12.1ubuntu2 imagemagick=8:6.8.9.9-7ubuntu5.16 libbz2-dev=1.0.6-8ubuntu0.2 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libevent-dev=2.0.21-stable-2ubuntu0.16.04.1 libffi-dev=3.2.1-4 libglib2.0-dev=2.48.2-0ubuntu4.8 libjpeg-dev=8c-2ubuntu8 libmagickcore-dev=8:6.8.9.9-7ubuntu5.16 libmagickwand-dev=8:6.8.9.9-7ubuntu5.16 libncurses-dev libpq-dev=9.5.25-0ubuntu0.16.04.1 libreadline-dev=6.3-8ubuntu2 libsqlite3-dev=3.11.0-1ubuntu1.5 libssl-dev=1.0.2g-1ubuntu4.20 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt-dev libyaml-dev=0.1.6-3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 $( if apt-cache show 'default-libmysqlclient-dev' 2> /dev/null | grep -q '^Version:' ; then echo 'default-libmysqlclient-dev' ; else echo 'libmysqlclient-dev' ; fi ;) -y \
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
