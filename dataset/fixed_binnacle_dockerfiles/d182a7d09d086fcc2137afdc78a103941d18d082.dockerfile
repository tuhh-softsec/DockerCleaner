FROM debian:buster-slim
LABEL io.balena.architecture="amd64"
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.27-1+deb10u5 ca-certificates=20200601~deb10u2 findutils=4.6.0+git+20190209-2 gnupg=2.2.12-1+deb10u2 dirmngr=2.2.12-1+deb10u2 inetutils-ping=2:1.9.4-7+deb10u2 netbase=5.6 curl=7.64.0-4+deb10u5 udev=241-7~deb10u8 procps=2:3.3.15-2 $( if apt-cache show 'iproute' 2> /dev/null | grep -q '^Version:' ; then echo 'iproute' ; else echo 'iproute2' ; fi ;) -y \
 && rm -rf /var/lib/apt/lists/* \
 && echo '#!/bin/sh\nset -e\nset -u\nexport DEBIAN_FRONTEND=noninteractive\nn=0\nmax=2\nuntil [ $n -gt $max ]; do\n set +e\n (\n apt-get update -qq &&\n apt-get install -y --no-install-recommends "$@"\n )\n CODE=$?\n set -e\n if [ $CODE -eq 0 ]; then\n break\n fi\n if [ $n -eq $max ]; then\n exit $CODE\n fi\n echo "apt failed, retrying"\n n=$(($n + 1))\ndone\nrm -r /var/lib/apt/lists/*' > /usr/sbin/install_packages \
 && chmod 0755 "/usr/sbin/install_packages"
#   Install packages for build variant
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20200601~deb10u2 curl=7.64.0-4+deb10u5 wget=1.20.1-1.1 bzr=2.7.0+bzr6622-15 git=1:2.20.1-2+deb10u8 mercurial=4.8.2-1+deb10u1 openssh-client=1:7.9p1-10+deb10u2 subversion=1.10.4-1+deb10u3 autoconf=2.69-11 build-essential=12.6 imagemagick=8:6.9.10.23+dfsg-2.1+deb10u4 libbz2-dev=1.0.6-9.2~deb10u2 libcurl4-openssl-dev=7.64.0-4+deb10u5 libevent-dev=2.1.8-stable-4 libffi-dev=3.2.1-9 libglib2.0-dev=2.58.3-2+deb10u4 libjpeg-dev=1:1.5.2-2+deb10u1 libmagickcore-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libmagickwand-dev=8:6.9.10.23+dfsg-2.1+deb10u4 libncurses-dev=6.1+20181013-2+deb10u3 libpq-dev=11.19-0+deb10u1 libreadline-dev=7.0-5 libsqlite3-dev=3.27.2-3+deb10u2 libssl-dev=1.1.1n-0+deb10u4 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libxslt-dev libyaml-dev=0.2.1-1 zlib1g-dev=1:1.2.11.dfsg-1+deb10u2 $( if apt-cache show 'default-libmysqlclient-dev' 2> /dev/null | grep -q '^Version:' ; then echo 'default-libmysqlclient-dev' ; else echo 'libmysqlclient-dev' ; fi ;) -y \
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
ENV DEBIAN_FRONTEND="noninteractive"
ENV UDEV="off"
#   01_nodoc
RUN echo 'path-exclude /usr/share/doc/*\npath-include /usr/share/doc/*/copyright\npath-exclude /usr/share/man/*\npath-exclude /usr/share/groff/*\npath-exclude /usr/share/info/*\npath-exclude /usr/share/lintian/*\npath-exclude /usr/share/linda/*\npath-exclude /usr/share/locale/*\npath-include /usr/share/locale/en*' > /etc/dpkg/dpkg.cfg.d/01_nodoc
#   01_buildconfig
RUN echo 'APT::Get::Assume-Yes "true";\nAPT::Install-Recommends "0";\nAPT::Install-Suggests "0";\nquiet "true";' > /etc/apt/apt.conf.d/01_buildconfig
RUN mkdir -p /usr/share/man/man1
COPY entry.sh /usr/bin/entry.sh
ENTRYPOINT ["/usr/bin/entry.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
