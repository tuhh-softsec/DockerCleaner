FROM ubuntu:16.04
ENV PYTHONUNBUFFERED="TRUE"
#   Install python3.6 and pip3
ENV PATH="/usr/local/bin:$PATH"
ENV LANG="C.UTF-8"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends tk-dev=8.6.0+9 -y )
RUN set -ex \
 && echo 'Acquire::CompressionTypes::Order:: "gz";' > /etc/apt/apt.conf.d/99use-gzip-compression \
 && : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends wget ca-certificates vim autoconf automake less groff dpkg-dev bzip2=1.0.* file g++ gcc imagemagick libbz2-dev libc6-dev curl libdb-dev libevent-dev libffi-dev libgeoip-dev libglib2.0-dev libjpeg-dev libkrb5-dev liblzma-dev libmagickcore-dev libmagickwand-dev libmysqlclient-dev libncurses5-dev libpng12-dev libpq-dev libreadline-dev libsqlite3-dev libssl-dev libtool libwebp-dev libxml2-dev libxslt1-dev libyaml-dev make patch xz-utils zlib1g-dev tcl tk e2fsprogs iptables xfsprogs xz-utils openjdk-8-jdk-headless fakeroot mono-mcs libcurl4-openssl-dev liberror-perl unzip
#   Install python 3.6
ENV GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D"
ENV PYTHON_VERSION="3.6.8"
RUN set -ex \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$GPG_KEY" \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && { command -v gpgconf > /dev/null \
 && gpgconf --kill all || : ; } \
 && rm -rf "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-loadable-sqlite-extensions --enable-shared --with-system-expat --with-system-ffi --without-ensurepip \
 && make -j "$( nproc ;)" \
 && make install \
 && ldconfig \
 && find /usr/local -depth
#   make some useful symlinks that are expected to exist
RUN cd /usr/local/bin \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="19.0.3"
RUN set -ex ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
RUN pip install mxnet-model-server==1.0.8 --no-cache-dir --pre
RUN mkdir -p /home/model-server/tmp
COPY dockerd-entrypoint.sh /usr/local/bin/dockerd-entrypoint.sh
COPY config.properties /home/model-server
RUN chmod +x /usr/local/bin/dockerd-entrypoint.sh
EXPOSE 8080/tcp 8081/tcp
WORKDIR /home/model-server
ENV TEMP="/home/model-server/tmp"
ENTRYPOINT ["/usr/local/bin/dockerd-entrypoint.sh"]
CMD ["serve"]
LABEL maintainer="dantu@amazon.com, rakvas@amazon.com, lufen@amazon.com, dden@amazon.com"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
