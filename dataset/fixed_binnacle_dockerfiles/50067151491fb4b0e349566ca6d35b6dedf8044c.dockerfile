#   Python 3.6.8 base kernel based ubuntu 16.04
#   base source from official repository at https://hub.docker.com/python/
FROM ubuntu:18.04
MAINTAINER Mario Cho "m.cho@lablup.com"
#   GPG_KEY & Python version
ENV GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D"
ENV PYTHON_VERSION="3.6.8"
#   /usr/local in this image only has its own built Python.
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   runtime dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 wget=1.19.4-1ubuntu2.2 tk-dev=8.6.0+9 netbase=5.4 -y \
 && rm -rf /var/lib/apt/lists/*
#   Python build
RUN set -ex \
 && savedAptMark="$( apt-mark showmanual ;)" \
 && apt-get update \
 && apt-get install --no-install-recommends dpkg-dev=1.19.0.5ubuntu2.4 gcc=4:7.4.0-1ubuntu2.3 libbz2-dev=1.0.6-8.1ubuntu0.2 libc6-dev=2.27-3ubuntu1.6 libexpat1-dev=2.2.5-3ubuntu0.9 libffi-dev=3.2.1-8 libgdbm-dev=1.14.1-6 liblzma-dev=5.2.2-1.3ubuntu0.1 libncursesw5-dev=6.1-1ubuntu1.18.04 libreadline-dev=7.0-3 libsqlite3-dev=3.22.0-1ubuntu0.7 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 make=4.1-9.1ubuntu1 tk-dev=8.6.0+9 wget=1.19.4-1ubuntu2.2 xz-utils=5.2.2-1.3ubuntu0.1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 $( command -v gpg > /dev/null || echo 'gnupg dirmngr' ;) -y \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" \
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
 && apt-mark auto '.*' > /dev/null \
 && apt-mark manual $savedAptMark \
 && find /usr/local -type f -executable -not
#   make some useful symlinks that are expected to exist
RUN cd /usr/local/bin \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="18.1"
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update -y ; apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 -y ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/* ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
RUN cd / ; tar czpf python.tar.gz /usr/local/bin /usr/local/lib/libpython* /usr/local/lib/python3.6* /usr/local/include
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
