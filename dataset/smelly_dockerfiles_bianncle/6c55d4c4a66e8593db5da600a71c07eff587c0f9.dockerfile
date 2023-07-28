#  Python 3.6.8 base kernel based ubuntu 16.04
#  base source from official repository at https://hub.docker.com/python/
FROM ubuntu:16.04
MAINTAINER Mario Cho "m.cho@lablup.com"
#  GPG_KEY & Python version
ENV GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D"
ENV PYTHON_VERSION="3.6.8"
#  /usr/local in this image only has its own built Python.
#  ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#  http://bugs.python.org/issue19846
#  > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#  runtime dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates wget tk-dev netbase -y \
 && rm -rf /var/lib/apt/lists/*
#  Python build
RUN set -ex \
 && savedAptMark="$( apt-mark showmanual ;)" \
 && apt-get update \
 && apt-get install --no-install-recommends dpkg-dev gcc libbz2-dev libc6-dev libexpat1-dev libffi-dev libgdbm-dev liblzma-dev libncursesw5-dev libreadline-dev libsqlite3-dev libssl-dev make tk-dev wget xz-utils zlib1g-dev $( command -v gpg > /dev/null || echo 'gnupg dirmngr' ;) -y \
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
 && find /usr/local -type f -executable -not ( -name '*tkinter*' ) -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false \
 && rm -rf /var/lib/apt/lists/* \
 && find /usr/local -depth ( ( -type d -a ( -name test -o -name tests ) ) -o ( -type f -a ( -name '*.pyc' -o -name '*.pyo' ) ) ) -exec rm -rf '{}' + \
 && rm -rf /usr/src/python \
 && python3 --version
#  make some useful symlinks that are expected to exist
RUN cd /usr/local/bin \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
#  if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="18.1"
RUN set -ex ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update -y ; apt-get install --no-install-recommends wget -y ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/* ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth ( ( -type d -a ( -name test -o -name tests ) ) -o ( -type f -a ( -name '*.pyc' -o -name '*.pyo' ) ) ) -exec rm -rf '{}' + ; rm -f get-pip.py
RUN cd / ; tar czpf python.tar.gz /usr/local/bin /usr/local/lib/libpython* /usr/local/lib/python3.6* /usr/local/include
