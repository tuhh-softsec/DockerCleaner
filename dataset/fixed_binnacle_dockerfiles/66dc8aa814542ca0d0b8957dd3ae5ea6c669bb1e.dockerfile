FROM debian:stretch-slim
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   runtime dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates libexpat1 libffi6 libgdbm3 libreadline7 libsqlite3-0 libssl1.1 -y \
 && rm -rf /var/lib/apt/lists/*
ENV GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D"
ENV PYTHON_VERSION="3.6.5"
RUN set -ex \
 && buildDeps=" dpkg-dev gcc libbz2-dev libc6-dev libexpat1-dev libffi-dev libgdbm-dev liblzma-dev libncursesw5-dev libreadline-dev libsqlite3-dev libssl-dev make tcl-dev tk-dev wget xz-utils zlib1g-dev $( command -v gpg > /dev/null || echo 'gnupg dirmngr' ;) " \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
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
 && apt-get purge -y --auto-remove $buildDeps \
 && find /usr/local -depth
#   make some useful symlinks that are expected to exist
RUN cd /usr/local/bin \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="9.0.3"
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends wget -y ; rm -rf /var/lib/apt/lists/* ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; apt-get purge -y --auto-remove wget ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
RUN pip install pika==1.3.1
RUN pip install Flask==2.2.3
#   Create and define the container's working directory.
RUN mkdir /python-service
WORKDIR /python-service
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!