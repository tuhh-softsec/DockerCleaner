#   Based off of python:3.6-slim, except that we are using ubuntu instead of debian.
FROM ubuntu:16.04
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   runtime dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 libexpat1=2.1.0-7ubuntu0.16.04.5 libffi6=3.2.1-4 libgdbm3=1.8.3-13.1 libreadline6=6.3-8ubuntu2 libsqlite3-0=3.11.0-1ubuntu1.5 libssl1.0.0=1.0.2g-1ubuntu4.20 -y ) \
 && rm -rf /var/lib/apt/lists/*
ENV GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D"
ENV PYTHON_VERSION="3.6.4"
RUN set -ex \
 && buildDeps=" dpkg-dev gcc libbz2-dev libc6-dev libexpat1-dev libffi-dev libgdbm-dev liblzma-dev libncursesw5-dev libreadline-dev libsqlite3-dev libssl-dev make tcl-dev tk-dev wget xz-utils zlib1g-dev $( command -v gpg > /dev/null || echo 'gnupg dirmngr' ;) " \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends $buildDeps -y ) \
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
RUN set -ex ; apt-get update ; (apt-get update ;apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 -y ) ; rm -rf /var/lib/apt/lists/* ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; apt-get purge -y --auto-remove wget ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
RUN : \
 && apt-get -y upgrade
#   xvfb is used to do CPU based rendering of Unity
RUN (apt-get update ;apt-get install --no-install-recommends xvfb=2:1.18.4-0ubuntu0.12 -y )
#   Install ml-agents-envs package locally
COPY ml-agents-envs /ml-agents-envs
WORKDIR /ml-agents-envs
RUN pip install -e .
#   Install ml-agents package next
COPY ml-agents /ml-agents
WORKDIR /ml-agents
RUN pip install -e .
#   port 5005 is the port used in in Editor training.
EXPOSE 5005/tcp
ENTRYPOINT ["mlagents-learn"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
