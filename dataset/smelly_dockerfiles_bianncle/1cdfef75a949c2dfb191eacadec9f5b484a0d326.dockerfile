FROM debian:stretch-slim
RUN set -ex \
 && buildDeps=" dpkg-dev gcc libbz2-dev libc6-dev libexpat1-dev libffi-dev libgdbm-dev liblzma-dev libncursesw5-dev libreadline-dev libsqlite3-dev libssl-dev make tcl-dev tk-dev wget xz-utils zlib1g-dev $( command -v gpg > /dev/null || echo 'gnupg dirmngr' ;) " \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/*
#  python 3.7.0 install
ENV PATH="/usr/local/bin:$PATH"
#  http://bugs.python.org/issue19846
#  > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#  runtime dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates libexpat1 libffi6 libgdbm3 libreadline7 libsqlite3-0 libssl1.1 netbase -y \
 && rm -rf /var/lib/apt/lists/*
ENV GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D"
ENV PYTHON_VERSION="3.7.0"
RUN wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
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
 && find /usr/local -depth ( ( -type d -a ( -name test -o -name tests ) ) -o ( -type f -a ( -name '*.pyc' -o -name '*.pyo' ) ) ) -exec rm -rf '{}' + \
 && rm -rf /usr/src/python
#  make some useful symlinks that are expected to exist
RUN cd /usr/local/bin \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
#  if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="10.0.1"
RUN set -ex ; apt-get update ; apt-get install --no-install-recommends wget -y ; rm -rf /var/lib/apt/lists/* ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; apt-get purge -y --auto-remove wget ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth ( ( -type d -a ( -name test -o -name tests ) ) -o ( -type f -a ( -name '*.pyc' -o -name '*.pyo' ) ) ) -exec rm -rf '{}' + ; rm -f get-pip.py
#  node 9 install
RUN groupadd --gid 1000 node \
 && useradd --uid 1000 --gid node --shell /bin/bash --create-home node
#  gpg keys listed at https://github.com/nodejs/node#release-team
RUN set -ex \
 && for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 B9AE9905FFD7803F25714661B63B535A4C206CA9 56730D5401028683275BD23C23EFEFE93C4CFFFE 77984A986EBC2AA786BC0F66B01FBB92821C587A 8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600; do gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done
ENV NODE_VERSION="9.11.2"
RUN buildDeps='xz-utils' \
 && ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='x64' ;;(ppc64el) ARCH='ppc64le' ;;(s390x) ARCH='s390x' ;;(arm64) ARCH='arm64' ;;(armhf) ARCH='armv7l' ;;(i386) ARCH='x86' ;;(*) echo "unsupported architecture" ; exit 1 ;; esac \
 && set -x \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates curl wget $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-$ARCH.tar.xz" \
 && curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION-linux-$ARCH.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xJf "node-v$NODE_VERSION-linux-$ARCH.tar.xz" -C /usr/local --strip-components=1 --no-same-owner \
 && rm "node-v$NODE_VERSION-linux-$ARCH.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt \
 && apt-get purge -y --auto-remove $buildDeps \
 && ln -s /usr/local/bin/node /usr/local/bin/nodejs
ENV YARN_VERSION="1.5.1"
RUN set -ex \
 && for key in 6A010C5166006599AA17F08146C2130DFD2497F5; do gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done \
 && curl -fsSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz" \
 && curl -fsSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz.asc" \
 && gpg --batch --verify yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz \
 && mkdir -p /opt \
 && tar -xzf yarn-v$YARN_VERSION.tar.gz -C /opt/ \
 && ln -s /opt/yarn-v$YARN_VERSION/bin/yarn /usr/local/bin/yarn \
 && ln -s /opt/yarn-v$YARN_VERSION/bin/yarnpkg /usr/local/bin/yarnpkg \
 && rm yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz
ADD . /server
WORKDIR /server
RUN npm install --production
CMD ["node", "/server/index.js"]
