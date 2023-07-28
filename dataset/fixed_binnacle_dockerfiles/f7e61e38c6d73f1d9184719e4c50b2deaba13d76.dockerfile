#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM alpine:3.9
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   install ca-certificates so that HTTPS works consistently
#   other runtime dependencies for Python are installed later
RUN apk add ca-certificates=20191127-r2 --no-cache
ENV GPG_KEY="97FC712E4C024BBEA48A61ED3A5CA953F73C700D"
ENV PYTHON_VERSION="3.5.7"
RUN set -ex \
 && apk add gnupg=2.2.19-r0 tar=1.32-r0 xz=5.2.4-r0 --no-cache --virtual .fetch-deps \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --batch --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && { command -v gpgconf > /dev/null \
 && gpgconf --kill all || : ; } \
 && rm -rf "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && apk add bzip2-dev=1.0.6-r7 coreutils=8.30-r0 dpkg-dev=1.19.2-r0 dpkg=1.19.2-r0 expat-dev=2.2.8-r0 findutils=4.6.0-r1 gcc=8.3.0-r0 gdbm-dev=1.13-r1 libc-dev=0.7.1-r0 libffi-dev=3.2.1-r6 linux-headers=4.18.13-r1 make=4.2.1-r2 ncurses-dev=6.1_p20190105-r0 openssl-dev=1.1.1k-r0 pax-utils=1.2.3-r0 readline-dev=7.0.003-r1 sqlite-dev=3.28.0-r3 tcl-dev=8.6.9-r0 tk=8.6.9-r0 tk-dev=8.6.9-r0 xz-dev=5.2.4-r0 zlib-dev=1.2.11-r1 --no-cache --virtual .build-deps \
 && apk del .fetch-deps \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-loadable-sqlite-extensions --enable-shared --with-system-expat --with-system-ffi --without-ensurepip \
 && make -j "$( nproc ;)" EXTRA_CFLAGS="-DTHREAD_STACK_SIZE=0x100000" \
 && make install \
 && find /usr/local -type f -executable -not
#   make some useful symlinks that are expected to exist
RUN cd /usr/local/bin \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="19.1.1"
RUN set -ex ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
CMD ["python3"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
