FROM docker
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   https://github.com/docker-library/python/issues/147
ENV PYTHONIOENCODING="UTF-8"
#   install ca-certificates so that HTTPS works consistently
#   other runtime dependencies for Python are installed later
RUN apk add ca-certificates=20220614-r4 --no-cache
ENV GPG_KEY="C01E1CAD5EA2C4F0B8E3571504C367C218ADD4FF"
ENV PYTHON_VERSION="2.7.15"
RUN set -ex \
 && apk add gnupg=2.2.40-r0 tar=1.34-r2 xz=5.2.9-r0 --no-cache --virtual .fetch-deps \
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
 && apk add bzip2-dev=1.0.8-r4 coreutils=9.1-r0 dpkg-dev=1.21.9-r0 dpkg=1.21.9-r0 findutils=4.9.0-r3 gcc=12.2.1_git20220924-r4 gdbm-dev=1.23-r0 libc-dev=0.7.2-r3 libnsl-dev=2.0.0-r0 libressl-dev=3.6.2-r0 libtirpc-dev=1.3.3-r0 linux-headers=5.19.5-r0 make=4.3-r1 ncurses-dev=6.3_p20221119-r0 pax-utils=1.3.5-r1 readline-dev=8.2.0-r0 sqlite-dev=3.40.1-r0 tcl-dev=8.6.12-r1 tk=8.6.12-r1 tk-dev=8.6.12-r1 zlib-dev=1.2.13-r0 --no-cache --virtual .build-deps \
 && apk del .fetch-deps \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-shared --enable-unicode=ucs4 \
 && make -j "$( nproc ;)" EXTRA_CFLAGS="-DTHREAD_STACK_SIZE=0x100000" \
 && make install \
 && find /usr/local -type f -executable -not
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="18.1"
RUN set -ex ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
RUN pip install schedule==1.2.0
ENTRYPOINT []
CMD python2 /schedule/main.py
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
