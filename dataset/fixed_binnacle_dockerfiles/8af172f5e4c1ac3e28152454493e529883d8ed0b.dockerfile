FROM alpine:%%PLACEHOLDER%%
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   https://github.com/docker-library/python/issues/147
ENV PYTHONIOENCODING="UTF-8"
#   install ca-certificates so that HTTPS works consistently
#   other runtime dependencies for Python are installed later
RUN apk add ca-certificates --no-cache
ENV GPG_KEY="%%PLACEHOLDER%%"
ENV PYTHON_VERSION="%%PLACEHOLDER%%"
RUN set -ex \
 && apk add gnupg tar xz --no-cache --virtual .fetch-deps \
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
 && apk add bzip2-dev coreutils dpkg-dev dpkg expat-dev findutils gcc gdbm-dev libc-dev libffi-dev libnsl-dev libtirpc-dev linux-headers make ncurses-dev openssl-dev pax-utils readline-dev sqlite-dev tcl-dev tk tk-dev zlib-dev --no-cache --virtual .build-deps \
 && apk del .fetch-deps \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-shared --enable-unicode=ucs4 --with-system-expat --with-system-ffi \
 && make -j "$( nproc ;)" EXTRA_CFLAGS="-DTHREAD_STACK_SIZE=0x100000" \
 && make install \
 && find /usr/local -type f -executable -not
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="%%PLACEHOLDER%%"
RUN set -ex ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
CMD ["python2"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
