FROM goodrainapps/alpine:3.4
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   install ca-certificates so that HTTPS works consistently
#   the other runtime dependencies for Python are installed later
RUN apk add ca-certificates --no-cache
ENV GPG_KEY="C01E1CAD5EA2C4F0B8E3571504C367C218ADD4FF"
ENV PYTHON_VERSION="2.7.9"
RUN set -ex apk --no-cache add --virtual build-dependencies build-base gcc libc-dev libffi-dev \
 && apk add gnupg openssl tar xz --no-cache --virtual .fetch-deps \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && (gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" || gpg --keyserver pgp.mit.edu --recv-keys "$GPG_KEY" || gpg --keyserver keyserver.pgp.com --recv-keys "$GPG_KEY" ) \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && rm -rf "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && apk add bzip2-dev coreutils dpkg-dev dpkg gcc gdbm-dev libc-dev linux-headers make ncurses-dev openssl openssl-dev pax-utils readline-dev sqlite-dev tcl-dev tk tk-dev zlib-dev --no-cache --virtual .build-deps \
 && apk del .fetch-deps \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-shared --enable-unicode=ucs4 \
 && make -j "$( nproc ;)" EXTRA_CFLAGS="-DTHREAD_STACK_SIZE=0x100000" \
 && make install \
 && runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" \
 && apk add $runDeps --virtual .python-rundeps \
 && apk del .build-deps \
 && find /usr/local -depth
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="9.0.1"
RUN set -ex ; apk add openssl --no-cache --virtual .fetch-deps ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; apk del .fetch-deps ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
CMD ["python2"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
