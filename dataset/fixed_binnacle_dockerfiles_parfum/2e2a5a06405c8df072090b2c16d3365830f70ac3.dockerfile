FROM alpine:3.4
#  ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#  http://bugs.python.org/issue19846
#  > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#  install ca-certificates so that HTTPS works consistently
#  the other runtime dependencies for Python are installed later
RUN apk add --no-cache ca-certificates
ENV GPG_KEY="C01E1CAD5EA2C4F0B8E3571504C367C218ADD4FF"
ENV PYTHON_VERSION="2.7.13"
RUN set -ex \
 && apk add --no-cache --virtual .fetch-deps gnupg openssl tar xz \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && rm -rf "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && apk add --no-cache --virtual .build-deps bzip2-dev coreutils dpkg-dev dpkg gcc gdbm-dev libc-dev linux-headers make ncurses-dev openssl openssl-dev pax-utils readline-dev sqlite-dev tcl-dev tk tk-dev zlib-dev \
 && apk del .fetch-deps \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-shared --enable-unicode=ucs4 \
 && make -j "$( nproc ;)" \
 && make install \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr/local | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add --virtual .python-rundeps $runDeps \
 && apk del .build-deps \
 && find /usr/local -depth ( ( -type d -a -name test -o -name tests ) -o ( -type f -a -name '*.pyc' -o -name '*.pyo' ) ) -exec rm -rf '{}' + \
 && rm -rf /usr/src/python
#  if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="9.0.1"
RUN set -ex ; apk add --no-cache --virtual .fetch-deps openssl ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; apk del .fetch-deps ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth ( ( -type d -a -name test -o -name tests ) -o ( -type f -a -name '*.pyc' -o -name '*.pyo' ) ) -exec rm -rf '{}' + ; rm -f get-pip.py
CMD ["python2"]
