FROM alpine:3.4
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#   install ca-certificates so that HTTPS works consistently
#   the other runtime dependencies for Python are installed later
RUN apk add ca-certificates=20161130-r0 --no-cache
ENV GPG_KEY="C01E1CAD5EA2C4F0B8E3571504C367C218ADD4FF"
ENV PYTHON_VERSION="2.7.13"
RUN set -ex \
 && apk add gnupg=2.1.12-r1 openssl=1.0.2n-r0 tar=1.29-r1 xz=5.2.2-r1 --no-cache --virtual .fetch-deps \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && rm -rf "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && apk add bzip2-dev=1.0.6-r5 coreutils=8.25-r0 dpkg-dev=1.18.7-r0 dpkg=1.18.7-r0 gcc=5.3.0-r0 gdbm-dev=1.11-r1 libc-dev=0.7-r0 linux-headers=4.4.6-r1 make=4.1-r1 ncurses-dev=6.0_p20171125-r0 openssl=1.0.2n-r0 openssl-dev=1.0.2n-r0 pax-utils=1.1.6-r0 readline-dev=6.3.008-r4 sqlite-dev=3.13.0-r2 tcl-dev=8.6.5-r0 tk=8.6.5-r0 tk-dev=8.6.5-r0 zlib-dev=1.2.11-r0 --no-cache --virtual .build-deps \
 && apk del .fetch-deps \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-shared --enable-unicode=ucs4 \
 && make -j "$( nproc ;)" \
 && make install \
 && runDeps="$( scanelf --needed --nobanner --recursive /usr/local | awk '{ gsub(/,/, "\nso:", $2); print "so:" $2 }' | sort -u | xargs -r apk info --installed | sort -u ;)" \
 && apk add $runDeps --virtual .python-rundeps \
 && apk del .build-deps \
 && find /usr/local -depth
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="9.0.1"
RUN set -ex ; apk add openssl=1.0.2n-r0 --no-cache --virtual .fetch-deps ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; apk del .fetch-deps ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
CMD ["python2"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
