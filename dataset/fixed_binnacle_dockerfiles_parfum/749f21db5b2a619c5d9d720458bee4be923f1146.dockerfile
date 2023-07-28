FROM alpine:3.8
#  VERSIONS
ENV ALPINE_VERSION="3.8" \
    PYTHON_VERSION="2.7.15"
#  PATHS
ENV PYTHON_PATH="/usr/local/bin/" \
    PATH="/usr/local/lib/python$PYTHON_VERSION/bin/:/usr/local/lib/pyenv/versions/$PYTHON_VERSION/bin:${PATH}" \
    PACKAGES=" dumb-init  musl  libc6-compat  linux-headers  build-base  bash  git  ca-certificates  libssl1.0  tzdata  " \
    PYTHON_BUILD_PACKAGES=" bzip2-dev  coreutils  dpkg-dev dpkg  expat-dev  findutils  gcc  gdbm-dev  libc-dev  libffi-dev  libnsl-dev  libtirpc-dev  linux-headers  make  ncurses-dev  libressl-dev  pax-utils  readline-dev  sqlite-dev  tcl-dev  tk  tk-dev  util-linux-dev  xz-dev  zlib-dev  git  "
RUN set -ex ; export PYTHON_MAJOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f3- | rev ;) ; export PYTHON_MINOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f2- | rev ;) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/community" >> /etc/apk/repositories; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main" >> /etc/apk/repositories; apk add --no-cache $PACKAGES || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add --no-cache $PACKAGES ) ; apk add --no-cache --virtual .build-deps $PYTHON_BUILD_PACKAGES || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add --no-cache --virtual .build-deps $PYTHON_BUILD_PACKAGES ) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main/" > /etc/apk/repositories; git clone --depth 1 https://github.com/pyenv/pyenv /usr/local/lib/pyenv ; GNU_ARCH="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; PYENV_ROOT=/usr/local/lib/pyenv CONFIGURE_OPTS="--build=$GNU_ARCH --enable-loadable-sqlite-extensions --enable-shared --with-system-expat --with-system-ffi --without-ensurepip --with-shared" /usr/local/lib/pyenv/bin/pyenv install $PYTHON_VERSION ; find /usr/local -type f -executable -not ( -name '*tkinter*' ) -exec scanelf --needed --nobanner --format '%n#p' '{}' ';' | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' | grep -ve 'libpython' | xargs -rt apk add --no-cache --virtual .python-rundeps ; find /usr/local/lib/pyenv/ -mindepth 1 -name versions -prune -o -exec rm -rf {} ; || true ; find /usr/local/lib/pyenv/versions/$PYTHON_VERSION/ -depth ( -name '*.pyo' -o -name '*.pyc' -o -name 'test' -o -name 'tests' ) -exec rm -rf '{}' + ; ln -s /usr/local/lib/pyenv/versions/$PYTHON_VERSION/bin/* $PYTHON_PATH ; ln -fs /usr/share/zoneinfo/Etc/UTC /etc/localtime ; apk del --no-cache --purge .build-deps ; rm -rf /var/cache/apk/*
#  since we will be "always" mounting the volume, we can set this up
ENTRYPOINT ["/usr/bin/dumb-init"]
CMD ["python"]
