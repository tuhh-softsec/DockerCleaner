FROM alpine:3.8
#   VERSIONS
ENV ALPINE_VERSION="3.8" \
    PYTHON_VERSION="3.7.2"
#   PATHS
ENV PYTHON_PATH="/usr/local/bin/" \
    PATH="/usr/local/lib/python$PYTHON_VERSION/bin/:/usr/local/lib/pyenv/versions/$PYTHON_VERSION/bin:${PATH}" \
    PACKAGES=" dumb-init  bash  ca-certificates  " \
    PYTHON_BUILD_PACKAGES=" bzip2-dev  coreutils  dpkg-dev dpkg  expat-dev  findutils  gcc  gdbm-dev  libc-dev  libffi-dev  libnsl-dev  libtirpc-dev  linux-headers  make  ncurses-dev  libressl-dev  pax-utils  readline-dev  sqlite-dev  tcl-dev  tk  tk-dev  util-linux-dev  xz-dev  zlib-dev  git  " \
    BUILD_PACKAGES=" build-base  linux-headers  libc6-compat  git  "
RUN set -ex ; export PYTHON_MAJOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f3- | rev ;) ; export PYTHON_MINOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f2- | rev ;) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/community" >> /etc/apk/repositories; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main" >> /etc/apk/repositories; apk add $PACKAGES --no-cache || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add $PACKAGES --no-cache ) ; apk add $PYTHON_BUILD_PACKAGES --no-cache --virtual .build-deps || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add $PYTHON_BUILD_PACKAGES --no-cache --virtual .build-deps ) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main/" > /etc/apk/repositories; git clone --depth 1 https://github.com/pyenv/pyenv /usr/local/lib/pyenv ; GNU_ARCH="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; PYENV_ROOT=/usr/local/lib/pyenv CONFIGURE_OPTS="--build=$GNU_ARCH --enable-loadable-sqlite-extensions --enable-shared --with-system-expat --with-system-ffi --without-ensurepip --with-shared" /usr/local/lib/pyenv/bin/pyenv install $PYTHON_VERSION ; find /usr/local -type f -executable -not
#   Copy in the entrypoint script -- this installs prerequisites on container start.
COPY entrypoint.sh /entrypoint.sh
#   This script installs APK and Pip prerequisites on container start, or ONBUILD. Notes:
#     * Reads the -a flags and /apk-requirements.txt for install requests
#     * Reads the -b flags and /build-requirements.txt for build packages -- removed when build is complete
#     * Reads the -p flags and /requirements.txt for Pip packages
#     * Reads the -r flag to specify a different file path for /requirements.txt
ENTRYPOINT ["/usr/bin/dumb-init", "bash", "/entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
