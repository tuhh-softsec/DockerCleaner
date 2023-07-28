FROM alpine:3.8
#   VERSIONS
ENV ALPINE_VERSION="3.8" \
    PYTHON_VERSION="3.6.8"
#   PATHS
ENV PYTHON_PATH="/usr/local/bin/" \
    PATH="/usr/local/lib/python$PYTHON_VERSION/bin/:/usr/local/lib/pyenv/versions/$PYTHON_VERSION/bin:${PATH}" \
    PACKAGES=" dumb-init  musl  libc6-compat  linux-headers  build-base  bash  git  ca-certificates  libssl1.0  tzdata  " \
    PYTHON_BUILD_PACKAGES=" bzip2-dev  coreutils  dpkg-dev dpkg  expat-dev  findutils  gcc  gdbm-dev  libc-dev  libffi-dev  libnsl-dev  libtirpc-dev  linux-headers  make  ncurses-dev  libressl-dev  pax-utils  readline-dev  sqlite-dev  tcl-dev  tk  tk-dev  util-linux-dev  xz-dev  zlib-dev  git  "
RUN set -ex ; export PYTHON_MAJOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f3- | rev ;) ; export PYTHON_MINOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f2- | rev ;) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/community" >> /etc/apk/repositories; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main" >> /etc/apk/repositories; apk add $PACKAGES --no-cache || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add $PACKAGES --no-cache ) ; apk add $PYTHON_BUILD_PACKAGES --no-cache --virtual .build-deps || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add $PYTHON_BUILD_PACKAGES --no-cache --virtual .build-deps ) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main/" > /etc/apk/repositories; git clone --depth 1 https://github.com/pyenv/pyenv /usr/local/lib/pyenv ; GNU_ARCH="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; PYENV_ROOT=/usr/local/lib/pyenv CONFIGURE_OPTS="--build=$GNU_ARCH --enable-loadable-sqlite-extensions --enable-shared --with-system-expat --with-system-ffi --without-ensurepip --with-shared" /usr/local/lib/pyenv/bin/pyenv install $PYTHON_VERSION ; find /usr/local -type f -executable -not
#   Copy in the entrypoint script -- this installs prerequisites on container start.
COPY entrypoint.sh /entrypoint.sh
#   install requirements
#   this way when you build you won't need to install again
#   and since COPY is cached we don't need to wait
ONBUILD COPY requirements.txt /tmp/requirements.txt
#   Run the dependencies installer and then allow it to be run again if needed.
ONBUILD RUN /entrypoint.sh -r /tmp/requirements.txt
ONBUILD RUN rm -f /requirements.installed
#   since we will be "always" mounting the volume, we can set this up
ENTRYPOINT ["/usr/bin/dumb-init"]
CMD ["python"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
