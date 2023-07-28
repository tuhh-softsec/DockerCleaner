FROM alpine:3.7
#   VERSIONS
ENV ALPINE_VERSION="3.7" \
    PYTHON_VERSION="3.6.5"
#   PATHS
ENV PYTHON_PATH="/usr/lib/python$PYTHON_VERSION" \
    PATH="/usr/lib/python$PYTHON_VERSION/bin/:${PATH}"
#   PACKAGES
#     * dumb-init: a proper init system for containers, to reap zombie children
#     * musl: standard C library
#     * lib6-compat: compatibility libraries for glibc
#     * linux-headers: commonly needed, and an unusual package name from Alpine.
#     * build-base: used so we include the basic development packages (gcc)
#     * bash: so we can access /bin/bash
#     * git: to ease up clones of repos
#     * ca-certificates: for SSL verification during Pip and easy_install
ENV PACKAGES=" dumb-init  musl  libc6-compat  linux-headers  build-base  bash  git  ca-certificates "
#   PACKAGES needed to built python
ENV PYTHON_BUILD_PACKAGES=" readline-dev  zlib-dev  bzip2-dev  sqlite-dev  libressl-dev "
RUN set -ex ; export PYTHON_MAJOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f3- | rev ;) ; export PYTHON_MINOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f2- | rev ;) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/community" >> /etc/apk/repositories; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main" >> /etc/apk/repositories; apk add $PACKAGES --no-cache || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add $PACKAGES --no-cache ) ; apk add $PYTHON_BUILD_PACKAGES --no-cache --virtual .build-deps || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add $PYTHON_BUILD_PACKAGES --no-cache --virtual .build-deps ) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main/" > /etc/apk/repositories; git clone --depth 1 https://github.com/pyenv/pyenv /usr/lib/pyenv ; PYENV_ROOT=/usr/lib/pyenv /usr/lib/pyenv/bin/pyenv install $PYTHON_VERSION ; mv /usr/lib/pyenv/versions/$PYTHON_VERSION/ $PYTHON_PATH ; rm -rfv /usr/lib/pyenv ; cd $PYTHON_PATH/bin/ \
 && sed -i "s+/usr/lib/pyenv/versions/$PYTHON_VERSION/+$PYTHON_PATH/+g" * ; rm -f $PYTHON_PATH/bin/python$PYTHON_MAJOR_VERSION $PYTHON_PATH/bin/python$PYTHON_MINOR_VERSION $PYTHON_PATH/bin/python$PYTHON_MAJOR_VERSION-config $PYTHON_PATH/bin/python$PYTHON_MINOR_VERSION-config ; ln -sf $PYTHON_PATH/bin/python $PYTHON_PATH/bin/python$PYTHON_MAJOR_VERSION ; ln -sf $PYTHON_PATH/bin/python $PYTHON_PATH/bin/python$PYTHON_MINOR_VERSION ; ln -sf $PYTHON_PATH/bin/python-config $PYTHON_PATH/bin/python$PYTHON_MAJOR_VERSION-config ; ln -sf $PYTHON_PATH/bin/python-config $PYTHON_PATH/bin/python$PYTHON_MINOR_VERSION-config ; find /usr/lib/python$PYTHON_VERSION -depth
#   pip  install package
RUN pip install pip==23.1 --upgrade ; pip install numpy==1.24.2 ; pip install aiobotocore==2.5.0 ; pip install pytz==2023.3 ; pip install requests==2.28.2 ; pip install psutil==5.9.4
#   since we will be "always" mounting the volume, we can set this up
ENTRYPOINT ["/usr/bin/dumb-init"]
#  CMD ["python"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
