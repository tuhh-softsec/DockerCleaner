FROM alpine:3.7
#  VERSIONS
ENV ALPINE_VERSION="3.7" \
    PYTHON_VERSION="3.6.5"
#  PATHS
ENV PYTHON_PATH="/usr/lib/python$PYTHON_VERSION" \
    PATH="/usr/lib/python$PYTHON_VERSION/bin/:${PATH}"
#  PACKAGES
#    * dumb-init: a proper init system for containers, to reap zombie children
#    * musl: standard C library
#    * lib6-compat: compatibility libraries for glibc
#    * linux-headers: commonly needed, and an unusual package name from Alpine.
#    * build-base: used so we include the basic development packages (gcc)
#    * bash: so we can access /bin/bash
#    * git: to ease up clones of repos
#    * ca-certificates: for SSL verification during Pip and easy_install
ENV PACKAGES=" dumb-init  musl  libc6-compat  linux-headers  build-base  bash  git  ca-certificates "
#  PACKAGES needed to built python
ENV PYTHON_BUILD_PACKAGES=" readline-dev  zlib-dev  bzip2-dev  sqlite-dev  libressl-dev "
RUN set -ex ; export PYTHON_MAJOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f3- | rev ;) ; export PYTHON_MINOR_VERSION=$( echo "${PYTHON_VERSION}" | rev | cut -d"." -f2- | rev ;) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/community" >> /etc/apk/repositories; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main" >> /etc/apk/repositories; apk add --no-cache $PACKAGES || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add --no-cache $PACKAGES ) ; apk add --no-cache --virtual .build-deps $PYTHON_BUILD_PACKAGES || (sed -i -e 's/dl-cdn/dl-4/g' /etc/apk/repositories \
 && apk add --no-cache --virtual .build-deps $PYTHON_BUILD_PACKAGES ) ; echo "http://dl-cdn.alpinelinux.org/alpine/v$ALPINE_VERSION/main/" > /etc/apk/repositories; git clone --depth 1 https://github.com/pyenv/pyenv /usr/lib/pyenv ; PYENV_ROOT=/usr/lib/pyenv /usr/lib/pyenv/bin/pyenv install $PYTHON_VERSION ; mv /usr/lib/pyenv/versions/$PYTHON_VERSION/ $PYTHON_PATH ; rm -rfv /usr/lib/pyenv ; cd $PYTHON_PATH/bin/ \
 && sed -i "s+/usr/lib/pyenv/versions/$PYTHON_VERSION/+$PYTHON_PATH/+g" * ; rm -f $PYTHON_PATH/bin/python$PYTHON_MAJOR_VERSION $PYTHON_PATH/bin/python$PYTHON_MINOR_VERSION $PYTHON_PATH/bin/python$PYTHON_MAJOR_VERSION-config $PYTHON_PATH/bin/python$PYTHON_MINOR_VERSION-config ; ln -sf $PYTHON_PATH/bin/python $PYTHON_PATH/bin/python$PYTHON_MAJOR_VERSION ; ln -sf $PYTHON_PATH/bin/python $PYTHON_PATH/bin/python$PYTHON_MINOR_VERSION ; ln -sf $PYTHON_PATH/bin/python-config $PYTHON_PATH/bin/python$PYTHON_MAJOR_VERSION-config ; ln -sf $PYTHON_PATH/bin/python-config $PYTHON_PATH/bin/python$PYTHON_MINOR_VERSION-config ; find /usr/lib/python$PYTHON_VERSION -depth ( -name '*.pyo' -o -name '*.pyc' -o -name 'test' -o -name 'tests' ) -exec rm -rf '{}' + ; apk del --no-cache --purge .build-deps ; rm -rf /var/cache/apk/* ; apk add --no-cache sqlite-dev
#  pip  install package
RUN pip install pip --upgrade ; pip install numpy ; pip install aiobotocore ; pip install pytz ; pip install requests ; pip install psutil
#  since we will be "always" mounting the volume, we can set this up
ENTRYPOINT ["/usr/bin/dumb-init"]
# CMD ["python"]
