#   This Dockerfile aims to make building Hubble v2 packages easier.
#   To build an image: 1. copy pkg/scripts/pyinstaller-requirements.txt to directory with this Dockerfile
#                      2. docker build -t <image_name> .
#   The resulting image is ready to run the pyinstaller on container start and drop hubble<version>-coreos.tar.gz
#   in the /data directory. Mount /data volume into a directory on the host to access the package.
#   To run the container:  docker run -it --rm -v `pwd`:/data <image_name>
#   Requires docker 17.05 or higher
ARG OSQUERY_BUILD_ENV=remote
#  --------------- TEMP CONTAINER FOR LOCAL OSQUERY -------------------------
FROM alpine AS osquery_local
ONBUILD COPY osquery /osquery
ONBUILD RUN echo "Copying osquery from local folder"
#  --------------- TEMP CONTAINER FOR GIT OSQUERY ----------------------------
FROM alpine/git AS osquery_remote
#  to osquery pin to a different version change the following envirnment variable
ENV OSQUERY_SRC_VERSION="3.3.2"
ENV OSQUERY_GIT_URL="https://github.com/facebook/osquery.git"
ONBUILD RUN cd / \
 && git clone "$OSQUERY_GIT_URL" \
 && cd osquery/ \
 && git checkout "$OSQUERY_SRC_VERSION" \
 && echo "Fetching osquery from git"
#  --------------- TEMP CONTAINER FOR OSQUERY ( BASED UPON ARGUMENT )  ------------
FROM osquery_"$OSQUERY_BUILD_ENV" AS osquery
#  --------------- ACTUAL DOCKERFILE FOR BUILD CREATION  --------------------------
FROM debian:9
RUN : \
 && apt-get -y upgrade
#  paths that hubble or hubble parts need in the package
RUN mkdir -p /etc/hubble/hubble.d /opt/hubble /opt/osquery /var/log/hubble_osquery/backuplogs
#  patchelf build start
#  must precede osquery as at the moment, osquery won't build without patchelf
ENV PATCHELF_GIT_URL="https://github.com/NixOS/patchelf.git"
ENV PATCHELF_TEMP="/tmp/patchelf"
RUN (apt-get update ;apt-get install --no-install-recommends autoconf=2.71-3 git=1:2.39.2-1ubuntu1 make=4.3-4.1build1 g++=4:12.2.0-3ubuntu1 -y )
RUN mkdir -p "$PATCHELF_TEMP" \
 && cd "$PATCHELF_TEMP" \
 && git clone "$PATCHELF_GIT_URL" \
 && cd patchelf \
 && sed -i 's/serial-tests/parallel-tests/' configure.ac \
 && ./bootstrap.sh \
 && ./configure \
 && make \
 && make install
#  osquery build start
#  osquery should be built first since requirements for other packages can interfere with osquery dependencies
#  to build, osquery scripts want sudo and a user to sudo with.
ENV OSQUERY_BUILD_USER="osquerybuilder"
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 make=4.3-4.1build1 python ruby=1:3.1 sudo=1.9.13p1-1ubuntu2 curl=7.88.1-7ubuntu1 -y )
RUN useradd --shell /bin/bash --create-home --user-group --groups sudo "$OSQUERY_BUILD_USER" \
 && sed -i 's/^%sudo.*/%sudo\ ALL=\(ALL\)\ NOPASSWD:\ ALL/' /etc/sudoers
COPY --from=osquery /osquery /home/"$OSQUERY_BUILD_USER"/osquery
RUN mkdir -p /usr/local/osquery/ \
 && chown "$OSQUERY_BUILD_USER":"$OSQUERY_BUILD_USER" -R /usr/local/osquery/ \
 && chown "$OSQUERY_BUILD_USER":"$OSQUERY_BUILD_USER" -R /home/"$OSQUERY_BUILD_USER"/osquery
USER $OSQUERY_BUILD_USER
ENV SKIP_TESTS="1"
RUN cd /home/"$OSQUERY_BUILD_USER"/osquery \
 && make sysprep \
 && sed -i '/augeas_lenses,/,/\"Directory\ that\ contains\ augeas\ lenses\ files\"\\)\;/ s/\/usr\/share\/osquery\/lenses/\/opt\/osquery\/lenses/' osquery/tables/system/posix/augeas.cpp \
 && make deps \
 && make \
 && make strip
USER root
RUN cp -pr /home/"$OSQUERY_BUILD_USER"/osquery/build/linux/osquery/osqueryi /opt/osquery \
 && cp -pr /home/"$OSQUERY_BUILD_USER"/osquery/build/linux/osquery/osqueryd /opt/osquery/hubble_osqueryd \
 && chown -R root. /opt/osquery \
 && chmod -R 500 /opt/osquery/* \
 && mkdir -p /opt/osquery/lenses \
 && cp -r /usr/local/osquery/share/augeas/lenses/dist/* /opt/osquery/lenses \
 && chmod -R 400 /opt/osquery/lenses/*
RUN ls -lahR /opt/osquery/ \
 && /opt/osquery/osqueryi --version
#  install packages that should be needed for ligbit2 compilation and successful pyinstaller run
RUN (apt-get update ;apt-get install --no-install-recommends python-dev libffi-dev=3.4.4-1 libssl-dev=3.0.8-1ubuntu1 libyaml-dev=0.2.5-1 libssh2-1=1.10.0-3 libssh2-1-dev=1.10.0-3 autoconf=2.71-3 automake=1:1.16.5-1.3 libtool=2.4.7-5 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 python-cffi zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 cmake=3.25.1-1 python-setuptools gcc=4:12.2.0-3ubuntu1 wget=1.21.3-1ubuntu1 python-pip openssl=3.0.8-1ubuntu1 -y ) \
 && apt-get clean
#  libcurl install start
#  install libcurl to avoid depending on host version
#  requires autoconf libtool libssh2-devel zlib-devel autoconf
ENV LIBCURL_SRC_URL="https://github.com/curl/curl.git"
ENV LIBCURL_SRC_VERSION="curl-7_64_1"
ENV LIBCURL_TEMP="/tmp/libcurl"
ENV PATH="/opt/hubble/bin/:/opt/hubble/include:/opt/hubble/lib:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
RUN mkdir -p "$LIBCURL_TEMP" \
 && cd "$LIBCURL_TEMP" \
 && git clone "$LIBCURL_SRC_URL" \
 && cd curl \
 && git checkout "$LIBCURL_SRC_VERSION" \
 && ./buildconf \
 && ./configure --prefix=/opt/hubble --disable-ldap --without-nss --disable-manual --disable-gopher --disable-smtp --disable-smb --disable-imap --disable-pop3 --disable-tftp --disable-telnet --disable-dict --disable-ldaps --disable-ldap --disable-rtsp --with-libssh2 \
 && make \
 && make install
#  git install start
#  install git so that git package won't be a package dependency
#  requires make git libcurl-devel autoconf zlib-devel gcc
ENV GIT_SRC_URL="https://github.com/git/git.git"
ENV GIT_SRC_VERSION="v2.21.0"
ENV GITTEMP="/tmp/gittemp"
RUN mkdir -p "$GITTEMP" \
 && cd "$GITTEMP" \
 && git clone "$GIT_SRC_URL" \
 && cd git \
 && git checkout "$GIT_SRC_VERSION" \
 && make configure \
 && ./configure --prefix=/opt/hubble --with-tcltk=no --with-expat=no --with-python=no --with-curl=/opt/hubble \
 && echo "NO_TCLTK=YesPlease" >> config.mak.autogen \
 && echo "NO_PERL=YesPlease" >> config.mak.autogen \
 && sed -i '0,/^NO_GETTEXT/s/^NO_GETTEXT.*/NO_GETTEXT=YesPlease/' config.mak.autogen \
 && make \
 && make install
#  clean up of /opt/hubble
RUN rm /opt/hubble/bin/curl* \
 && rm -rf /opt/hubble/include /opt/hubble/share
#  libgit2 install start
#  must precede pyinstaller requirements
ENV LIBGIT2_SRC_URL="https://github.com/libgit2/libgit2/archive/v0.26.5.tar.gz"
#  it turns out github provided release files can change. so even though the code hopefully hasn't changed, the hash has.
ENV LIBGIT2_SRC_SHA256="52e28a5166564bc4365a2e4112f5e5c6e334708dbf13596241b2fd34efc1b0a9"
ENV LIBGIT2_SRC_VERSION="0.26.5"
ENV LIBGIT2TEMP="/tmp/libgit2temp"
RUN mkdir -p "$LIBGIT2TEMP" \
 && cd "$LIBGIT2TEMP" \
 && wget -q "$LIBGIT2_SRC_URL" -O libgit2.tar.gz \
 && echo "$LIBGIT2_SRC_SHA256 libgit2.tar.gz" | sha256sum -c - \
 && tar xzf libgit2.tar.gz \
 && cd libgit2-"$LIBGIT2_SRC_VERSION"/ \
 && export LIBGIT2=/usr/local/ \
 && cmake . -DCMAKE_INSTALL_PREFIX=$LIBGIT2 \
 && make \
 && make install
#  pyinstaller requirements start
#  must be preceded by libgit2 install
COPY pyinstaller-requirements.txt /
RUN pip install -v -r pyinstaller-requirements.txt
#  deb package making requirements start
RUN (apt-get update ;apt-get install --no-install-recommends ruby=1:3.1 ruby-dev=1:3.1 rubygems gcc=4:12.2.0-3ubuntu1 make=4.3-4.1build1 -y ) \
 && gem install fpm --version 1.15.1 --no-ri --no-rdoc
#  pyinstaller start
#  commands specified for ENTRYPOINT and CMD are executed when the container is run, not when the image is built
#  use the following variables to choose the version of hubble
ENV HUBBLE_CHECKOUT="v3.0.1"
ENV HUBBLE_GIT_URL="https://github.com/hubblestack/hubble.git"
ENV HUBBLE_VERSION="3.0.1"
ENV HUBBLE_ITERATION="1"
ENV HUBBLE_URL="https://github.com/hubblestack/hubble"
ENV HUBBLE_SRC_PATH="/hubble_src"
ENV _HOOK_DIR="./pkg/"
ENV _BINARY_LOG_LEVEL="INFO"
ENV _INCLUDE_PATH=""
ENV LD_LIBRARY_PATH="/opt/hubble/lib:/lib:/lib64:/usr/lib:/usr/lib64:/usr/local/lib:/usr/local/lib64"
RUN git clone "$HUBBLE_GIT_URL" "$HUBBLE_SRC_PATH" \
 && cd "$HUBBLE_SRC_PATH" \
 && git checkout "$HUBBLE_CHECKOUT" \
 && cp -rf "$HUBBLE_SRC_PATH" /hubble_build \
 && sed -i "s/BRANCH_NOT_SET/${HUBBLE_CHECKOUT}/g" /hubble_build/hubblestack/__init__.py \
 && sed -i "s/COMMIT_NOT_SET/`git describe `/g" /hubble_build/hubblestack/__init__.py
RUN mkdir /data
VOLUME /data
WORKDIR /hubble_build
ENTRYPOINT ["/bin/bash", "-o", "xtrace", "-c"]
CMD [ "if [ -f /data/hubble_buildinfo ] ; then echo \"\" >> /hubble_build/hubblestack/__init__.py ; cat /data/hubble_buildinfo >> /hubble_build/hubblestack/__init__.py; fi \
 && pyinstaller --onedir --noconfirm --log-level ${_BINARY_LOG_LEVEL} --additional-hooks-dir=${_HOOK_DIR} --runtime-hook=pkg/pyinstaller-runtimehooks/pathopthubble.py hubble.py \
 && mkdir -p /var/log/hubble_osquery/backuplogs \
 && cp -rf /hubble_build/conf/hubble /etc/hubble/ \
 && cp -rf /hubble_build/conf/hubble-profile.sh /etc/profile.d/ \
 && cp -pr /hubble_build/dist/hubble /opt/hubble/hubble-libs \
 && ln -s /opt/hubble/hubble-libs/hubble /opt/hubble/hubble \
 && rm -rf /opt/hubble/hubble-libs/librpm* \
 && cp /usr/lib/x86_64-linux-gnu/libssh2.so.1 /opt/hubble/hubble-libs \
 && tar -cPvzf /data/hubblestack-${HUBBLE_VERSION}.tar.gz /etc/hubble /opt/hubble /opt/osquery /etc/profile.d/hubble-profile.sh /var/log/hubble_osquery/backuplogs \
 && mkdir -p /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION} \
 && tar -xzvf /data/hubblestack-${HUBBLE_VERSION}.tar.gz -C /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION} \
 && mkdir -p /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION}/etc/init.d \
 && cp /hubble_build/pkg/hubble /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION}/etc/init.d/ \
 && mkdir -p /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION}/usr/lib/systemd/system \
 && cp /hubble_build/pkg/hubble.service /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION}/usr/lib/systemd/system/ \
 && cp -f /hubble_build/conf/hubble /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION}/etc/hubble/ \
 && if [ -f /data/hubble ] ; then cp /data/hubble /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION}/etc/hubble/ ; fi \
 && if [ -d /data/opt ] ; then cp -r /data/opt/* /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION}/opt/ ; fi \
 && cd /hubble_build/debbuild/hubblestack-${HUBBLE_VERSION} \
 && mkdir -p usr/bin \
 && ln -s /opt/hubble/hubble usr/bin/hubble \
 && fpm -s dir -t deb -n hubblestack -v ${HUBBLE_VERSION} --iteration ${HUBBLE_ITERATION} --url ${HUBBLE_URL} --deb-no-default-config-files --after-install /hubble_build/conf/afterinstall-systemd.sh --after-upgrade /hubble_build/conf/afterupgrade.sh --before-remove /hubble_build/conf/beforeremove.sh etc/hubble etc/init.d opt usr /var/log/hubble_osquery/backuplogs \
 && cp hubblestack_${HUBBLE_VERSION}-${HUBBLE_ITERATION}_amd64.deb /data/hubblestack-${HUBBLE_VERSION}-${HUBBLE_ITERATION}.deb9.amd64.deb \
 && openssl dgst -sha256 /data/hubblestack-${HUBBLE_VERSION}-${HUBBLE_ITERATION}.deb9.amd64.deb > /data/hubblestack-${HUBBLE_VERSION}-${HUBBLE_ITERATION}.deb9.amd64.deb.sha256" ]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
