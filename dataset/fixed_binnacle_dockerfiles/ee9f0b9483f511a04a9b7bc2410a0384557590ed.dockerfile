FROM ubuntu:10.04
ARG IMAGE
ENV DEFAULT_DOCKCROSS_IMAGE="${IMAGE}" \
    LANG="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8"
ARG CMAKE_VERSION=3.13.4
ARG GIT_VERSION=2.16.2
ARG NINJA_VERSION=1.9.0.g99df1.kitware.dyndep-1.jobserver-1
ARG PYTHON_VERSION=3.6.4
#   Image build scripts
COPY imagefiles/build-and-install-curl.sh imagefiles/build-and-install-git.sh imagefiles/build-and-install-openssl.sh imagefiles/build-and-install-openssh.sh imagefiles/build-and-install-python.sh imagefiles/install-cmake-binary.sh imagefiles/install-gosu-binary.sh imagefiles/install-gosu-binary-wrapper.sh imagefiles/install-ninja-binary.sh imagefiles/utils.sh /imagefiles/
ARG DEBIAN_FRONTEND=noninteractive
RUN sed -i "s/archive.ubuntu.com/old-releases.ubuntu.com/" /etc/apt/sources.list \
 && apt-get update \
 && (LANG=C LANGUAGE=C LC_ALL=C apt-get install -y locales ) \
 && locale-gen ${LANG%.*} ${LANG} \
 && apt-get -y upgrade \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf build-essential curl pkg-config unzip subversion -y \
 && apt-get install --no-install-recommends wget -y \
 && apt-get install --no-install-recommends gettext libexpat1-dev zlib1g-dev -y \
 && echo "deb http://ppa.launchpad.net/ubuntu-toolchain-r/test/ubuntu lucid main" >> /etc/apt/sources.list \
 && echo "deb-src http://ppa.launchpad.net/ubuntu-toolchain-r/test/ubuntu lucid main" >> /etc/apt/sources.list \
 && gpg --ignore-time-conflict --no-options --no-default-keyring --secret-keyring /etc/apt/secring.gpg --trustdb-name /etc/apt/trustdb.gpg --keyring /etc/apt/trusted.gpg --keyserver keyserver.ubuntu.com --recv 60C317803A41BA51845E371A1E9377A2BA9EF27F \
 && apt-get update \
 && apt-get install --no-install-recommends gcc-4.6 -y \
 && apt-get install --no-install-recommends g++-4.6 -y \
 && /imagefiles/build-and-install-openssl.sh \
 && /imagefiles/build-and-install-curl.sh \
 && /imagefiles/build-and-install-openssh.sh \
 && /imagefiles/build-and-install-git.sh \
 && /imagefiles/build-and-install-python.sh \
 && /imagefiles/install-cmake-binary.sh \
 && /imagefiles/install-gosu-binary.sh \
 && /imagefiles/install-gosu-binary-wrapper.sh \
 && /imagefiles/install-ninja-binary.sh \
 && rm -rf /imagefiles \
 && rm -rf /var/lib/apt/lists/* \
 && mkdir /var/lib/apt/lists/partial
ENV AR="/usr/bin/ar" \
    AS="/usr/bin/as" \
    CC="/usr/bin/gcc-4.6" \
    CPP="/usr/bin/cpp-4.6" \
    CXX="/usr/bin/g++-4.6"
WORKDIR /work
ENTRYPOINT ["/dockcross/entrypoint.sh"]
#   Runtime scripts
COPY imagefiles/entrypoint.sh imagefiles/dockcross /dockcross/
#   Build-time metadata as defined at http://label-schema.org
ARG BUILD_DATE
ARG IMAGE
ARG VCS_REF
ARG VCS_URL
LABEL org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.name="$IMAGE" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="$VCS_URL" \
      org.label-schema.schema-version="1.0" \
      maintainer="Jean-Christophe Fillion-Robin <jchris.fillionr@kitware.com>"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
