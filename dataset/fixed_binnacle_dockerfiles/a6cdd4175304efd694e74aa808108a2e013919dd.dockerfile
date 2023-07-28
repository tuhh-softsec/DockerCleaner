#   Copyright 2016 Google Inc.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   Dockerfile used to build php binaries.
#   Example usage:
#     docker run -v /mydir:/workspace deb-package-builder 5.6.29-1,7.0.14-2
#   Then you'll get deb packages in /mydir.
FROM gcr.io/gcp-runtimes/ubuntu_16_0_4
ENV PHP_DIR="/opt/php" \
    PATH="/opt/php/bin:$PATH"
#   Need to install debian packaging tools etc
RUN apt-get update -y \
 && apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 curl=7.88.1-7ubuntu1 libcurl3-gnutls=7.88.1-7ubuntu1 gettext=0.21-11 libbz2-1.0=1.0.8-5build1 libgmp10=2:6.2.1+dfsg1-1.1ubuntu1 libicu55 libjpeg-turbo8=2.1.5-2ubuntu1 liblua5.3-0=5.3.6-2 libmcrypt4=2.5.8-7 libmemcached11=1.1.4-1 libmemcachedutil2=1.1.4-1 libpcre3=2:8.39-15 libpng12-0 libpq5=15.2-1 libreadline6 librecode0=3.6-25 libsasl2-modules=2.1.28+dfsg-10 libsqlite3-0=3.40.1-1 libxml2=2.9.14+dfsg-1.1build2 libxslt1.1=1.1.35-1 sasl2-bin=2.1.28+dfsg-10 zlib1g=1:1.2.13.dfsg-1ubuntu4 debhelper=13.11.4ubuntu3 devscripts=2.23.3ubuntu2 libparse-debcontrol-perl=2.005-6 libbz2-dev=1.0.8-5build1 libcurl4-gnutls-dev=7.88.1-7ubuntu1 libfreetype6-dev=2.12.1+dfsg-4 libgettextpo-dev=0.21-11 libgmp-dev=2:6.2.1+dfsg1-1.1ubuntu1 libuv1-dev=1.44.2-1 libicu-dev=72.1-3ubuntu1 libjpeg-turbo8-dev=2.1.5-2ubuntu1 libjson-c-dev=0.16-2 liblua5.3-dev=5.3.6-2 libmagick++-dev=8:6.9.11.60+dfsg-1.6 libmcrypt-dev=2.5.8-7 libmemcached-dev=1.1.4-1 libpcre3-dev=2:8.39-15 libpng-dev=1.6.39-2 libpq-dev=15.2-1 libreadline6-dev librecode-dev=3.6-25 libsasl2-dev=2.1.28+dfsg-10 libsqlite3-dev=3.40.1-1 libsodium-dev=1.0.18-1build2 libssl-dev=3.0.8-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 build-essential=12.9ubuntu3 autoconf=2.71-3 bison=2:3.8.2+dfsg-1build1 file=1:5.44-3 g++=4:12.2.0-3ubuntu1 gcc=4:12.2.0-3ubuntu1 libc-dev make=4.3-4.1build1 patch=2.7.6-7build2 pkg-config=1.8.1-1ubuntu2 re2c=3.0-2 binutils=2.40-2ubuntu3 cmake=3.25.1-1 dh-exec=0.27 lsb-release=12.0-1ubuntu1 fakeroot=1.31-1.1 libtool=2.4.7-5 automake=1:1.16.5-1.3 autotools-dev=20220109.1 -y -q
COPY build.sh /
RUN chmod 0755 /build.sh
RUN mkdir -p /workspace
WORKDIR /workspace
#   COPY debian /workspace/debian
#   COPY extensions /workspace/extensions
#   COPY functions.sh /workspace/functions.sh
#   COPY libraries /workspace/libraries
#   COPY gpgkeys /workspace/gpgkeys
ENTRYPOINT ["/build.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
