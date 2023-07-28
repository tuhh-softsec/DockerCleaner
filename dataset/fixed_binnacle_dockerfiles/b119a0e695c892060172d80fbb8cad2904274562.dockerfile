FROM ubuntu:14.04 AS toolchain
ENV OSX_SDK_URL="https://s3.dockerproject.org/darwin/v2/" \
    OSX_SDK="MacOSX10.10.sdk" \
    OSX_MIN="10.10" \
    CTNG="1.23.0"
#   FIRST PART
#   build osx64 toolchain (stripped of man documentation)
#   the toolchain produced is not self contained, it needs clang at runtime
#
#   SECOND PART
#   build gcc (no g++) centos6-x64 toolchain
#   doc: https://crosstool-ng.github.io/docs/
#   apt-get should be all dep to build toolchain
#   sed and 1st echo are for convenience to get the toolchain in /tmp/x86_64-centos6-linux-gnu
#   other echo are to enable build by root (crosstool-NG refuse to do that by default)
#   the last 2 rm are just to save some time and space writing docker layers
#
#   THIRD PART
#   build fpm and creates a set of deb from gem
#   ruby2.0 depends on ruby1.9.3 which is install as default ruby
#   rm/ln are here to change that
#   created deb depends on rubygem-json but json gem is not build
#   so do by hand
#   might wanna make sure osx cross and the other tarball as well as the packages ends up somewhere other than tmp
#   might also wanna put them as their own layer to not have to unpack them every time?
RUN apt-get update \
 && apt-get install --no-install-recommends clang-3.8=1:3.8-2ubuntu3~trusty5 patch=2.7.1-4ubuntu2.4 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 ca-certificates=20170717~14.04.2 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 make=3.81-8.2ubuntu3 cmake=2.8.12.2-0ubuntu3 libssl-dev=1.0.1f-1ubuntu2.27 xz-utils=5.1.1alpha+20120614-2ubuntu2 -y \
 && git clone https://github.com/tpoechtrager/osxcross.git /tmp/osxcross \
 && curl -L ${OSX_SDK_URL}/${OSX_SDK}.tar.xz -o /tmp/osxcross/tarballs/${OSX_SDK}.tar.xz \
 && ln -s /usr/bin/clang-3.8 /usr/bin/clang \
 && ln -s /usr/bin/clang++-3.8 /usr/bin/clang++ \
 && ln -s /usr/bin/llvm-dsymutil-3.8 /usr/bin/dsymutil \
 && UNATTENDED=yes OSX_VERSION_MIN=${OSX_MIN} /tmp/osxcross/build.sh \
 && rm -rf /tmp/osxcross/target/SDK/${OSX_SDK}/usr/share \
 && cd /tmp \
 && tar cfJ osxcross.tar.xz osxcross/target \
 && rm -rf /tmp/osxcross \
 && apt-get install --no-install-recommends bison=2:3.0.2.dfsg-2 curl=7.35.0-1ubuntu2.20 flex=2.5.35-10.1ubuntu2 gawk=1:4.0.1+dfsg-2.1ubuntu2 gcc=4:4.8.2-1ubuntu6 g++=4:4.8.2-1ubuntu6 gperf=3.0.4-1 help2man=1.44.1 libncurses5-dev=5.9+20140118-1ubuntu1 make=3.81-8.2ubuntu3 patch=2.7.1-4ubuntu2.4 python-dev=2.7.5-5ubuntu3 texinfo=5.2.0.dfsg.1-2 xz-utils=5.1.1alpha+20120614-2ubuntu2 -y \
 && curl -L http://crosstool-ng.org/download/crosstool-ng/crosstool-ng-${CTNG}.tar.xz | tar -xJ -C /tmp/ \
 && cd /tmp/crosstool-ng-${CTNG} \
 && ./configure --enable-local \
 && make \
 && ./ct-ng x86_64-centos6-linux-gnu \
 && sed -i '/CT_PREFIX_DIR=/d' .config \
 && echo 'CT_PREFIX_DIR="/tmp/${CT_HOST:+HOST-${CT_HOST}/}${CT_TARGET}"' >> .config \
 && echo 'CT_EXPERIMENTAL=y' >> .config \
 && echo 'CT_ALLOW_BUILD_AS_ROOT=y' >> .config \
 && echo 'CT_ALLOW_BUILD_AS_ROOT_SURE=y' >> .config \
 && ./ct-ng build \
 && cd /tmp \
 && rm /tmp/x86_64-centos6-linux-gnu/build.log.bz2 \
 && tar cfJ x86_64-centos6-linux-gnu.tar.xz x86_64-centos6-linux-gnu/ \
 && rm -rf /tmp/x86_64-centos6-linux-gnu/ \
 && rm -rf /tmp/crosstool-ng-${CTNG}
#   base image to crossbuild grafana
FROM ubuntu:14.04
ENV GOVERSION="1.12.6" \
    PATH="/usr/local/go/bin:$PATH" \
    GOPATH="/go" \
    NODEVERSION="10.14.2"
COPY --from=toolchain /tmp/x86_64-centos6-linux-gnu.tar.xz /tmp/
COPY --from=toolchain /tmp/osxcross.tar.xz /tmp/
RUN apt-get update \
 && apt-get install --no-install-recommends clang-3.8=1:3.8-2ubuntu3~trusty5 gcc-aarch64-linux-gnu=4:4.8.2-1 gcc-arm-linux-gnueabihf=4:4.8.2-1 gcc-mingw-w64-x86-64=4.8.2-10ubuntu2+12 apt-transport-https=1.0.1ubuntu2.24 ca-certificates=20170717~14.04.2 curl=7.35.0-1ubuntu2.20 libfontconfig1=2.11.0-0ubuntu4.2 gcc=4:4.8.2-1ubuntu6 g++=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 make=3.81-8.2ubuntu3 rpm=4.11.1-3ubuntu0.1 xz-utils=5.1.1alpha+20120614-2ubuntu2 expect=5.45-5ubuntu1 gnupg2=2.0.22-3ubuntu1.4 unzip=6.0-9ubuntu1.5 -y \
 && ln -s /usr/bin/clang-3.8 /usr/bin/clang \
 && ln -s /usr/bin/clang++-3.8 /usr/bin/clang++ \
 && ln -s /usr/bin/llvm-dsymutil-3.8 /usr/bin/dsymutil \
 && curl -L https://nodejs.org/dist/v${NODEVERSION}/node-v${NODEVERSION}-linux-x64.tar.xz | tar -xJ --strip-components=1 -C /usr/local \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb [arch=amd64] https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update \
 && apt-get install --no-install-recommends yarn \
 && curl -L https://storage.googleapis.com/golang/go${GOVERSION}.linux-amd64.tar.gz | tar -xz -C /usr/local \
 && git clone https://github.com/raspberrypi/tools.git /opt/rpi-tools --depth=1
RUN apt-get install --no-install-recommends gcc=4:4.8.2-1ubuntu6 libc-dev make=3.81-8.2ubuntu3 -y \
 && gpg2 --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB \
 && curl -sSL https://get.rvm.io | bash -s stable \
 && /bin/bash -l -c "rvm requirements \
 && rvm install 2.2 \
 && gem install -N fpm"
COPY ./bootstrap.sh /tmp/bootstrap.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
