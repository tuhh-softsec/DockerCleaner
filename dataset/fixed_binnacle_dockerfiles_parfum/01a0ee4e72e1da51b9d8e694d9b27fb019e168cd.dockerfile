FROM tcm1911/wheezy-i386
MAINTAINER sean.seefried@gmail.com
#
#  I live in Australia so change the mirror to one more appropriate
#  to where you live.
#
RUN echo "deb http://ftp.au.debian.org/debian wheezy main" > /etc/apt/sources.list
RUN echo "deb-src http://ftp.au.debian.org/debian wheezy main" >> /etc/apt/sources.list
RUN apt-get update && apt-get install --no-install-recommends build-essential ghc git libncurses5-dev cabal-install llvm-3.0 ca-certificates curl file m4 autoconf zlib1g-dev libgnutls-dev libxml2-dev libgsasl7-dev pkg-config python c2hs -y
WORKDIR /root
ENV TERM="xterm"
#  Install automake-1.14
COPY root-scripts/install-automake.sh /root/
RUN bash -c ./install-automake.sh
#  Create a new user 'androidbuilder'
COPY root-scripts/create-androidbuilder-user.sh /root/
RUN bash -c ./create-androidbuilder-user.sh
#  Log-in to the new user
USER androidbuilder
#  Update cabal and install the latest
WORKDIR /home/androidbuilder
#  Set the working directory
ENV BASE="/home/androidbuilder/ghc-build"
#  FIXME: Move the adding of the patches until later in the Docker build,
#  just before GHC is built
RUN mkdir -p $BASE/patches
COPY patches/* $BASE/patches/
COPY user-scripts/set-env.sh $BASE/
WORKDIR $BASE
#
#  Update cabal
#
COPY user-scripts/update-host-cabal.sh $BASE/
RUN ./update-host-cabal.sh
#
#  These downloads take quite a while and are annoying. I put them here
#  at the beginning to get them out of the way. If something breaks in the
#  build it would suck if you had to rewind to a Docker image BEFORE
#  the downloads had occurred.
#
COPY user-scripts/download-iconv.sh $BASE/
RUN ./download-iconv.sh
COPY user-scripts/download-ncurses.sh $BASE/
RUN ./download-ncurses.sh
COPY user-scripts/download-gmp.sh $BASE/
RUN ./download-gmp.sh
COPY user-scripts/download-ghc.sh $BASE/
RUN ./download-ghc.sh
COPY user-scripts/download-ndk.sh $BASE/
RUN ./download-ndk.sh
#
#  Start preparing the environment.
#
COPY user-scripts/unpack-ndk.sh $BASE/
RUN ./unpack-ndk.sh
COPY user-scripts/create-ndk-standalone-toolchain.sh $BASE/
RUN ./create-ndk-standalone-toolchain.sh
COPY user-scripts/unpack-ncurses.sh $BASE/
RUN ./unpack-ncurses.sh
COPY user-scripts/set-env-1.sh $BASE/
#
#  From this point on all scripts should include set-env-1.sh which
#  itself builds upon set-env.sh
#
COPY user-scripts/build-iconv.sh $BASE/
RUN ./build-iconv.sh
COPY user-scripts/build-ncurses.sh $BASE/
RUN ./build-ncurses.sh
COPY user-scripts/build-gmp.sh $BASE/
RUN ./build-gmp.sh
COPY user-scripts/build-gsasl.sh $BASE/
RUN ./build-gsasl.sh
COPY user-scripts/build-libidn.sh $BASE/
RUN ./build-libidn.sh
COPY user-scripts/build-libxml2.sh $BASE/
RUN ./build-libxml2.sh
COPY user-scripts/build-nettle.sh $BASE/
RUN ./build-nettle.sh
COPY user-scripts/build-gnutls26.sh $BASE/
RUN ./build-gnutls26.sh
#
#  At last we are ready to build GHC. First we build it for the host
#  architecture and then we build the cross-compiler.
#
#  This will take a while
COPY user-scripts/build-ghc-host.sh $BASE/
RUN ./build-ghc-host.sh
#  This takes a while too
COPY user-scripts/build-ghc-cross-compiler.sh $BASE/
RUN ./build-ghc-cross-compiler.sh
COPY user-scripts/build-hsc2hs-wrapper.sh $BASE/
RUN ./build-hsc2hs-wrapper.sh
COPY user-scripts/build-cross-compile-cabal.sh $BASE/
RUN ./build-cross-compile-cabal.sh
COPY user-scripts/add-bindir-links.sh $BASE/
RUN ./add-bindir-links.sh
COPY user-scripts/update-cabal-install.sh $BASE/
RUN ./update-cabal-install.sh
#
#  Now to add add some PATHs to the .bashrc
#
COPY user-scripts/add-paths-to-bashrc.sh $BASE/
RUN ./add-paths-to-bashrc.sh
#
#  Now to clean up the build directory. It takes up a lot of space (3.6G).
#
WORKDIR /home/androidbuilder
RUN rm -rf $BASE
COPY user-scripts/README /home/androidbuilder/
RUN cat README
