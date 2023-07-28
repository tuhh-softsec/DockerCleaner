FROM ubuntu:18.04
#  ###################################################
#  # Build environment (for manual devving)         ##
#  ###################################################
ENV TOOLCHAINDIR="/usr/src/arm-linux-3.3/toolchain_gnueabi-4.4.0_ARMv5TE/usr/bin"
ENV PATH="${TOOLCHAINDIR}:${PATH}:$HOME/.composer/vendor/mediamonks/composer-vendor-cleaner/bin"
ENV TARGET="arm-unknown-linux-uclibcgnueabi"
ENV AR="${TARGET}-ar"
ENV AS="${TARGET}-as"
ENV CC="${TARGET}-gcc"
ENV CXX="${TARGET}-g++"
ENV LD="${TARGET}-ld"
ENV NM="${TARGET}-nm"
ENV RANLIB="${TARGET}-ranlib"
ENV STRIP="${TARGET}-strip"
ENV TOPDIR="/env"
ENV SOURCEDIR="${TOPDIR}/src"
ENV PREFIXDIR="${TOPDIR}/prefix"
ENV BUILDDIR="${TOPDIR}/build"
ENV INSTALLDIR="${TOPDIR}/sdcard/firmware/bin"
ENV WEBROOT="${TOPDIR}/sdcard/firmware/www"
ENV DEBIAN_FRONTEND="noninteractive"
#  ###################################################
#  # Source utils in profile                        ##
#  ###################################################
RUN echo "source /env/tools/dev/helpers.sh" >> /root/.bashrc
#  ###################################################
#  # Install dependencies and requirements          ##
#  ###################################################
RUN echo "*** Install required packages" \
 && apt-get update -qq \
 && apt-get install --no-install-recommends autoconf=2.69-11 ca-certificates=20211016ubuntu0.18.04.1 bison=2:3.0.4.dfsg-1build1 build-essential=12.4ubuntu1 cpio=2.12+dfsg-6ubuntu0.18.04.4 curl=7.58.0-2ubuntu3.24 file=1:5.32-2ubuntu0.4 flex=2.6.4-6 gawk=1:4.1.4+dfsg-1build1 gettext=0.19.8.1-6ubuntu0.3 git=1:2.17.1-1ubuntu0.17 jq=1.5+dfsg-2 libtool=2.4.6-2 lib32z1-dev=1:1.2.11.dfsg-0ubuntu2.2 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 locales=2.27-3ubuntu1.6 make=4.1-9.1ubuntu1 ncurses-dev openssl=1.1.1-1ubuntu2.1~18.04.21 pkg-config=0.29.1-0ubuntu2 python3=3.6.7-1~18.04 python=2.7.15~rc1-1 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-dev=3.6.7-1~18.04 python-dev=2.7.15~rc1-1 rsync=3.1.2-2.1ubuntu1.6 texi2html=1.82+dfsg1-5 texinfo=6.5.0.dfsg.1-2 tofrodos=1.7.13+ds-3 unrar=1:5.5.8-1 unzip=6.0-21ubuntu1.2 vim=2:8.0.1453-1ubuntu1.11 wget=1.19.4-1ubuntu2.2 zip=3.0-11build1 -qq -y \
 && apt-get clean
#  ###################################################
#  # Configure locales                              ##
#  ###################################################
RUN locale-gen en_US.UTF-8
#  ###################################################
#  # Download and unpack toolchain                  ##
#  ###################################################
RUN echo "*** Downloading toolchain" \
 && mkdir -p /usr/src/arm-linux-3.3 \
 && curl -qs --output /tmp/toolchain.tgz https://fliphess.com/toolchain/Software/Embedded_Linux/source/toolchain_gnueabi-4.4.0_ARMv5TE.tgz
RUN echo "*** Unpacking Toolchain" \
 && cd /usr/src/arm-linux-3.3 \
 && tar xzf /tmp/toolchain.tgz
#  ###################################################
#  # Set workdir and copy files                     ##
#  ###################################################
WORKDIR /env
COPY . .
#  ###################################################
#  #                                                ##
#  ###################################################
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
