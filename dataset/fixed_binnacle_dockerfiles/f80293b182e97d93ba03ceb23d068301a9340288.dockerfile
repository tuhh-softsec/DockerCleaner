FROM ubuntu:16.04
RUN apt-get update \
 && apt-get install --no-install-recommends g++=4:5.3.1-1ubuntu1 make=4.1-6 file=1:5.25-2ubuntu1.4 curl=7.47.0-1ubuntu2.19 ca-certificates=20210119~16.04.1 python2.7=2.7.12-1ubuntu0~16.04.18 git=1:2.7.4-0ubuntu1.10 cmake=3.5.1-1ubuntu3 sudo=1.8.16-0ubuntu1.10 xz-utils=5.1.1alpha+20120614-2ubuntu2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 g++-arm-linux-gnueabi=4:5.3.1-1ubuntu1 g++-arm-linux-gnueabihf=4:5.3.1-1ubuntu1 g++-aarch64-linux-gnu=4:5.3.1-1ubuntu1 gcc-sparc64-linux-gnu=4:5.3.1-1ubuntu1 libc6-dev-sparc64-cross=2.23-0ubuntu3cross1 bzip2=1.0.6-8ubuntu0.2 patch=2.7.5-1ubuntu0.16.04.2 libssl-dev=1.0.2g-1ubuntu4.20 pkg-config=0.29.1-0ubuntu1 libnewlib-arm-none-eabi=2.2.0+git20150830.5a3d536-1 qemu-system-arm=1:2.5+dfsg-5ubuntu10.51 software-properties-common=0.96.20.10 -y
WORKDIR /build
#   Use the team-gcc-arm-embedded PPA for a newer version of Arm GCC
RUN add-apt-repository ppa:team-gcc-arm-embedded/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends gcc-arm-embedded -y
COPY dist-various-1/build-rumprun.sh /build
RUN ./build-rumprun.sh
COPY dist-various-1/install-x86_64-redox.sh /build
RUN ./install-x86_64-redox.sh
COPY dist-various-1/install-mips-musl.sh /build
RUN ./install-mips-musl.sh
COPY dist-various-1/install-mipsel-musl.sh /build
RUN ./install-mipsel-musl.sh
#   Suppress some warnings in the openwrt toolchains we downloaded
ENV STAGING_DIR="/tmp"
COPY scripts/musl.sh /build
RUN env CC=arm-linux-gnueabi-gcc CFLAGS="-march=armv5te -marm -mfloat-abi=soft" CXX=arm-linux-gnueabi-g++ CXXFLAGS="-march=armv5te -marm -mfloat-abi=soft" bash musl.sh armv5te \
 && env CC=arm-linux-gnueabi-gcc CFLAGS="-march=armv6 -marm" CXX=arm-linux-gnueabi-g++ CXXFLAGS="-march=armv6 -marm" bash musl.sh arm \
 && env CC=arm-linux-gnueabihf-gcc CFLAGS="-march=armv6 -marm -mfpu=vfp" CXX=arm-linux-gnueabihf-g++ CXXFLAGS="-march=armv6 -marm -mfpu=vfp" bash musl.sh armhf \
 && env CC=arm-linux-gnueabihf-gcc CFLAGS="-march=armv7-a" CXX=arm-linux-gnueabihf-g++ CXXFLAGS="-march=armv7-a" bash musl.sh armv7 \
 && env CC=aarch64-linux-gnu-gcc CXX=aarch64-linux-gnu-g++ bash musl.sh aarch64 \
 && env CC=mips-openwrt-linux-gcc CXX=mips-openwrt-linux-g++ bash musl.sh mips \
 && env CC=mipsel-openwrt-linux-gcc CXX=mipsel-openwrt-linux-g++ bash musl.sh mipsel \
 && rm -rf /build/*
#   FIXME(mozilla/sccache#235) this shouldn't be necessary but is currently
#   necessary to disambiguate the mips compiler with the mipsel compiler. We want
#   to give these two wrapper scripts (currently identical ones) different hashes
#   to ensure that sccache understands that they're different compilers.
RUN echo "# a" >> /usr/local/mips-linux-musl/bin/mips-openwrt-linux-musl-wrapper.sh \
 && echo "# b" >> /usr/local/mipsel-linux-musl/bin/mipsel-openwrt-linux-musl-wrapper.sh
ENV RUN_MAKE_TARGETS="thumbv6m-none-eabi"
ENV RUN_MAKE_TARGETS="$RUN_MAKE_TARGETS,thumbv7m-none-eabi"
ENV RUN_MAKE_TARGETS="$RUN_MAKE_TARGETS,thumbv7em-none-eabi"
ENV RUN_MAKE_TARGETS="$RUN_MAKE_TARGETS,thumbv7em-none-eabihf"
ENV TARGETS="asmjs-unknown-emscripten"
ENV TARGETS="$TARGETS,wasm32-unknown-emscripten"
ENV TARGETS="$TARGETS,x86_64-rumprun-netbsd"
ENV TARGETS="$TARGETS,mips-unknown-linux-musl"
ENV TARGETS="$TARGETS,mipsel-unknown-linux-musl"
ENV TARGETS="$TARGETS,arm-unknown-linux-musleabi"
ENV TARGETS="$TARGETS,arm-unknown-linux-musleabihf"
ENV TARGETS="$TARGETS,armv5te-unknown-linux-gnueabi"
ENV TARGETS="$TARGETS,armv5te-unknown-linux-musleabi"
ENV TARGETS="$TARGETS,armv7-unknown-linux-musleabihf"
ENV TARGETS="$TARGETS,aarch64-unknown-linux-musl"
ENV TARGETS="$TARGETS,sparc64-unknown-linux-gnu"
ENV TARGETS="$TARGETS,x86_64-unknown-redox"
ENV TARGETS="$TARGETS,thumbv6m-none-eabi"
ENV TARGETS="$TARGETS,thumbv7m-none-eabi"
ENV TARGETS="$TARGETS,thumbv7em-none-eabi"
ENV TARGETS="$TARGETS,thumbv7em-none-eabihf"
ENV TARGETS="$TARGETS,thumbv8m.main-none-eabi"
ENV TARGETS="$TARGETS,riscv32imc-unknown-none-elf"
ENV TARGETS="$TARGETS,riscv32imac-unknown-none-elf"
ENV TARGETS="$TARGETS,riscv64imac-unknown-none-elf"
ENV TARGETS="$TARGETS,riscv64gc-unknown-none-elf"
ENV TARGETS="$TARGETS,armebv7r-none-eabi"
ENV TARGETS="$TARGETS,armebv7r-none-eabihf"
ENV TARGETS="$TARGETS,armv7r-none-eabi"
ENV TARGETS="$TARGETS,armv7r-none-eabihf"
ENV TARGETS="$TARGETS,thumbv7neon-unknown-linux-gnueabihf"
ENV CC_mipsel_unknown_linux_musl="mipsel-openwrt-linux-gcc" \
    CC_mips_unknown_linux_musl="mips-openwrt-linux-gcc" \
    CC_sparc64_unknown_linux_gnu="sparc64-linux-gnu-gcc" \
    CC_x86_64_unknown_redox="x86_64-unknown-redox-gcc" \
    CC_armebv7r_none_eabi="arm-none-eabi-gcc" \
    CC_thumbv7neon_unknown_linux_gnueabihf="arm-linux-gnueabihf-gcc" \
    AR_thumbv7neon_unknown_linux_gnueabihf="arm-linux-gnueabihf-ar" \
    CXX_thumbv7neon_unknown_linux_gnueabihf="arm-linux-gnueabihf-g++"
ENV RUST_CONFIGURE_ARGS="--musl-root-armv5te=/musl-armv5te  --musl-root-arm=/musl-arm  --musl-root-armhf=/musl-armhf  --musl-root-armv7=/musl-armv7  --musl-root-aarch64=/musl-aarch64  --musl-root-mips=/musl-mips  --musl-root-mipsel=/musl-mipsel  --enable-emscripten  --disable-docs"
ENV SCRIPT="python2.7 ../x.py test --target $RUN_MAKE_TARGETS src/test/run-make &&  python2.7 ../x.py dist --target $TARGETS"
#   sccache
COPY scripts/sccache.sh /scripts/
RUN sh /scripts/sccache.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
