#   Usage of Docker image.
#
#   While building:
#   --build-arg DISK_IMAGE=<sdimg>
#         image to add to the Docker image
#   --build-arg BOOTLOADER=<u-boot.elf>
#         U-Boot ELF image to add to Docker image.
#
#   While launching:
#   -v $BUILDDIR:/mnt/build:ro
#         Use BUILDDIR from a poky build as image input.
#   -v <config-dir>:/mnt/config:ro
#         Use server.crt and/or artifact-verify-key.pem from config-dir, if it exists.
#   -e SERVER_URL=https://whatever.mender.io
#         Use SERVER_URL as server address for client.
#   -e TENANT_TOKEN=<token>
#         Use token as tenant token for client.
FROM alpine:3.6
#   Install packages
RUN apk update \
 && apk upgrade \
 && apk add util-linux=2.28.2-r2 bash=4.3.48-r1 e2fsprogs-extra=1.43.4-r0 python3=3.6.8-r0 \
 && rm -rf /var/cache/apk/*
#   Install qemu from source
RUN apk update \
 && apk add alsa-lib-dev=1.1.3-r0 bison=3.0.4-r0 curl-dev=7.61.1-r2 flex=2.6.4-r1 glib-dev=2.52.1-r0 glib-static=2.52.1-r0 gnutls-dev=3.5.13-r0 gtk+3.0-dev=3.22.12-r0 libaio-dev=0.3.110-r0 libcap-dev=2.25-r1 libcap-ng-dev=0.7.8-r0 libjpeg-turbo-dev=1.5.3-r2 libnfs-dev=1.11.0-r0 libpng-dev=1.6.37-r0 libssh2-dev=1.8.2-r0 libusb-dev=1.0.21-r0 linux-headers=4.4.6-r2 lzo-dev=2.10-r0 ncurses-dev=6.0_p20171125-r1 paxmark=0.10-r0 snappy-dev=1.1.4-r1 spice-dev=0.13.3-r4 texinfo=6.3-r0 usbredir-dev=0.7-r2 util-linux-dev=2.28.2-r2 vde2-dev=2.3.2-r7 xfsprogs-dev=4.5.0-r1 zlib-dev=1.2.11-r0 git=2.13.7-r2 alpine-sdk=0.5-r0 --virtual build-dependencies \
 && git clone --progress -b qemu-system-reset-race-fix git://github.com/mendersoftware/qemu.git \
 && cd qemu \
 && git submodule update --init dtc \
 && ./configure --target-list=arm-softmmu --disable-werror --prefix=/usr --localstatedir=/var --sysconfdir=/etc --libexecdir=/usr/lib/qemu --disable-glusterfs --disable-debug-info --disable-bsd-user --disable-werror --disable-sdl --disable-xen --disable-attr --disable-gtk \
 && make install -j4 V=1 \
 && cd .. \
 && rm -rf qemu \
 && apk del build-dependencies \
 && apk add so:libaio.so.1 so:libasound.so.2 so:libbz2.so.1 so:libc.musl-x86_64.so.1 so:libcurl.so.4 so:libepoxy.so.0 so:libgbm.so.1 so:libgcc_s.so.1 so:libglib-2.0.so.0 so:libgnutls.so.30 so:libjpeg.so.8 so:liblzo2.so.2 so:libncursesw.so.6 so:libnettle.so.6 so:libnfs.so.8 so:libpixman-1.so.0 so:libpng16.so.16 so:libsnappy.so.1 so:libspice-server.so.1 so:libssh2.so.1 so:libstdc++.so.6 so:libusb-1.0.so.0 so:libusbredirparser.so.1 so:libvdeplug.so.3 so:libz.so.1 rm /var/cache/apk/* -rf
RUN echo vexpress-qemu > /machine.txt
ARG DISK_IMAGE=scripts/docker/empty-file
ARG BOOTLOADER=scripts/docker/empty-file
COPY $BOOTLOADER ./u-boot.elf
COPY $DISK_IMAGE .
COPY scripts/mender-qemu ./
COPY scripts/docker/entrypoint.sh ./
COPY scripts/docker/setup-mender-configuration.py ./
COPY env.txt ./
#   Delete images if they are empty. This is to save space if the artifacts are
#   mounted on /mnt/build instead.
RUN if [ `stat -c %s core-image-full-cmdline-vexpress-qemu.sdimg ` -eq 0 ] ; then rm -f core-image-full-cmdline-vexpress-qemu.sdimg ; fi
RUN if [ `stat -c %s u-boot.elf ` -eq 0 ] ; then rm -f u-boot.elf ; fi
RUN chmod +x entrypoint.sh mender-qemu
EXPOSE 8822/tcp
ENTRYPOINT ["./entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
