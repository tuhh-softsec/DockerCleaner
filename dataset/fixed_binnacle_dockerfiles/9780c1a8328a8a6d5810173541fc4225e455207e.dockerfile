FROM alpine:edge
MAINTAINER JAremko <w3techplaygound@gmail.com>
#   Kudos to @urzds for Xpra building example
#   NOTE: Don't forget to update xpra_sha file:
#         sha1sum  "xpra-${XPRA_VERSION}.tar.xz" > xpra_sha
ENV XPRA_VERSION="2.5"
COPY video_dummy_patches /tmp/video_dummy_patches
COPY xpra_sha /tmp/
RUN echo "http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories \
 && echo "http://nl.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories \
 && apk --no-cache upgrade \
 && apk add bash=5.2.15-r2 curl=8.0.1-r1 dbus-x11=1.14.6-r1 ffmpeg=6.0-r9 gstreamer=1.22.1-r0 libvpx=1.13.0-r1 libxcomposite=0.4.6-r1 libxdamage=1.1.6-r0 libxext=1.3.5-r0 libxfixes=6.0.1-r0 libxkbfile=1.1.2-r0 libxrandr=1.5.3-r0 libxtst=1.2.4-r1 lz4=1.9.4-r1 lzo=2.10-r3 openrc=0.46-r3 openssh=9.3_p1-r1 openssl=3.1.0-r2 py-asn1 py-cffi py-cryptography py-dbus py-enum34 py-gobject3 py-gtk py-gtkglext py-idna py-ipaddress py-lz4 py-netifaces py-numpy py-pillow py-pycryptodome py-rencode py-six py2-xxhash shared-mime-info=2.2-r2 websockify x264=0.164_git20220602-r1 xhost=1.0.9-r0 xorg-server=21.1.8-r0 --no-cache \
 && apk add autoconf=2.71-r2 automake=1.16.5-r2 build-base=0.5-r3 coreutils=9.2-r2 cython-dev ffmpeg-dev=6.0-r9 flac-dev=1.4.2-r1 git=2.40.0-r0 libc-dev=0.7.2-r4 libtool=2.4.7-r1 libvpx-dev=1.13.0-r1 libxcomposite-dev=0.4.6-r1 libxdamage-dev=1.1.6-r0 libxext-dev=1.3.5-r0 libxfixes-dev=6.0.1-r0 libxi-dev=1.8-r0 libxkbfile-dev=1.1.2-r0 libxrandr-dev=1.5.3-r0 libxtst-dev=1.2.4-r1 linux-headers=6.2-r0 lz4-dev=1.9.4-r1 musl-utils=1.2.3_git20230322-r0 npm=9.6.4-r0 opus-dev=1.3.1-r1 py-dbus-dev py-gtk-dev py-gtkglext-dev py-numpy-dev py-yuicompressor py2-pip python-dev util-macros=1.20.0-r0 which=2.21-r3 x264-dev=0.164_git20220602-r1 xorg-server-dev=21.1.8-r0 xorgproto=2022.2-r0 xvidcore-dev=1.3.7-r1 xz=5.4.2-r0 --no-cache --virtual build-deps \
 && npm install uglify-js@2 -g \
 && cd /tmp \
 && curl http://www.xpra.org/src/xpra-$XPRA_VERSION.tar.xz -o xpra.tar.xz \
 && sha1sum -c xpra_sha \
 && tar -xf "xpra.tar.xz" \
 && cd "xpra-${XPRA_VERSION}" \
 && echo -e 'Section "Module"\n Load "fb"\n EndSection' >> etc/xpra/xorg.conf \
 && python2 setup.py install --verbose --with-Xdummy --with-Xdummy_wrapper --with-bencode --with-clipboard --with-csc_swscale --with-cython_bencode --with-dbus --with-enc_ffmpeg --with-enc_x264 --with-gtk2 --with-gtk_x11 --with-pillow --with-server --with-vpx --with-vsock --with-x11 --without-client --without-csc_libyuv --without-cuda_kernels --without-cuda_rebuild --without-dec_avcodec2 --without-enc_x265 --without-gtk3 --without-mdns --without-opengl --without-printing --without-sound --without-strict --without-uinput --without-webcam \
 && mkdir -p /var/run/xpra/ \
 && cd /tmp \
 && git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
 && cd /tmp/su-exec \
 && make \
 && chmod 770 su-exec \
 && mv su-exec /usr/sbin/ \
 && git clone https://github.com/JAremko/xf86-video-dummy.git /tmp/xf86-video-dummy \
 && cd /tmp/xf86-video-dummy \
 && git apply /tmp/video_dummy_patches/Constant-DPI.patch /tmp/video_dummy_patches/fix-pointer-limits.patch /tmp/video_dummy_patches/30-bit-depth.patch \
 && aclocal \
 && autoconf \
 && automake --add-missing --force-missing \
 && ./configure \
 && make \
 && make install \
 && mv /usr/local/lib/xorg/modules/drivers/dummy_drv.so /usr/lib/xorg/modules/drivers/ \
 && apk del build-deps \
 && rm -rf /var/cache/* /tmp/* /var/log/* ~/.cache \
 && mkdir -p /var/cache/apk \
 && mkdir -p /var/run/sshd \
 && chmod 0755 /var/run/sshd \
 && rc-update add sshd \
 && rc-status \
 && touch /run/openrc/softlevel \
 && /etc/init.d/sshd start > /dev/null 2>&1 \
 && /etc/init.d/sshd stop > /dev/null 2>&1
#   docker run ... --volumes-from <ME> -e DISPLAY=<MY_DISPLAY> ... firefox
VOLUME /tmp/.X11-unix
#   Mount <some_ssh_key>.pub in here to enable xpra via ssh
VOLUME /etc/pub-keys
COPY bin/* /usr/local/bin/
ENV DISPLAY=":14" \
    SHELL="/bin/bash" \
    SSHD_PORT="22" \
    START_XORG="yes" \
    XPRA_HTML="no" \
    XPRA_MODE="start" \
    XPRA_READONLY="no" \
    XORG_DPI="96" \
    XPRA_COMPRESS="0" \
    XPRA_DPI="0" \
    XPRA_ENCODING="rgb" \
    XPRA_HTML_DPI="96" \
    XPRA_KEYBOARD_SYNC="yes" \
    XPRA_MMAP="yes" \
    XPRA_SHARING="yes" \
    XPRA_TCP_PORT="10000"
ENV GID="1000" \
    GNAME="xpra" \
    SHELL="/bin/bash" \
    UHOME="/home/xpra" \
    UID="1000" \
    UNAME="xpra"
EXPOSE $SSHD_PORT $XPRA_TCP_PORT
ENTRYPOINT ["/usr/local/bin/run"]
CMD ["xhost", "+"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
