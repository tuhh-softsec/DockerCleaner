FROM debian:stretch
MAINTAINER Jacob Chen "jacob2.chen@rock-chips.com"
#   setup multiarch enviroment
RUN dpkg --add-architecture arm64
RUN echo "deb-src http://deb.debian.org/debian stretch main" >> /etc/apt/sources.list
RUN echo "deb-src http://deb.debian.org/debian stretch-updates main" >> /etc/apt/sources.list
RUN echo "deb-src http://security.debian.org stretch/updates main" >> /etc/apt/sources.list
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends crossbuild-essential-arm64=12.3 -y )
#   perpare build dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends sudo=1.8.19p1-2.1+deb9u3 git=1:2.11.0-3+deb9u7 fakeroot=1.21-3.1 devscripts=2.17.6+deb9u2 cmake=3.7.2-1 vim=2:8.0.0197-4+deb9u7 qemu-user-static=1:2.8+dfsg-6+deb9u17 binfmt-support=2.1.6-2 dh-make=2.201608 dh-exec=0.23+b1 pkg-kde-tools=0.15.24 device-tree-compiler=1.4.2-1 bc=1.06.95-9+b3 cpio=2.11+dfsg-6 parted=3.2-17 dosfstools=4.1-1 mtools=4.0.18-2+b1 libssl-dev=1.1.0l-1~deb9u6 g++-aarch64-linux-gnu=4:6.3.0-4 -y )
RUN : \
 && apt-get build-dep -y -a arm64 libdrm
RUN : \
 && apt-get build-dep -y -a arm64 xorg-server
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libgstreamer-plugins-bad1.0-dev:arm64 libgstreamer-plugins-base1.0-dev:arm64 libgstreamer1.0-dev:arm64 libgstreamermm-1.0-dev:arm64 libqt5gstreamer-dev:arm64 libqtgstreamer-dev:arm64 libxfont1-dev:arm64 libxxf86dga-dev:arm64 libunwind-dev:arm64 libnetcdf-dev:arm64 -y )
RUN cp /usr/lib/pkgconfig/xf86dgaproto.pc /usr/lib/aarch64-linux-gnu/pkgconfig/xf86dgaproto.pc
#  # qt-multimedia
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends qt5-qmake=5.7.1+dfsg-3+deb9u3 qt5-qmake:arm64 qtbase5-dev:arm64 qttools5-dev-tools:arm64 qtbase5-dev-tools:arm64 libpulse-dev:arm64 qtbase5-private-dev:arm64 qtbase5-dev:arm64 libasound2-dev:arm64 libqt5quick5:arm64 libqt5multimediaquick-p5:arm64 qtdeclarative5-dev:arm64 libopenal-dev:arm64 qtmultimedia5-examples:arm64 libqt5multimediawidgets5:arm64 qtmultimedia5-dev:arm64 qtconnectivity5-dev:arm64 -y )
#  # opencv
#
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libhighgui-dev:arm64 libopencv-calib3d-dev:arm64 libopencv-calib3d2.4v5:arm64 libopencv-contrib-dev:arm64 libopencv-core-dev:arm64 libopencv-core2.4v5:arm64 libopencv-features2d-dev:arm64 libopencv-features2d2.4v5:arm64 libopencv-flann-dev:arm64 libopencv-gpu-dev:arm64 libopencv-highgui-dev:arm64 libopencv-imgproc-dev:arm64 libopencv-legacy-dev:arm64 libopencv-ml-dev:arm64 libopencv-objdetect-dev:arm64 libopencv-ocl-dev:arm64 libopencv-photo-dev:arm64 libopencv-stitching-dev:arm64 libopencv-superres-dev:arm64 libopencv-ts-dev:arm64 libopencv-video-dev:arm64 libopencv-videostab-dev:arm64 -y )
RUN apt-get download libopencv-dev:arm64
RUN dpkg -x libopencv*.deb /
#  # gstreamer-plugin-good
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libgstreamer1.0-dev:arm64 libraw1394-dev:arm64 libiec61883-dev:arm64 libavc1394-dev:arm64 libv4l-dev:arm64 libgudev-1.0-dev:arm64 libgstreamer-plugins-base1.0-dev:arm64 libcairo2-dev:arm64 liborc-0.4-dev:arm64 libcaca-dev:arm64 libspeex-dev:arm64 libpng-dev:arm64 libshout3-dev:arm64 libjpeg-dev:arm64 libaa1-dev:arm64 libflac-dev:arm64 libdv4-dev:arm64 libdv4-dev:arm64 libxdamage-dev:arm64 libxext-dev:arm64 libxfixes-dev:arm64 libxv-dev:arm64 libgtk-3-dev:arm64 libwavpack-dev:arm64 libtag1-dev:arm64 libsoup2.4-dev:arm64 libpulse-dev:arm64 libbz2-dev:arm64 libjack-jackd2-dev:arm64 libvpx-dev:arm64 cdbs=0.4.150 gtk-doc-tools:arm64 libzvbi-dev:arm64 libxvidcore-dev:arm64 libxml2-dev:arm64 libx265-dev:arm64 libx11-dev:arm64 libwildmidi-dev:arm64 libwebrtc-audio-processing-dev:arm64 libwebp-dev:arm64 libvo-amrwbenc-dev:arm64 libvo-aacenc-dev:arm64 libssl-dev:arm64 libsrtp0-dev:arm64 libspandsp-dev:arm64 libsoundtouch-dev:arm64 libsndfile1-dev:arm64 librtmp-dev:arm64 librsvg2-dev:arm64 libpng-dev:arm64 liborc-0.4-dev:arm64 libopus-dev:arm64 libopenjp2-7-dev:arm64 libopenal-dev:arm64 libofa0-dev:arm64 libmpcdec-dev:arm64 libmodplug-dev:arm64 libmms-dev:arm64 libmjpegtools-dev:arm64 liblilv-dev:arm64 libkate-dev:arm64 libiptcdata0-dev:arm64 libgsm1-dev:arm64 libgnutls28-dev:arm64 libsbc-dev:arm64 libgme-dev:arm64 libglu1-mesa-dev:arm64 libglib2.0-dev:arm64 libgles2-mesa-dev:arm64 libgl1-mesa-dev:arm64 libfluidsynth-dev:arm64 libfaad-dev:arm64 libexif-dev:arm64 libexempi-dev:arm64 libegl1-mesa-dev:arm64 libdvdnav-dev:arm64 libde265-dev:arm64 libdca-dev:arm64 libcurl4-gnutls-dev:arm64 libchromaprint-dev:arm64 libcairo2-dev:arm64 libbs2b-dev:arm64 libass-dev:arm64 ladspa-sdk:arm64 libwayland-dev:arm64 -y )
#   xf86-video-armsorc
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends xserver-xorg-dev:arm64 -y )
RUN cp /usr/lib/pkgconfig/* /usr/lib/aarch64-linux-gnu/pkgconfig/
#   FFmpeg
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends frei0r-plugins-dev:arm64 flite1-dev:arm64 libzmq3-dev:arm64 ladspa-sdk:arm64 libass-dev:arm64 libbluray-dev:arm64 libbs2b-dev:arm64 libbz2-dev:arm64 libcaca-dev:arm64 libxvmc-dev:arm64 libcdio-paranoia-dev:arm64 libchromaprint-dev:arm64 libdc1394-22-dev:arm64 libdrm-dev:arm64 libfontconfig1-dev:arm64 libfreetype6-dev:arm64 libfribidi-dev:arm64 libgme-dev:arm64 libgsm1-dev:arm64 libiec61883-dev:arm64 libxvidcore-dev:arm64 libavc1394-dev:arm64 libjack-jackd2-dev:arm64 libleptonica-dev:arm64 liblzma-dev:arm64 libmp3lame-dev:arm64 libxcb-xfixes0-dev:arm64 libopenal-dev:arm64 libomxil-bellagio-dev:arm64 libopencore-amrnb-dev:arm64 libzvbi-dev:arm64 libxv-dev:arm64 libxcb-shm0-dev:arm64 libopencore-amrwb-dev:arm64 libopencv-imgproc-dev:arm64 libopenjp2-7-dev:arm64 libopenmpt-dev:arm64 libxml2-dev:arm64 libopus-dev:arm64 libpulse-dev:arm64 librubberband-dev:arm64 librsvg2-dev:arm64 libsctp-dev:arm64 libxcb-shape0-dev:arm64 libsdl2-dev:arm64 libshine-dev:arm64 libsnappy-dev:arm64 libsoxr-dev:arm64 libspeex-dev:arm64 libssh-gcrypt-dev:arm64 libtesseract-dev:arm64 libtheora-dev:arm64 libtwolame-dev:arm64 libva-dev:arm64 libvdpau-dev:arm64 libx265-dev:arm64 libvo-amrwbenc-dev:arm64 libvorbis-dev:arm64 libvpx-dev:arm64 libwavpack-dev:arm64 libwebp-dev:arm64 libx264-dev:arm64 doxygen=1.8.13-4+b1 cleancss=1.0.12-2 node-less=1.6.3~dfsg-2 -y )
#   Mpv
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libasound2-dev:arm64 libass-dev:arm64 libavcodec-dev:arm64 libavdevice-dev:arm64 libavfilter-dev:arm64 libavformat-dev:arm64 libavresample-dev:arm64 libavutil-dev:arm64 libbluray-dev:arm64 libcaca-dev:arm64 libcdio-paranoia-dev:arm64 libdvdnav-dev:arm64 libdvdread-dev:arm64 libegl1-mesa-dev:arm64 libgbm-dev:arm64 libgl1-mesa-dev:arm64 libjack-dev:arm64 libjpeg-dev:arm64 liblcms2-dev:arm64 liblua5.2-dev:arm64 libpulse-dev:arm64 librubberband-dev:arm64 libsdl2-dev:arm64 libsmbclient-dev:arm64 libsndio-dev:arm64 libswscale-dev:arm64 libuchardet-dev:arm64 libva-dev:arm64 libvdpau-dev:arm64 libwayland-dev:arm64 libx11-dev:arm64 libxinerama-dev:arm64 libxkbcommon-dev:arm64 libxrandr-dev:arm64 libxss-dev:arm64 libxv-dev:arm64 -y )
#  # yocto
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends gawk=1:4.1.4+dfsg-1 wget=1.18-5+deb9u3 git-core=1:2.11.0-3+deb9u7 diffstat=1.61-1+b1 unzip=6.0-21+deb9u2 texinfo=6.3.0.dfsg.1-1+b2 build-essential=12.3 chrpath=0.16-2+b1 socat=1.7.3.1-2+deb9u1 xterm=327-2+deb9u3 locales=2.24-11+deb9u4 -y )
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
 && echo 'LANG="en_US.UTF-8"' > /etc/default/locale \
 && dpkg-reconfigure --frontend=noninteractive locales \
 && update-locale LANG=en_US.UTF-8
COPY ./overlay/ /
RUN echo "Update Headers!"
#  RUN dpkg -i /packages/mpp/*.deb
RUN dpkg -i /packages/arm64/mpp/*.deb
RUN dpkg -i /packages/arm64/gstreamer/*.deb
RUN dpkg -i /packages/arm64/libmali/*.deb
RUN dpkg -i /packages/arm64/qt/*.deb
RUN find /packages/arm64/libdrm -name '*.deb' | sudo xargs -I{} dpkg -x {} /
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends -y -f )
#   switch to a no-root user
RUN useradd -c 'rk user' -m -d /home/rk -s /bin/bash rk
RUN sed -i -e '/\%sudo/ c \%sudo ALL=(ALL) NOPASSWD: ALL' /etc/sudoers
RUN usermod -a -G sudo rk
USER rk
# Please add your HEALTHCHECK here!!!
