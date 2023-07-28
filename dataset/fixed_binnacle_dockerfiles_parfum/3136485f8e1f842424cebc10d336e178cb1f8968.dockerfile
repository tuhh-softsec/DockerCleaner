# FROM ubuntu:yakkety
FROM ubuntu
MAINTAINER Duzy Chan <code@duzy.info>
#  RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
#    libgl1-mesa-dri \
#    net-tools \
#    sudo \
#    weston
#   
# ###############################################################################
#  The following additional packages will be installed:
#     dbus fontconfig fontconfig-config fonts-dejavu-core libapparmor1 libbsd0
#    libcairo2 libcolord2 libdatrie1 libdbus-1-3 libdrm-amdgpu1 libdrm-intel1
#    libdrm-nouveau2 libdrm-radeon1 libdrm2 libedit2 libegl1-mesa libelf1
#    libevdev2 libexpat1 libffi6 libfontconfig1 libfreetype6 libgbm1
#    libglapi-mesa libgles2-mesa libglib2.0-0 libglib2.0-data libgraphite2-3
#    libgudev-1.0-0 libharfbuzz0b libicu57 libinput-bin libinput10 libjpeg-turbo8
#    libjpeg8 liblcms2-2 libllvm3.8 libmtdev1 libpango-1.0-0 libpangocairo-1.0-0
#    libpangoft2-1.0-0 libpciaccess0 libpixman-1-0 libpng16-16 libthai-data
#    libthai0 libtxc-dxtn-s2tc0 libwacom-bin libwacom-common libwacom2
#    libwayland-client0 libwayland-cursor0 libwayland-egl1-mesa
#    libwayland-server0 libx11-6 libx11-data libx11-xcb1 libxau6
#    libxcb-composite0 libxcb-dri2-0 libxcb-dri3-0 libxcb-present0 libxcb-render0
#    libxcb-shm0 libxcb-sync1 libxcb-xfixes0 libxcb-xkb1 libxcb1 libxcursor1
#    libxdmcp6 libxext6 libxfixes3 libxkbcommon0 libxml2 libxrender1
#    libxshmfence1 sgml-base shared-mime-info ucf xdg-user-dirs xkb-data xml-core
#  Suggested packages:
#    default-dbus-session-bus | dbus-session-bus colord liblcms2-utils pciutils
#    sgml-base-doc debhelper
#  The following NEW packages will be installed:
#    dbus fontconfig fontconfig-config fonts-dejavu-core libapparmor1 libbsd0
#    libcairo2 libcolord2 libdatrie1 libdbus-1-3 libdrm-amdgpu1 libdrm-intel1
#    libdrm-nouveau2 libdrm-radeon1 libdrm2 libedit2 libegl1-mesa libelf1
#    libevdev2 libexpat1 libffi6 libfontconfig1 libfreetype6 libgbm1
#    libgl1-mesa-dri libglapi-mesa libgles2-mesa libglib2.0-0 libglib2.0-data
#    libgraphite2-3 libgudev-1.0-0 libharfbuzz0b libicu57 libinput-bin libinput10
#    libjpeg-turbo8 libjpeg8 liblcms2-2 libllvm3.8 libmtdev1 libpango-1.0-0
#    libpangocairo-1.0-0 libpangoft2-1.0-0 libpciaccess0 libpixman-1-0
#    libpng16-16 libthai-data libthai0 libtxc-dxtn-s2tc0 libwacom-bin
#    libwacom-common libwacom2 libwayland-client0 libwayland-cursor0
#    libwayland-egl1-mesa libwayland-server0 libx11-6 libx11-data libx11-xcb1
#    libxau6 libxcb-composite0 libxcb-dri2-0 libxcb-dri3-0 libxcb-present0
#    libxcb-render0 libxcb-shm0 libxcb-sync1 libxcb-xfixes0 libxcb-xkb1 libxcb1
#    libxcursor1 libxdmcp6 libxext6 libxfixes3 libxkbcommon0 libxml2 libxrender1
#    libxshmfence1 net-tools sgml-base shared-mime-info sudo ucf weston
#    xdg-user-dirs xkb-data xml-core
RUN cp /etc/apt/sources.list /etc/apt/sources.list.backup
COPY sources.list /etc/apt/sources.list
# # net-tools for ping
RUN apt-get update \
 && apt-cache show libgl1-mesa-dri net-tools sudo weston
# # Install `sudo'
RUN apt-get install --no-install-recommends libaudit1 --fix-missing -y
RUN apt-get install --no-install-recommends libc6 --fix-missing -y
RUN apt-get install --no-install-recommends libpam0g --fix-missing -y
RUN apt-get install --no-install-recommends libselinux1 --fix-missing -y
RUN apt-get install --no-install-recommends libpam-modules --fix-missing -y
RUN apt-get install --no-install-recommends sudo --fix-missing -y
# # Install `net-tools' (Depends: libc6 (>= 2.14), libselinux1 (>= 1.32))
RUN apt-get install --no-install-recommends net-tools --fix-missing -y
# # Install `libgl1-mesa-dri'
RUN apt-get install --no-install-recommends libdrm-amdgpu1 --fix-missing -y
RUN apt-get install --no-install-recommends libdrm-intel1 --fix-missing -y
RUN apt-get install --no-install-recommends libdrm-nouveau2 --fix-missing -y
# RUN apt-get install --fix-missing -y libdrm-radeon1
RUN apt-get install --no-install-recommends libdrm2 --fix-missing -y
RUN apt-get install --no-install-recommends libelf1 --fix-missing -y
RUN apt-get install --no-install-recommends libexpat1 --fix-missing -y
RUN apt-get install --no-install-recommends libgcc1 --fix-missing -y
RUN apt-get install --no-install-recommends libgcrypt20 --fix-missing -y
RUN apt-get install --no-install-recommends libllvm3.8 --fix-missing -y
RUN apt-get install --no-install-recommends libstdc++6 --fix-missing -y
RUN apt-get install --no-install-recommends libgl1-mesa-dri --fix-missing -y
# # Install `weston'
RUN apt-get install --no-install-recommends adduser --fix-missing -y
RUN apt-get install --no-install-recommends libegl1-mesa --fix-missing -y
RUN apt-get install --no-install-recommends libwayland-egl1-mesa --fix-missing -y
RUN apt-get install --no-install-recommends libgles2-mesa --fix-missing -y
RUN apt-get install --no-install-recommends libc6 --fix-missing -y
RUN apt-get install --no-install-recommends libcairo2 --fix-missing -y
RUN apt-get install --no-install-recommends libcolord2 --fix-missing -y
RUN apt-get install --no-install-recommends libdbus-1-3 --fix-missing -y
RUN apt-get install --no-install-recommends libdrm2 --fix-missing -y
RUN apt-get install --no-install-recommends libgbm1 --fix-missing -y
RUN apt-get install --no-install-recommends libglib2.0-0 --fix-missing -y
RUN apt-get install --no-install-recommends libinput10 --fix-missing -y
RUN apt-get install --no-install-recommends libjpeg8 --fix-missing -y
RUN apt-get install --no-install-recommends liblcms2-2 --fix-missing -y
RUN apt-get install --no-install-recommends libpam0g --fix-missing -y
RUN apt-get install --no-install-recommends libpango-1.0-0 --fix-missing -y
RUN apt-get install --no-install-recommends libpangocairo-1.0-0 --fix-missing -y
RUN apt-get install --no-install-recommends libpixman-1-0 --fix-missing -y
RUN apt-get install --no-install-recommends libpng16-16 --fix-missing -y
RUN apt-get install --no-install-recommends libsystemd0 --fix-missing -y
RUN apt-get install --no-install-recommends libudev1 --fix-missing -y
RUN apt-get install --no-install-recommends libwayland-client0 --fix-missing -y
RUN apt-get install --no-install-recommends libwayland-cursor0 --fix-missing -y
RUN apt-get install --no-install-recommends libwayland-server0 --fix-missing -y
RUN apt-get install --no-install-recommends libxcb-composite0 --fix-missing -y
RUN apt-get install --no-install-recommends libxcb-render0 --fix-missing -y
RUN apt-get install --no-install-recommends libxcb-shm0 --fix-missing -y
RUN apt-get install --no-install-recommends libxcb-xfixes0 --fix-missing -y
RUN apt-get install --no-install-recommends libxcb-xkb1 --fix-missing -y
RUN apt-get install --no-install-recommends libxcb1 --fix-missing -y
RUN apt-get install --no-install-recommends libxcursor1 --fix-missing -y
RUN apt-get install --no-install-recommends libxkbcommon0 --fix-missing -y
RUN apt-get install --no-install-recommends weston --fix-missing -y
RUN useradd -m -s /bin/bash user
RUN echo "user ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/user
ENV DISPLAY=":0"
ENV XDG_RUNTIME_DIR="/var/lib/wayland"
RUN mkdir -p /var/lib/wayland \
 && chmod 0700 /var/lib/wayland
USER root
WORKDIR /root
CMD weston-launch
