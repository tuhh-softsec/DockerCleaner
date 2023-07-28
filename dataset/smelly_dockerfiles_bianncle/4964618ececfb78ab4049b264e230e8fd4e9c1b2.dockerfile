# ###########################################################
#  Copyright (c) 2015 Jonathan Yantis
#  Released under the MIT license
# ###########################################################
#  ├─yantis/archlinux-tiny
#     ├─yantis/archlinux-small
#        ├─yantis/archlinux-mac-installer
#  Dockerhub can not handle uploading layers above 500MB
#  The layers time out on upload. Also, if a single RUN command
#  takes to long to run Dockerhub will fail to build it so
#  breaking them down into smaller chunks even if more redundant.
#  Dockerhub also can not handle lots of pacman signature checks
#  As it times out at exactly two hours.
FROM yantis/archlinux-small
MAINTAINER Jonathan Yantis <yantis@yantis.net>
# ##############################################################################
#  Install permanent additions to the container.
# ##############################################################################
#  Don't update to avoid breaking things
RUN pacman --noconfirm -Syy \
 && bash -c "echo 'curl -L $1 | sh' > /bin/run-remote-script" \
 && chmod +x /bin/run-remote-script \
 && sed -i "s/\[core\]/\[core\]\nSigLevel = Never/" /etc/pacman.conf \
 && sed -i "s/\[extra\]/\[extra\]\nSigLevel = Never/" /etc/pacman.conf \
 && sed -i "s/\[community\]/\[community\]\nSigLevel = Never/" /etc/pacman.conf \
 && pacman --noconfirm --needed -S perl texinfo rsync squashfs-tools \
 && localepurge \
 && rm -r /usr/share/info/* \
 && rm -r /usr/share/man/* \
 && find /. -name "*~" -type f -delete \
 && bash -c "echo 'y' | pacman -Scc >/dev/null 2>&1" \
 && paccache -rk0 > /dev/null 2>&1 \
 && pacman-optimize \
 && rm -r /var/lib/pacman/sync/*
# ##############################################################################
#  Build and Cache NVIDA Drivers
#  349xx series drivers are problematic on the Macbooks so using 346xx
# ##############################################################################
RUN pacman --noconfirm -Sy binutils gcc make autoconf fakeroot \
 && mkdir -p /var/cache/pacman/custom \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/nvidia-346xx-dkms.tar.gz \
 && tar -xvf /tmp/nvidia-346xx-dkms.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/nvidia-346xx-dkms \
 && runuser -l docker -c "(cd /tmp/nvidia-346xx-dkms \
 && makepkg -scd --noconfirm)" \
 && mv /tmp/nvidia-346xx-dkms/*.xz /var/cache/pacman/custom/ \
 && rm -r /tmp/* \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/nvidia-346xx-utils.tar.gz \
 && tar -xvf /tmp/nvidia-346xx-utils.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/nvidia-346xx-utils \
 && runuser -l docker -c "(cd /tmp/nvidia-346xx-utils \
 && makepkg -scd --noconfirm)" \
 && mv /tmp/nvidia-346xx-utils/*.xz /var/cache/pacman/custom/ \
 && rm -r /tmp/* \
 && pacman --noconfirm -Rs binutils gcc make autoconf fakeroot \
 && rm -r /usr/share/info/* \
 && rm -r /usr/share/man/* \
 && bash -c "echo 'y' | pacman -Scc >/dev/null 2>&1" \
 && paccache -rk0 > /dev/null 2>&1 \
 && pacman-optimize \
 && rm -r /var/lib/pacman/sync/*
# ##############################################################################
#  Build any packages we may need to install.
# ##############################################################################
RUN pacman --noconfirm --needed -Sy base-devel \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/xf86-input-mtrack-git.tar.gz \
 && tar -xvf /tmp/xf86-input-mtrack-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/xf86-input-mtrack-git \
 && runuser -l docker -c "(cd /tmp/xf86-input-mtrack-git \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/xf86-input-mtrack-git/*.xz /var/cache/pacman/custom/ \
 && pacman --noconfirm -S b43-fwcutter \
 && curl -LO http://downloads.openwrt.org/sources/broadcom-wl-4.178.10.4.tar.bz2 \
 && tar xjf broadcom-wl-4.178.10.4.tar.bz2 \
 && mkdir /firmware \
 && b43-fwcutter -w /firmware broadcom-wl-4.178.10.4/linux/wl_apsta.o \
 && rm -r broadcom-wl* \
 && pacman --noconfirm -Rs b43-fwcutter \
 && mkdir -p /var/cache/pacman/general \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/thermald.tar.gz \
 && tar -xvf /tmp/thermald.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/thermald \
 && runuser -l docker -c "(cd /tmp/thermald \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/thermald/*.xz /var/cache/pacman/general/ \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/mbpfan-git.tar.gz \
 && tar -xvf /tmp/mbpfan-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/mbpfan-git \
 && runuser -l docker -c "(cd /tmp/mbpfan-git \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/mbpfan-git/*.xz /var/cache/pacman/custom/ \
 && rm -r /tmp/* \
 && pacman --noconfirm -Sw --cachedir /var/cache/pacman/custom broadcom-wl-dkms xf86-video-intel \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/ttf-literation-mono-powerline-git.tar.gz \
 && tar -xvf /tmp/ttf-literation-mono-powerline-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/ttf-literation-mono-powerline-git \
 && runuser -l docker -c "(cd /tmp/ttf-literation-mono-powerline-git \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/ttf-literation-mono-powerline-git/*.xz /var/cache/pacman/general/ \
 && rm -r /tmp/* \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/oh-my-zsh-git.tar.gz \
 && tar -xvf /tmp/oh-my-zsh-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/oh-my-zsh-git \
 && runuser -l docker -c "(cd /tmp/oh-my-zsh-git \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/oh-my-zsh-git/*.xz /var/cache/pacman/general/ \
 && rm -r /tmp/* \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/bullet-train-oh-my-zsh-theme-git.tar.gz \
 && tar -xvf /tmp/bullet-train-oh-my-zsh-theme-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/bullet-train-oh-my-zsh-theme-git \
 && runuser -l docker -c "(cd /tmp/bullet-train-oh-my-zsh-theme-git \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/bullet-train-oh-my-zsh-theme-git/*.xz /var/cache/pacman/general/ \
 && rm -r /tmp/* \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/fasd-git.tar.gz \
 && tar -xvf /tmp/fasd-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/fasd-git \
 && runuser -l docker -c "(cd /tmp/fasd-git \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/fasd-git/*.xz /var/cache/pacman/general/ \
 && rm -r /tmp/* \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/zsh-dwim-git.tar.gz \
 && tar -xvf /tmp/zsh-dwim-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/zsh-dwim-git \
 && runuser -l docker -c "(cd /tmp/zsh-dwim-git \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/zsh-dwim-git/*.xz /var/cache/pacman/general/ \
 && rm -r /tmp/* \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/zaw-git.tar.gz \
 && tar -xvf /tmp/zaw-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/zaw-git \
 && runuser -l docker -c "(cd /tmp/zaw-git \
 && makepkg -sc --noconfirm)" \
 && mv /tmp/zaw-git/*.xz /var/cache/pacman/general/ \
 && rm -r /tmp/* \
 && wget -P /tmp https://aur.archlinux.org/cgit/aur.git/snapshot/sddm-archlinux-theme-git.tar.gz \
 && tar -xvf /tmp/sddm-archlinux-theme-git.tar.gz -C /tmp \
 && chown -R docker:docker /tmp/sddm-archlinux-theme-git \
 && runuser -l docker -c "(cd /tmp/sddm-archlinux-theme-git \
 && makepkg -sdc --noconfirm)" \
 && mv /tmp/sddm-archlinux-theme-git/*.xz /var/cache/pacman/general/ \
 && rm -r /tmp/* \
 && pacman --noconfirm -Rs dbus-glib dri2proto dri3proto fontsproto glproto libxml2 libxss mesa pixman presentproto randrproto renderproto flex resourceproto videoproto xf86driproto xineramaproto xorg-util-macros gcc binutils guile make libxfont xorg-bdftopcf xorg-font-utils fontconfig libtool m4 git inputproto dbus systemd package-query bison autoconf freetype2 harfbuzz graphite libpng xorg-server-devel gettext \
 && localepurge \
 && rm -r /usr/share/info/* \
 && rm -r /usr/share/man/* \
 && rm -r /usr/share/doc/* \
 && find /. -name "*~" -type f -delete \
 && bash -c "echo 'y' | pacman -Scc >/dev/null 2>&1" \
 && paccache -rk0 > /dev/null 2>&1 \
 && pacman-optimize \
 && rm -r /var/lib/pacman/sync/*
# ##############################################################################
#  Cache packages that have happened since the last airootfs image
#  Purposely add another layer here to break up the size.
#  Since we kept the one above clean it should add minimal overhead.
# ##############################################################################
RUN pacman --noconfirm -Syw --cachedir /var/cache/pacman/general btrfs-progs ca-certificates-utils ca-certificates dnsmasq glib2 glibc gnupg gnutls grml-zsh-config gssproxy lftp libinput libssh2 libsystemd libtasn1 lz4 man-pages nano nettle ntp partclone openconnect systemd-sysvcompat tcpdump testdisk \
 && rm -r /var/lib/pacman/sync/*
# ##############################################################################
#  Just download these since we don't actually need them for the docker container.
#  Make sure none of these are in the list above.
#  Broke into two layers for better download speeds on the container.
# ##############################################################################
RUN pacman --noconfirm -Syw --cachedir /var/cache/pacman/general base-devel acpi alsa-utils arch-install-scripts aria2 c-ares cpupower ctags dkms feh git haveged htop gnome-keyring gnome-terminal google-chrome linux linux-headers hfsprogs intel-ucode imagemagick lm_sensors mlocate networkmanager network-manager-applet pavucontrol package-query pciutils powertop pulseaudio-alsa pulseaudio \
 && rm -r /var/lib/pacman/sync/*
RUN pacman --noconfirm -Syw --cachedir /var/cache/pacman/general libical solid gamin bluez plasma konsole python-dateutil python-docutils python-pyasn1 python-rsa python-setuptools python-six reflector rsync sqlite sddm systemd terminus-font tree tmux vim xfce4 xfce4-whiskermenu-plugin xorg-server xorg-server-utils xorg-xinit xorg-xev yajl yaourt zsh-syntax-highlighting \
 && rm -r /var/lib/pacman/sync/*
#  # Download and cache Infinality Fonts
#  RUN bash -c "echo \"[infinality-bundle-fonts]\" >> /etc/pacman.conf" && \
#      bash -c "echo \"Server = http://bohoomil.com/repo/fonts \" >>/etc/pacman.conf" && \
#      bash -c "echo \"SigLevel = Never\" >> /etc/pacman.conf" && \
#      bash -c "echo \"[infinality-bundle]\" >> /etc/pacman.conf" && \
#      bash -c "echo \"Server = http://bohoomil.com/repo/x86_64 \" >>/etc/pacman.conf" && \
#      bash -c "echo \"SigLevel = Never\" >> /etc/pacman.conf" && \
#      rm /var/cache/pacman/general/ttf-dejavu-* && \
#      rm /var/cache/pacman/general/freetype2-* && \
#      rm /var/cache/pacman/general/fontconfig-* && \
#      rm /var/cache/pacman/general/cairo-* && \
#      rm /var/cache/pacman/general/cantarell-fonts-* && \
#      pacman --noconfirm -Syw --cachedir /var/cache/pacman/general \
#              infinality-bundle \
#              ibfonts-meta-base \
#              otf-cantarell-ib \
#              ibfonts-meta-extended-lt \
#              otf-oswald-ib \
#              otf-quintessential-ib \
#              otf-tex-gyre-ib \
#              t1-cursor-ib \
#              t1-urw-fonts-ib \
#              ttf-caladea-ib \
#              ttf-cantoraone-ib \
#              ttf-carlito-ib \
#              ttf-ddc-uchen-ib \
#              ttf-droid-ib \
#              ttf-gelasio-ib \
#              ttf-lohit-odia-ib \
#              ttf-lohit-punjabi-ib \
#              ttf-merriweather-ib \
#              ttf-merriweather-sans-ib \
#              # ttf-noto-serif-multilang-ib \
#              ttf-opensans-ib \
#              ttf-signika-family-ib \
#              ttf-ubuntu-font-family-ib && \
#      # Clean up pacman
#      rm -r /var/lib/pacman/sync/*
CMD /bin/zsh
