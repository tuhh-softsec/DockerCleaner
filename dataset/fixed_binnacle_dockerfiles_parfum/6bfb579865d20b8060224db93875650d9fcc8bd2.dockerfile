# # -*- docker-image-name: "scaleway/debian:jessie" -*-
ARG MULTIARCH_ARCH
FROM multiarch/debian-debootstrap:${MULTIARCH_ARCH}-jessie
MAINTAINER Scaleway <opensource@scaleway.com> (@scaleway)
#  Environment
ENV DEBIAN_FRONTEND="noninteractive" \
    SCW_BASE_IMAGE="scaleway/debian:jessie"
#  Configure aptitude
#  Note that, `${ARCH}` is set in the multiarch/debian-debootstrap image
COPY ./overlay/etc/apt/ ./overlay-${ARCH}*/etc/apt/ /etc/apt/
#  Adding and calling builder-enter
COPY ./overlay-base/usr/local/sbin/scw-builder-enter /usr/local/sbin/
RUN /bin/sh -xe /usr/local/sbin/scw-builder-enter
#  Install packages
RUN apt-get update -q \
 && apt-get -y -qq upgrade \
 && apt-get install --no-install-recommends bash-completion bc bootlogd ca-certificates cron curl dbus dstat ethstatus file fio haveged htop ifupdown ioping iotop iperf iptables iputils-ping isc-dhcp-client less libnss-myhostname locales locate lsb-release lsof make man-db mg module-init-tools mosh nano net-tools netcat ntp ntpdate python-apt python-yaml rsync rsyslog screen shunit2 socat ssh sudo sysstat systemd-sysv tcpdump tmux uuid-runtime vim wget whiptail unattended-upgrades linux-image-"$( dpkg --print-architecture | sed 's/armhf/armmp/' ;)" grub-efi-$( dpkg --print-architecture | sed 's/armhf/arm/' ;) -y -qq \
 && apt-get clean
#  Default target
RUN systemctl set-default multi-user
#  Enable update-motd.d support
RUN rm -f /etc/motd \
 && ln -s /var/run/motd /etc/motd
#  Patch rootfs
COPY ./overlay/ ./overlay-base/ ./overlay-${ARCH}*/ /
#  Configure locales
RUN locale-gen
#  Configure Systemd
RUN systemctl preset --preset-mode=full $( cat /etc/systemd/system-preset/*scw*.preset | cut -d' ' -f2 | tr '\n' ' ' ;)
RUN systemctl disable systemd-modules-load.service systemd-update-utmp-runlevel proc-sys-fs-binfmt_misc.automount kmod-static-nodes.service
RUN ln -s /etc/systemd/system/networking.service /etc/systemd/system/multi-user.target.wants/networking.service
RUN rm -f /sbin/init \
 && ln -sf ../lib/systemd/systemd /sbin/init
#  Fix permissions
RUN chmod 755 /etc/default
#  Clean rootfs from image-builder
RUN /usr/local/sbin/scw-builder-leave
