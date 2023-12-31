#  Copyright (C) 2018 VyOS maintainers and contributors
#
#  This program is free software; you can redistribute it and/or modify
#  in order to easy exprort images built to "external" world
#  it under the terms of the GNU General Public License version 2 or later as
#  published by the Free Software Foundation.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#  Must be run with --privileged flag, recommended to run the container with a
#  volume mapped in order to easy export images
FROM debian:jessie
LABEL authors="VyOS Maintainers <maintainers@vyos.io>"
ENV DEBIAN_FRONTEND="noninteractive"
#  Standard shell should be bash not dash
RUN echo "dash dash/sh boolean false" | debconf-set-selections \
 && dpkg-reconfigure dash
RUN apt-get update \
 && apt-get install dialog apt-utils locales -y
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen
ENV LANG="en_US.utf8"
RUN apt-get update \
 && apt-get install vim git curl make sudo mc pbuilder devscripts squashfs-tools autoconf automake dpkg-dev syslinux genisoimage lsb-release fakechroot libtool libapt-pkg-dev parted kpartx qemu-system-x86 qemu-utils quilt python3-lxml python3-setuptools python3-nose python3-coverage python3-sphinx python3-pystache pkg-config debhelper jq -y
#
#  Setup Debian Jessie Backports repository
#
COPY no--check-valid-until /etc/apt/apt.conf.d/
RUN echo "deb http://archive.debian.org/debian/ jessie-backports main" > /etc/apt/sources.list.d/jessie-backports.list
RUN apt-get update \
 && apt-get install jessie-backports python3-git python3-pip gosu -y -t
#  Package needed for mdns-repeater
RUN apt-get update \
 && apt-get install jessie-backports dh-systemd -y -t
#
#  Remove Debian Jessie Backports repository
#
RUN rm -f /etc/apt/sources.list.d/jessie-backports.list /etc/apt/apt.conf.d/no--check-valid-until
#
#  Building libvyosconf requires a full configured OPAM/OCaml setup
#
RUN apt-get update \
 && apt-get install libffi-dev libpcre3-dev -y
RUN curl https://raw.githubusercontent.com/ocaml/opam/2.0.2/shell/install.sh --output /tmp/opam_install.sh \
 && sed -i 's/read BINDIR/BINDIR=""/' /tmp/opam_install.sh \
 && sh /tmp/opam_install.sh \
 && opam init --root=/opt/opam --comp=4.07.0 --disable-sandboxing
RUN eval $( opam env --root=/opt/opam --set-root ;) \
 && opam install -y oasis
RUN eval $( opam env --root=/opt/opam --set-root ;) \
 && opam install -y fileutils lwt lwt_ppx lwt_log ocplib-endian ounit pcre ppx_deriving_yojson sha toml xml-light batteries ocaml-protoc ctypes-foreign menhir
RUN eval $( opam env --root=/opt/opam --set-root ;) \
 && opam install -y ctypes
#  Build VyConf which is required to build libvyosconfig
RUN eval $( opam env --root=/opt/opam --set-root ;) \
 && opam pin add vyconf https://github.com/vyos/vyconf.git#51d79a3f -y
#  Build libvyosconfig
RUN eval $( opam env --root=/opt/opam --set-root ;) \
 && git clone https://github.com/vyos/libvyosconfig.git \
 && cd libvyosconfig \
 && git checkout 9a80a5d3 \
 && dpkg-buildpackage -uc -us -tc -b \
 && dpkg -i ../libvyosconfig0_*_amd64.deb
#  Packages needed for vyatta-cfg
RUN apt-get update \
 && apt-get install libglib2.0-dev libperl-dev libboost-filesystem-dev -y
#  Packages needed for vyatta-iproute
RUN apt-get update \
 && apt-get install iptables-dev libatm1-dev libcap-dev libdb-dev libelf-dev libselinux1-dev -y
#  Packages needed for vyatta-webgui
RUN apt-get update \
 && apt-get install libexpat1-dev subversion -y
#  Packages needed for pmacct
RUN apt-get update \
 && apt-get install libpcap-dev libpq-dev libmysqlclient-dev libgeoip-dev librabbitmq-dev libjansson-dev librdkafka-dev libnetfilter-log-dev -y
#  Pavkages needed for wireguard
RUN apt-get update \
 && apt-get install libmnl-dev -y
#  Packages needed for kernel
RUN apt-get update \
 && apt-get install kernel-package libncurses5-dev flex bison libelf-dev -y
#  Packages needed for vyos-accel-ppp
RUN apt-get update \
 && apt-get install cdbs cmake liblua5.1-dev -y
#  Prerequisites for building rtrlib
#  see http://docs.frrouting.org/projects/dev-guide/en/latest/building-frr-for-debian8.html
RUN apt-get update \
 && apt-get install doxygen libssh-dev libssl-dev -y
#  Build rtrlib release 0.6.3
RUN export RTRLIB_VERSION="0.6.3" \
 && wget -P /tmp https://github.com/rtrlib/rtrlib/archive/v${RTRLIB_VERSION}.tar.gz \
 && tar xf /tmp/v${RTRLIB_VERSION}.tar.gz -C /tmp \
 && cd /tmp/rtrlib-${RTRLIB_VERSION} \
 && dpkg-buildpackage -uc -us -tc -b \
 && dpkg -i ../librtr*_amd64.deb ../librtr*_all.deb
#
#  Setup VyOS Debian repository
#
COPY vyos-dev.key /tmp/vyos-dev.key
RUN apt-key add /tmp/vyos-dev.key
RUN echo "deb http://dev.packages.vyos.net/repositories/current/debian/ current main" > /etc/apt/sources.list.d/vyos.list
#  Packages needed to build frr itself
#  libyang-dev packages are hsoted on dev.packages.vyos.net see
#  https://github.com/FRRouting/frr/blob/master/doc/developer/building-libyang.rst
#  for more info
RUN apt-get update \
 && apt-get install libyang-dev libyang0.16 chrpath install-info libjson-c-dev libpython3-dev python3-dev python3-pytest texinfo -y
#
#  Cleanup VyOS Debian Repository
#
RUN rm -f /etc/apt/sources.list.d/vyos.list
#  Packages needed for conntrack-tools
RUN apt-get update \
 && apt-get install libnetfilter-conntrack-dev libnetfilter-cthelper0-dev libnetfilter-cttimeout-dev libnetfilter-queue-dev -y
#  Packages needed for hvinfo
RUN apt-get update \
 && apt-get install gnat gprbuild -y
#  Packages needed for vyos-1x
RUN apt-get update \
 && apt-get install whois -y
#  Packages needed for vyos-xe-guest-utilities
RUN apt-get update \
 && apt-get install golang -y
#  Packages needed for ipaddrcheck
RUN apt-get update \
 && apt-get install check -y
#  As there is no Debian Jessie/Stretch package for libcidr available but this
#  is required for ipaddrcheck we have to build it from source
RUN git clone https://github.com/wikimedia/analytics-libcidr.git \
 && cd analytics-libcidr \
 && git checkout 026c611d90a1 \
 && dpkg-buildpackage -uc -us -tc -b \
 && dpkg -i ../libcidr*.deb
#  Packages needed for lldpd
RUN apt-get update \
 && apt-get install libbsd-dev libevent-dev -y
#  Packages needed for vyatta-quagga
RUN apt-get update \
 && apt-get install libpam-dev libcap-dev libsnmp-dev gawk -y
#  Packages needed for vyos-strongswan
RUN apt-get update \
 && apt-get install libkrb5-dev libssl-dev libxml2-dev systemd libcurl4-openssl-dev libgcrypt20-dev libgmp3-dev libldap2-dev libsqlite3-dev dh-apparmor gperf libsystemd-dev python3-stdeb python-setuptools -y
#  Packages needed for vyos-opennhrp
RUN apt-get update \
 && apt-get install libc-ares-dev -y
#  Packages needed for ddclient
RUN apt-get update \
 && apt-get install xmlto -y
#  Packages needed for keepalived
RUN apt-get update \
 && apt-get install libnl-3-200 libnl-3-dev libnl-nf-3-200 libnl-nf-3-dev libipset-dev libnl-genl-3-200 libnl-genl-3-dev libpopt-dev -y
#  Packages needed for net-snmp
RUN apt-get update \
 && apt-get install python-all python2.7-dev libmysqld-dev -y
#  Update live-build
RUN echo 'deb http://ftp.debian.org/debian stretch main' | tee -a /etc/apt/sources.list.d/stretch.list \
 && apt-get update \
 && apt-get install stretch live-build -y -t \
 && rm -f /etc/apt/sources.list.d/stretch.list \
 && apt-get update \
 && rm -rf /var/lib/apt/lists/*
#  Install packer
RUN export LATEST="$( curl -s https://checkpoint-api.hashicorp.com/v1/check/packer | jq -r -M '.current_version' ;)" ; echo "url https://releases.hashicorp.com/packer/"$LATEST"/packer_"$LATEST"_linux_amd64.zip" | curl -K- | gzip -d > /usr/bin/packer \
 && chmod +x /usr/bin/packer
#  Allow password-less 'sudo' for all users in group 'sudo'
RUN sed "s/^%sudo.*/%sudo\tALL=(ALL) NOPASSWD:ALL/g" -i /etc/sudoers \
 && chmod a+s /usr/sbin/useradd /usr/sbin/groupadd /usr/sbin/gosu /usr/sbin/usermod
#  Ensure sure all users have access to our OCAM installation
RUN echo "$( opam env --root=/opt/opam --set-root ;)" >> /etc/skel/.bashrc
#  Cleanup
RUN rm -rf /tmp/*
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
