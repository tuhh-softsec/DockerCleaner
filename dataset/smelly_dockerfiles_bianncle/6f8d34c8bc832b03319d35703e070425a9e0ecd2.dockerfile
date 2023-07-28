FROM scratch
ADD vagga-0.8.1.tar.xz /
ENV PATH="/vagga:/bin"
RUN ["/vagga/busybox", "mkdir", "/bin"]
RUN ["/vagga/busybox", "ln", "-vsfn", "/vagga/busybox", "/bin/sh"]
RUN /vagga/busybox ln -vsfn /vagga/busybox /bin/[ \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/[[ \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/acpid \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/add-shell \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/addgroup \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/adduser \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/adjtimex \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/arp \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/arping \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ash \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/awk \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/base64 \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/basename \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/bbconfig \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/beep \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/blkdiscard \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/blkid \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/blockdev \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/brctl \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/bunzip2 \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/bzcat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/bzip2 \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/cal \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/cat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/catv \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/chgrp \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/chmod \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/chown \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/chpasswd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/chroot \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/chvt \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/cksum \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/clear \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/cmp \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/comm \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/conspy \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/cp \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/cpio \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/crond \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/crontab \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/cryptpw \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/cut \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/date \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dc \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/deallocvt \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/delgroup \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/deluser \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/depmod \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/df \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/diff \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dirname \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dmesg \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dnsd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dnsdomainname \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dos2unix \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/du \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dumpkmap \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/dumpleases \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/echo \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ed \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/egrep \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/eject \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/env \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ether-wake \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/expand \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/expr \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fakeidentd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/false \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fatattr \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fbset \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fbsplash \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fdflush \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fdformat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fdisk \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fgrep \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/find \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/findfs \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/flock \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fold \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/free \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fsck \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fstrim \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fsync \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ftpd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ftpget \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ftpput \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/fuser \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/getopt \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/getty \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/grep \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/groups \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/gunzip \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/gzip \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/halt \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/hd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/hdparm \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/head \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/hexdump \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/hostid \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/hostname \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/httpd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/hwclock \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/id \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ifconfig \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ifdown \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ifenslave \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ifup \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/inetd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/init \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/inotifyd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/insmod \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/install \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ionice \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/iostat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ip \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ipaddr \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ipcalc \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ipcrm \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ipcs \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/iplink \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/iproute \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/iprule \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/iptunnel \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/kbd_mode \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/kill \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/killall \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/killall5 \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/klogd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/less \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ln \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/loadfont \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/loadkmap \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/logger \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/login \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/logread \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/losetup \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ls \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/lsmod \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/lsof \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/lspci \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/lsusb \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/lzcat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/lzma \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/lzop \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/lzopcat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/makemime \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/md5sum \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mdev \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mesg \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/microcom \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mkdir \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mkdosfs \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mkfifo \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mkfs.vfat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mknod \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mkpasswd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mkswap \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mktemp \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/modinfo \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/modprobe \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/more \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mount \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mountpoint \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mpstat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/mv \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nameif \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nanddump \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nandwrite \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nbd-client \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nc \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/netstat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nice \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nmeter \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nohup \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nologin \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nsenter \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/nslookup \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ntpd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/od \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/openvt \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/passwd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/patch \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pgrep \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pidof \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ping \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ping6 \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pipe_progress \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pkill \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pmap \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/poweroff \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/powertop \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/printenv \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/printf \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ps \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pscan \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pstree \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pwd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/pwdx \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/raidautorun \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/rdate \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/rdev \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/readahead \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/readlink \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/readprofile \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/realpath \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/reboot \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/reformime \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/remove-shell \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/renice \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/reset \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/resize \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/rev \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/rfkill \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/rm \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/rmdir \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/rmmod \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/route \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/run-parts \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sed \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sendmail \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/seq \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/setconsole \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/setfont \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/setkeycodes \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/setlogcons \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/setserial \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/setsid \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sh \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sha1sum \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sha256sum \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sha3sum \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sha512sum \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/showkey \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/shuf \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/slattach \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sleep \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/smemcap \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sort \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/split \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/stat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/strings \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/stty \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/su \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sum \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/swapoff \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/swapon \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/switch_root \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sync \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/sysctl \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/syslogd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/tac \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/tail \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/tar \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/tee \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/telnet \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/test \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/tftp \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/time \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/timeout \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/top \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/touch \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/tr \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/traceroute \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/traceroute6 \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/true \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/truncate \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/tty \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/ttysize \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/tunctl \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/udhcpc \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/udhcpc6 \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/udhcpd \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/umount \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/uname \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/unexpand \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/uniq \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/unix2dos \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/unlink \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/unlzma \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/unlzop \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/unshare \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/unxz \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/unzip \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/uptime \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/usleep \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/uudecode \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/uuencode \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/vconfig \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/vi \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/vlock \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/volname \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/watch \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/watchdog \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/wc \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/wget \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/which \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/whoami \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/whois \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/xargs \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/xzcat \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/yes \
 && /vagga/busybox ln -vsfn /vagga/busybox /bin/zcat \
 && true
