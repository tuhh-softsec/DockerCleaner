FROM olbat/sak AS base
FROM scratch
ARG MAINTAINER
MAINTAINER $MAINTAINER
COPY --from=base /bin/bash /bin/bzip2 /bin/cat /bin/chgrp /bin/chmod /bin/chown /bin/cp /bin/cpio /bin/date /bin/dd /bin/df /bin/dir /bin/echo /bin/false /bin/grep /bin/gzexe /bin/gzip /bin/ln /bin/login /bin/ls /bin/mkdir /bin/mknod /bin/mktemp /bin/mv /bin/nc /bin/ping /bin/ping6 /bin/pwd /bin/readlink /bin/rm /bin/rmdir /bin/sed /bin/sh /bin/sleep /bin/stty /bin/sync /bin/tailf /bin/tar /bin/tempfile /bin/touch /bin/true /bin/umount /bin/uname /bin/vdir /bin/which /usr/bin/[ /usr/bin/addr2line /usr/bin/ar /usr/bin/arch /usr/bin/awk /usr/bin/b2sum /usr/bin/base32 /usr/bin/base64 /usr/bin/basename /usr/bin/catchsegv /usr/bin/c++filt /usr/bin/chattr /usr/bin/chcon /usr/bin/cksum /usr/bin/clear /usr/bin/cmp /usr/bin/comm /usr/bin/c_rehash /usr/bin/csplit /usr/bin/curl /usr/bin/cut /usr/bin/diff /usr/bin/diff3 /usr/bin/dircolors /usr/bin/dirname /usr/bin/drill /usr/bin/du /usr/bin/elfedit /usr/bin/env /usr/bin/expand /usr/bin/expiry /usr/bin/expr /usr/bin/factor /usr/bin/file /usr/bin/find /usr/bin/fmt /usr/bin/fold /usr/bin/gprof /usr/bin/groups /usr/bin/head /usr/bin/hostid /usr/bin/iconv /usr/bin/id /usr/bin/install /usr/bin/join /usr/bin/ldd /usr/bin/line /usr/bin/link /usr/bin/locale /usr/bin/logname /usr/bin/lzip /usr/bin/md5sum /usr/bin/mkfifo /usr/bin/nice /usr/bin/nl /usr/bin/nm /usr/bin/nohup /usr/bin/nproc /usr/bin/numfmt /usr/bin/objcopy /usr/bin/objdump /usr/bin/od /usr/bin/openssl /usr/bin/paste /usr/bin/patch /usr/bin/pathchk /usr/bin/pinky /usr/bin/pldd /usr/bin/pr /usr/bin/printenv /usr/bin/printf /usr/bin/ptx /usr/bin/ranlib /usr/bin/readelf /usr/bin/realpath /usr/bin/reset /usr/bin/runcon /usr/bin/script /usr/bin/scriptreplay /usr/bin/sdiff /usr/bin/seq /usr/bin/sfill /usr/bin/sg /usr/bin/sha1sum /usr/bin/sha224sum /usr/bin/sha256sum /usr/bin/sha384sum /usr/bin/sha512sum /usr/bin/shred /usr/bin/shuf /usr/bin/size /usr/bin/sort /usr/bin/split /usr/bin/srm /usr/bin/stat /usr/bin/stdbuf /usr/bin/strings /usr/bin/strip /usr/bin/sum /usr/bin/tac /usr/bin/tail /usr/bin/tee /usr/bin/test /usr/bin/timeout /usr/bin/touch /usr/bin/tr /usr/bin/traceroute /usr/bin/truncate /usr/bin/tsort /usr/bin/tty /usr/bin/unexpand /usr/bin/uniq /usr/bin/unlink /usr/bin/users /usr/bin/vi /usr/bin/wc /usr/bin/whereis /usr/bin/which /usr/bin/who /usr/bin/whoami /usr/bin/xargs /usr/bin/xxd /usr/bin/xz /usr/bin/yes /bin/
COPY --from=base /lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2
COPY --from=base /lib/x86_64-linux-gnu/libnss_files.so.2 /lib/
COPY --from=base /lib/x86_64-linux-gnu/libacl.so.1 /lib/x86_64-linux-gnu/libattr.so.1 /lib/x86_64-linux-gnu/libaudit.so.1 /lib/x86_64-linux-gnu/libblkid.so.1 /lib/x86_64-linux-gnu/libbsd.so.0 /lib/x86_64-linux-gnu/libbz2.so.1.0 /lib/x86_64-linux-gnu/libcap-ng.so.0 /lib/x86_64-linux-gnu/libcom_err.so.2 /lib/x86_64-linux-gnu/libcrypt.so.1 /lib/x86_64-linux-gnu/libc.so.6 /lib/x86_64-linux-gnu/libdl.so.2 /lib/x86_64-linux-gnu/libe2p.so.2 /lib/x86_64-linux-gnu/libgcc_s.so.1 /lib/x86_64-linux-gnu/libgcrypt.so.20 /lib/x86_64-linux-gnu/libgpg-error.so.0 /lib/x86_64-linux-gnu/libidn.so.11 /lib/x86_64-linux-gnu/libkeyutils.so.1 /lib/x86_64-linux-gnu/liblzma.so.5 /lib/x86_64-linux-gnu/libmount.so.1 /lib/x86_64-linux-gnu/libm.so.6 /lib/x86_64-linux-gnu/libpam_misc.so.0 /lib/x86_64-linux-gnu/libpam.so.0 /lib/x86_64-linux-gnu/libpcre.so.3 /lib/x86_64-linux-gnu/libpthread.so.0 /lib/x86_64-linux-gnu/libreadline.so.7 /lib/x86_64-linux-gnu/libresolv.so.2 /lib/x86_64-linux-gnu/librt.so.1 /lib/x86_64-linux-gnu/libselinux.so.1 /lib/x86_64-linux-gnu/libtinfo.so.5 /lib/x86_64-linux-gnu/libutil.so.1 /lib/x86_64-linux-gnu/libuuid.so.1 /lib/x86_64-linux-gnu/libz.so.1 /usr/lib/x86_64-linux-gnu/libbfd-2.28-system.so /usr/lib/x86_64-linux-gnu/libcrypto.so.1.0.2 /usr/lib/x86_64-linux-gnu/libcrypto.so.1.1 /usr/lib/x86_64-linux-gnu/libcurl.so.4 /usr/lib/x86_64-linux-gnu/libffi.so.6 /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgnutls.so.30 /usr/lib/x86_64-linux-gnu/libgssapi_krb5.so.2 /usr/lib/x86_64-linux-gnu/libhogweed.so.4 /usr/lib/x86_64-linux-gnu/libidn2.so.0 /usr/lib/x86_64-linux-gnu/libk5crypto.so.3 /usr/lib/x86_64-linux-gnu/libkrb5.so.3 /usr/lib/x86_64-linux-gnu/libkrb5support.so.0 /usr/lib/x86_64-linux-gnu/liblber-2.4.so.2 /usr/lib/x86_64-linux-gnu/libldap_r-2.4.so.2 /usr/lib/x86_64-linux-gnu/libldns.so.2 /usr/lib/x86_64-linux-gnu/libmagic.so.1 /usr/lib/x86_64-linux-gnu/libmpfr.so.4 /usr/lib/x86_64-linux-gnu/libnettle.so.6 /usr/lib/x86_64-linux-gnu/libnghttp2.so.14 /usr/lib/x86_64-linux-gnu/libopcodes-2.28-system.so /usr/lib/x86_64-linux-gnu/libp11-kit.so.0 /usr/lib/x86_64-linux-gnu/libpsl.so.5 /usr/lib/x86_64-linux-gnu/librtmp.so.1 /usr/lib/x86_64-linux-gnu/libsasl2.so.2 /usr/lib/x86_64-linux-gnu/libsigsegv.so.2 /usr/lib/x86_64-linux-gnu/libssh2.so.1 /usr/lib/x86_64-linux-gnu/libssl.so.1.0.2 /usr/lib/x86_64-linux-gnu/libssl.so.1.1 /usr/lib/x86_64-linux-gnu/libstdc++.so.6 /usr/lib/x86_64-linux-gnu/libtasn1.so.6 /usr/lib/x86_64-linux-gnu/libunistring.so.0 /lib/
COPY --from=base /etc/debian_version /etc/debian_version
COPY --from=base /etc/profile /etc/profile
COPY --from=base /etc/bash.bashrc /etc/bash.bashrc
COPY --from=base /etc/passwd /etc/passwd
COPY --from=base /etc/group /etc/group
COPY --from=base /etc/shadow /etc/shadow
COPY --from=base /usr/lib/locale/locale-archive /usr/lib/locale/locale-archive
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!