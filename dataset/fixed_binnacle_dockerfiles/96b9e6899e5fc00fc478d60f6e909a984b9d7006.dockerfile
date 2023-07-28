FROM ubuntu:xenial
MAINTAINER obscur95 <obscur95@gmail.com>
#
ENV DEBIAN_FRONTEND="noninteractive  DEBCONF_NONINTERACTIVE_SEEN true"
#
#  Configuration de la langue FR
#
RUN locale-gen fr_FR.UTF-8
ENV LANG="fr_FR.UTF-8"
ENV LANGUAGE="fr_FR:fr"
ENV LC_ALL="fr_FR.UTF-8"
#
#
#  Creation des repertoires
#
RUN mkdir /home/gns3 \
 && mkdir /home/gns3/GNS3 \
 && mkdir /home/gns3/GNS3/images \
 && mkdir /home/gns3/GNS3/images/IOS \
 && mkdir /home/gns3/GNS3/images/IOU \
 && mkdir /home/gns3/GNS3/images/QEMU \
 && mkdir /home/gns3/GNS3/images/QEMU/CSR1000v \
 && mkdir /home/gns3/GNS3/images/QEMU/Fortigate \
 && mkdir /home/gns3/GNS3/images/QEMU/PC \
 && mkdir /home/gns3/GNS3/projects
#
#  Copie du fichier sources.list
#
COPY sources.list /etc/apt/sources.list
#
#  Mise a jour de la distribution
#
RUN dpkg --add-architecture i386 \
 && : \
 && apt-get -y dist-upgrade
#
#  Installation des packages
#
RUN (apt-get update ;apt-get install --no-install-recommends libc6:i386 libstdc++6:i386 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends libssl1.0.0:i386 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends lsb-release=9.20160110ubuntu0.2 telnet=0.17-40 traceroute=1:2.0.21-1 tcpdump=4.9.3-0ubuntu0.16.04.1 net-tools=1.60-26ubuntu1 vim=2:7.4.1689-3ubuntu1.5 nano=2.5.3-2ubuntu2 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 bison=2:3.0.4.dfsg-1 flex=2.6.0-11 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends python3-setuptools=20.7.0-1 python3.5=3.5.2-2ubuntu0~16.04.13 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends python3-pip=8.1.1-2ubuntu0.6 -y ) \
 && python3.5 -m pip install -U pip
#
#  Pre-requis GNS3 server
#
RUN (apt-get update ;apt-get install --no-install-recommends qemu-kvm=1:2.5+dfsg-5ubuntu10.51 qemu-system-x86=1:2.5+dfsg-5ubuntu10.51 vpcs=0.5b2-1 dynamips=0.2.14-1 uuid-runtime=2.27.1-6ubuntu3.10 -y ) \
 && cd /tmp ; git clone http://github.com/ndevilla/iniparser.git ; cd iniparser ; make \
 && cp /tmp/iniparser/libiniparser.* /usr/lib/ \
 && cp /tmp/iniparser/src/iniparser.h /usr/local/include \
 && cp /tmp/iniparser/src/dictionary.h /usr/local/include \
 && cd /tmp ; git clone https://github.com/GNS3/iouyap.git ; cd iouyap ; make ; make install \
 && (apt-get update ;apt-get install --no-install-recommends libpcap-dev=1.7.4-2ubuntu0.1 -y ) \
 && cd /tmp ; git clone https://github.com/GNS3/ubridge.git ; cd ubridge ; make ; make install \
 && cd / \
 && ln -s /lib/i386-linux-gnu/libcrypto.so.1.0.0 /lib/i386-linux-gnu/libcrypto.so.4
#
#  Installation de GNS3 server
#
RUN python3.5 -m pip install gns3-server
#
#  Copie du fichier de configuration de GNS3 server
#
COPY GNS3.conf /etc/xdg/GNS3.conf
#
#  Copie des images IOS
#
COPY IOS/c3745-advipservicesk9-mz.124-25d.bin /home/gns3/GNS3/images/IOS/c3745-advipservicesk9-mz.124-25d.bin
COPY IOS/c7200-adventerprisek9-mz.124-15.T17.bin /home/gns3/GNS3/images/IOS/c7200-adventerprisek9-mz.124-15.T17.bin
COPY IOS/c7200-adventerprisek9-mz.152-4.S7.bin /home/gns3/GNS3/images/IOS/c7200-adventerprisek9-mz.152-4.S7.bin
#
#  Copie des images IOU
#
COPY IOU/CiscoIOUKeygen.py /home/gns3/GNS3/images/IOU/CiscoIOUKeygen.py
COPY IOU/i86bi-linux-l2-adventerprisek9-15.6.0.9S.bin /home/gns3/GNS3/images/IOU/i86bi-linux-l2-adventerprisek9-15.6.0.9S.bin
COPY IOU/i86bi-linux-l2-ipbasek9-15.1g.bin /home/gns3/GNS3/images/IOU/i86bi-linux-l2-ipbasek9-15.1g.bin
COPY IOU/i86bi-linux-l3-adventerprisek9-15.5.2T.bin /home/gns3/GNS3/images/IOU/i86bi-linux-l3-adventerprisek9-15.5.2T.bin
#
#  Copie des images QEMU
#
COPY QEMU/CSR1000v/csr1000v_harddisk.vmdk /home/gns3/GNS3/images/QEMU/CSR1000v/csr1000v_harddisk.vmdk
COPY QEMU/CSR1000v/csr1000v-universalk9.03.16.04b.S.155-3.S4b-ext.iso /home/gns3/GNS3/images/QEMU/CSR1000v/csr1000v-universalk9.03.16.04b.S.155-3.S4b-ext.iso
COPY QEMU/Fortigate/fortios_5.4.2.qcow2 /home/gns3/GNS3/images/QEMU/Fortigate/fortios_5.4.2.qcow2
COPY QEMU/Fortigate/fortigate_p10G.qcow2 /home/gns3/GNS3/images/QEMU/Fortigate/fortigate_p10G.qcow2
COPY QEMU/PC/TinyCore-current.iso /home/gns3/GNS3/images/QEMU/PC/TinyCore-current.iso
COPY QEMU/PC/TinyCore-hda.qcow2 /home/gns3/GNS3/images/QEMU/PC/TinyCore-hda.qcow2
#
#  Rends executable les images IOS et IOU
#
RUN chmod +x /home/gns3/GNS3/images/IOS/c3745-advipservicesk9-mz.124-25d.bin \
 && chmod +x /home/gns3/GNS3/images/IOS/c7200-adventerprisek9-mz.124-15.T17.bin \
 && chmod +x /home/gns3/GNS3/images/IOS/c7200-adventerprisek9-mz.152-4.S7.bin \
 && chmod +x /home/gns3/GNS3/images/IOU/i86bi-linux-l2-adventerprisek9-15.6.0.9S.bin \
 && chmod +x /home/gns3/GNS3/images/IOU/i86bi-linux-l2-ipbasek9-15.1g.bin \
 && chmod +x /home/gns3/GNS3/images/IOU/i86bi-linux-l3-adventerprisek9-15.5.2T.bin
#
#  Ouverture des ports
#
EXPOSE 3080/tcp 4000-4050 5900-5910
#
#  Copie du fichier de demarrage
#
COPY startup.sh /home/gns3/startup.sh
RUN chmod a+x /home/gns3/startup.sh
#
ENTRYPOINT cd /home/gns3 ; ./startup.sh
#
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
