#   Version: 0.7.1
FROM ubuntu:14.04
MAINTAINER Valerio Di Giampietro "valerio@digiampietro.com"
#
#   increase the version to force recompilation of everything
#
ENV GNS3LARGEVERSION="0.7.2"
#
#   ------------------------------------------------------------------
#   environment variables to avoid that dpkg-reconfigure 
#   tries to ask the user any questions
#
ENV DEBIAN_FRONTEND="noninteractive"
ENV DEBCONF_NONINTERACTIVE_SEEN="true"
#
#   ----------------------------------------------------------------- 
#   install needed packages to build and run gns3 and related sw
#
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 wget=1.15-1ubuntu1.14.04.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libpcap-dev=1.5.3-2 uuid-dev=2.20.1-5.1ubuntu20.9 libelf-dev=0.158-0ubuntu5.3 cmake=2.8.12.2-0ubuntu3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python3-setuptools=3.3-1ubuntu2 python3-pyqt4=4.10.4+dfsg-1ubuntu1 python3-ws4py=0.3.2-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python3-netifaces=0.8-3build1 python3-zmq=14.0.1-1build2 python3-tornado=3.1.1-1ubuntu2 python3-dev=3.4.0-0ubuntu2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends bison=2:3.0.2.dfsg-2 flex=2.5.35-10.1ubuntu2 -y )
#   
#   for iou install 32 bit libraries, python is needed to generate the license file
#
RUN (apt-get update ;apt-get install --no-install-recommends lib32z1=1:1.2.8.dfsg-1ubuntu1.1 lib32ncurses5=5.9+20140118-1ubuntu1 lib32bz2-1.0=1.0.6-5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends lxterminal=0.1.11-4ubuntu3.1 telnet=0.17-36build2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python=2.7.5-5ubuntu3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wireshark=2.6.6-1~ubuntu14.04.0 cpulimit=2.0-1 -y )
#
#   -----------------------------------------------------------------
#   compile and install dynamips, gns3-server, gns3-gui
#
RUN mkdir /src
RUN cd /src ; git clone https://github.com/GNS3/dynamips.git
RUN cd /src/dynamips ; git checkout v0.2.14
RUN mkdir /src/dynamips/build
RUN cd /src/dynamips/build ; cmake .. ; make ; make install
#
RUN cd /src ; git clone https://github.com/GNS3/gns3-gui.git
RUN cd /src ; git clone https://github.com/GNS3/gns3-server.git
RUN cd /src/gns3-server ; git checkout v1.2.3 ; python3 setup.py install
RUN cd /src/gns3-gui ; git checkout v1.2.3 ; python3 setup.py install
#
#  -----------------------------------------------------------------------
#   compile and install vpcs, 64 bit version
#
RUN cd /src ; wget -O - http://sourceforge.net/projects/vpcs/files/0.5/beta/vpcs-0.5b2-src.tbz/download | bzcat | tar -xvf -
RUN cd /src/vpcs-*/src ; ./mk.sh 64
RUN cp /src/vpcs-*/src/vpcs /usr/local/bin/vpcs
#
#   --------------------------------------------------------------------
#   compile and install iniparser (needed for iouyap) and 
#   iouyap (needed to run iou without additional virtual machine)
#
RUN cd /src ; git clone http://github.com/ndevilla/iniparser.git
RUN cd /src/iniparser ; make
RUN cd /src/iniparser ; cp libiniparser.* /usr/lib ; cp src/iniparser.h /usr/local/include/ ; cp src/dictionary.h /usr/local/include/
#
RUN cd /src ; git clone https://github.com/GNS3/iouyap.git
RUN cd /src/iouyap ; make
RUN cd /src/iouyap ; cp iouyap /usr/local/bin
#
#   to run iou 32 bit support is needed so add i386 repository, cannot be done
#   before compiling dynamips
#
RUN dpkg --add-architecture i386
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libssl-dev:i386 -y )
#
#   ---------------------------------------------------------------------------
#   install QEMU
#
RUN (apt-get update ;apt-get install --no-install-recommends qemu=2.0.0+dfsg-2ubuntu1.46 -y )
#
#   ---------------------------------------------------------------------------
#   install uml-utilities e iptables to be able to  use tap0 device
#   and NAT
#
RUN (apt-get update ;apt-get install --no-install-recommends uml-utilities=20070815-1.3ubuntu1 iptables=1.4.21-1ubuntu1 -y )
#
#   ---------------------------------------------------------------------------
#   these links are needed to run IOU
#
RUN ln -s /usr/lib/i386-linux-gnu/libcrypto.so /usr/lib/i386-linux-gnu/libcrypto.so.4
#
#
#   prepare startup files /src/misc
#
RUN mkdir /src/misc
#
#   install gnome connection manager
#
RUN cd /src/misc ; wget http://kuthulu.com/gcm/gnome-connection-manager_1.1.0_all.deb
#  RUN cd /src/misc; wget http://va.ler.io/myfiles/deb/gnome-connection-manager_1.1.0_all.deb
RUN (apt-get update ;apt-get install --no-install-recommends expect=5.45-5ubuntu1 python-vte=1:0.28.2-5ubuntu1 python-glade2=2.24.0-3ubuntu3 -y )
RUN mkdir -p /usr/share/desktop-directories
RUN cd /src/misc ; dpkg -i gnome-connection-manager_1.1.0_all.deb
RUN (while true ; do echo ; done ) | perl -MCPAN -e 'install JSON::Tiny'
RUN (while true ; do echo ; done ) | perl -MCPAN -e 'install File::Slurp'
#  RUN cd /usr/local/bin; ln -s /usr/share/gnome-connection-manager/* .
COPY gcmconf /usr/local/bin/gcmconf
COPY startup.sh /src/misc/startup.sh
COPY iourc.sample /src/misc/iourc.txt
COPY gcm /usr/local/bin/gcm
#   Set the locale
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8  "
ENV LANGUAGE="en_US:en  "
ENV LC_ALL="en_US.UTF-8  "
RUN chmod a+x /src/misc/startup.sh
ENTRYPOINT cd /src/misc ; ./startup.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
