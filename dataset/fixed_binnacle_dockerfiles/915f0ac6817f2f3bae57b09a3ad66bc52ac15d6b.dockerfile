FROM debian:stretch
MAINTAINER gulv@microsoft.com
RUN echo "deb http://debian-archive.trafficmanager.net/debian/ stretch main contrib non-free" >> /etc/apt/sources.list \
 && echo "deb-src http://debian-archive.trafficmanager.net/debian/ stretch main contrib non-free" >> /etc/apt/sources.list \
 && echo "deb http://debian-archive.trafficmanager.net/debian-security/ stretch/updates main contrib non-free" >> /etc/apt/sources.list \
 && echo "deb-src http://debian-archive.trafficmanager.net/debian-security/ stretch/updates main contrib non-free" >> /etc/apt/sources.list \
 && echo "deb http://debian-archive.trafficmanager.net/debian stretch-backports main" >> /etc/apt/sources.list
#  # Make apt-get non-interactive
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=1.4.11 default-jre-headless=2:1.8-58+deb9u1 openssh-server=1:7.4p1-10+deb9u7 curl=7.52.1-5+deb9u16 wget=1.18-5+deb9u3 unzip=6.0-21+deb9u2 git=1:2.11.0-3+deb9u7 build-essential=12.3 libtool=2.4.6-2 lintian=2.5.50.4 sudo=1.8.19p1-2.1+deb9u3 dh-make=2.201608 dh-exec=0.23+b1 kmod=23-2 libtinyxml2-4=4.0.1-1 libboost-program-options1.62-dev=1.62.0+dfsg-4 libtinyxml2-dev=4.0.1-1 python=2.7.13-2 python-pip=9.0.1-2+deb9u2 libncurses5-dev=6.0+20161126-1+deb9u2 texinfo=6.3.0.dfsg.1-1+b2 dh-autoreconf=14 python3-pip=9.0.1-2+deb9u2 doxygen=1.8.13-4+b1 devscripts=2.17.6+deb9u2 git-buildpackage=0.8.12.2 perl-modules libswitch-perl=2.17-2 dh-systemd=10.2.5 libreadline-dev=7.0-3 texlive-latex-base=2016.20170123-5 texlive-generic-recommended=2016.20170123-5 texlive-fonts-recommended=2016.20170123-5 libpam0g-dev=1.1.8-3.6 libpam-dev libcap-dev=1:2.25-1 imagemagick=8:6.9.7.4+dfsg-11+deb9u14 ghostscript=9.26a~dfsg-0+deb9u9 groff=1.22.3-9 libpcre3-dev=2:8.39-3 gawk=1:4.1.4+dfsg-1 chrpath=0.16-2+b1 libc-ares-dev=1.12.0-1+deb9u2 libsnmp-dev=5.7.3+dfsg-1.7+deb9u3 libjson-c3=0.12.1-1.1+deb9u1 libjson-c-dev=0.12.1-1.1+deb9u1 libsystemd-dev=232-25+deb9u14 python-ipaddr=2.1.11-2 libcmocka-dev=1.0.1-3 python3-all-dev=3.5.3-1 python3-all-dbg=3.5.3-1 install-info=6.3.0.dfsg.1-1+b2 logrotate=3.11.0-0.1 cdbs=0.4.150 libxml-simple-perl=2.22-1 graphviz=2.38.0-17+deb9u1 aspell=0.60.7~20110707-3+deb9u1 bc=1.06.95-9+b3 fakeroot=1.21-3.1 build-essential=12.3 devscripts=2.17.6+deb9u2 quilt=0.63-8 stgit=0.17.1-1 module-assistant=0.11.9 gem2deb=0.33.1 libboost-all-dev=1.62.0.1 libevent-dev=2.0.21-stable-3 libglib2.0-dev=2.50.3-2+deb9u3 libqt4-dev=4:4.8.7+dfsg-11+deb9u3 python-all-dev=2.7.13-2 python-twisted=16.6.0-2+deb9u3 phpunit=5.4.6-2~deb9u1 libbit-vector-perl=7.4-1+b2 openjdk-8-jdk=8u332-ga-1~deb9u1 javahelper=0.59 maven-debian-helper=2.1.3 ant=1.9.9-1+deb9u1 libmaven-ant-tasks-java=2.1.3-4 libhttpclient-java=4.5.2-2+deb9u1 libslf4j-java=1.7.22-1 libservlet3.1-java=8.5.54-0+deb9u8 qt5-default=5.7.1+dfsg-3+deb9u3 pkg-php-tools=1.35 libpcre3=2:8.39-3 libpcre3-dev=2:8.39-3 byacc=20140715-1+b1 flex=2.6.1-1.3 libglib2.0-dev=2.50.3-2+deb9u3 bison=2:3.0.4.dfsg-1+b1 expat=2.2.0-2+deb9u5 libexpat1-dev=2.2.0-2+deb9u5 dpatch=2.0.38 libdb-dev=5.3.1 iptables-dev=1.6.0+snapshot20161117-6 ctags libtool-bin=2.4.6-2 libxml2-dev=2.9.4+dfsg1-2.2+deb9u7 libusb-1.0-0-dev=2:1.0.21-1 libcurl3-nss-dev libunwind8-dev=1.1-4.1 telnet=0.17-41 libc-ares2=1.12.0-1+deb9u2 libgoogle-perftools4=2.5-2.2 cpio=2.11+dfsg-6 squashfs-tools=1:4.3-3+deb9u3 zip=3.0-11+b1 linux-compiler-gcc-6-x86=4.9.320-2 linux-kbuild-4.9=4.9.320-2 libdaemon-dev=0.14-6 libdbus-1-dev=1.10.32-0+deb9u1 libjansson-dev=2.9-1 libpcap-dev=1.8.1-3+deb9u1 dnsutils=1:9.10.3.dfsg.P4-12.3+deb9u12 libusb-dev=2:0.1.12-30 augeas-tools=1.8.0-1+deb9u1 libyaml-dev=0.1.7-2 libevent-dev=2.0.21-stable-3 libjudy-dev=1.0.5-5 libedit-dev=3.1-20160903-3 libnanomsg-dev=0.8~beta+dfsg-1+b1 python-stdeb=0.8.5-1 libjemalloc-dev=3.6.0-9.1 dkms=2.3-2 sharutils=1:4.15.2-2+deb9u1 libncursesw5-dev=6.0+20161126-1+deb9u2 libbz2-dev=1.0.6-8.1 liblzma-dev=5.2.2-1.2+deb9u1 libgdbm-dev=1.8.3-14 tk-dev=8.6.0+9 blt-dev=2.5.3+dfsg-3 libmpdec-dev=2.4.2-1 libbluetooth-dev=5.43-2+deb9u5 locales=2.24-11+deb9u4 libsqlite3-dev=3.16.2-5+deb9u3 libgpm2=1.20.4-6.2+b1 time=1.7-25.1+b1 net-tools=1.60+git20161116.90da8a0-1 xvfb=2:1.19.2-1+deb9u9 python-sphinx=1.4.9-2 python3-sphinx=1.4.9-2 cppcheck=1.76.1-1 clang=1:3.8-36 pylint=1.6.5-1 python-pytest=3.0.6-1 gcovr=3.3-1 python-pytest-cov=2.4.0-2 python-parse=1.6.6-0.1 default-libmysqlclient-dev=1.0.2 libssl1.0-dev=1.0.2u-1~deb9u7 libperl-dev=5.24.1-3+deb9u7 libpci-dev=1:3.5.2-1 libpci3=1:3.5.2-1 libsensors4=1:3.4.0-4 libsensors4-dev=1:3.4.0-4 libwrap0-dev=7.6.q-26 debhelper=10.2.5 autotools-dev=20161112.1 libbsd-dev=0.8.3-1+deb9u1 pkg-config=0.29-4+b1 check=0.10.0-3+b3 docutils-common=0.13.1+dfsg-2 libjs-sphinxdoc=1.4.9-2 libjs-underscore=1.8.3~dfsg-1+deb9u1 python-docutils=0.13.1+dfsg-2 python-jinja2=2.8-1 python-markupsafe=0.23-3 python-pygments=2.2.0+dfsg-1+deb9u2 python-roman=2.0.0-2 python-sphinx=1.4.9-2 sphinx-common=1.4.9-2 python3-sphinx=1.4.9-2 python-lxml=3.7.1-1+deb9u5 python-jinja2=2.8-1 python-netaddr=0.7.18-2 python-ipaddr=2.1.11-2 python-yaml=3.12-1 python3-yaml=3.12-1 procmail=3.22-25+deb9u1 libgtest-dev=1.8.0-6 cmake=3.7.2-1 autoconf-archive=20160916-1 python-sphinx=1.4.9-2 python-docutils=0.13.1+dfsg-2 python3-all=3.5.3-1 python3-setuptools=33.1.1-1 python3-sphinx=1.4.9-2 python3-docutils=0.13.1+dfsg-2 python3-requests=2.12.4-1 python3-pytest=3.0.6-1 python3-colorama=0.3.7-1 bash-completion=1:2.1-4.3 dosfstools=4.1-1 qemu-kvm=1:2.8+dfsg-6+deb9u17 libvirt-clients=3.0.0-4+deb9u5 librrd8=1.6.0-1+b2 librrd-dev=1.6.0-1+b2 rrdtool=1.6.0-1+b2 automake1.11=1:1.11.6-4 libselinux1-dev=2.6-3+b3 -y )
#   For smartmontools 6.6-1
RUN apt-get -t stretch-backports install -y debhelper
#   For linux build
RUN apt-get -y build-dep linux
#   For gobgp and telemetry build
RUN export VERSION=1.11.5 \
 && wget https://storage.googleapis.com/golang/go$VERSION.linux-amd64.tar.gz \
 && tar -C /usr/local -xzf go$VERSION.linux-amd64.tar.gz \
 && echo 'export GOROOT=/usr/local/go' >> /etc/bash.bashrc \
 && echo 'export PATH=$PATH:$GOROOT/bin' >> /etc/bash.bashrc
#   For p4 build
RUN pip install ctypesgen==1.1.1 crc16==0.1.1
#   For sonic config engine testing
RUN pip install pyangbind==0.6.0
#   Note: force upgrade debian packaged jinja2, if installed
RUN pip install jinja2==3.1.2 --force-reinstall --upgrade > =2.10
#   For templating
RUN pip install j2cli==0.3.10
#   Remove python-click 6.6
RUN apt-get purge -y python-click
#   For sonic utilities testing
RUN pip install click-default-group==1.2.2 click==8.1.3 natsort==8.3.1 tabulate==0.9.0 netifaces==0.10.7 fastentrypoints==0.12
#   For sonic snmpagent mock testing
RUN pip3 install mockredispy==2.9.3
RUN pip3 install PyYAML > =5.1
#   For sonic-platform-common testing
RUN pip3 install redis
#   For supervisor build
RUN pip install meld3==2.0.1 mock==5.0.2
#   For vs image build
RUN pip install pexpect==4.6.0
#   For sonic-utilities build
RUN pip install mockredispy==2.9.3
RUN pip install pytest-runner==4.4
RUN pip install setuptools==40.8.0
#   Install dependencies for isc-dhcp-relay build
RUN apt-get -y build-dep isc-dhcp
#   Install vim
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:8.0.0197-4+deb9u7 -y )
#   Install rsyslog
RUN (apt-get update ;apt-get install --no-install-recommends rsyslog=8.24.0-1+deb9u3 -y )
RUN cd /usr/src/gtest \
 && cmake . \
 && make -C /usr/src/gtest
RUN mkdir /var/run/sshd
EXPOSE 22/tcp
#   Install depot-tools (for git-retry)
RUN git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git /usr/share/depot_tools
ENV PATH="/usr/share/depot_tools:$PATH"
#   Install docker engine 17.03.2~ce-0 inside docker and enable experimental feature
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends apt-transport-https=1.4.11 ca-certificates=20200601~deb9u2 curl=7.52.1-5+deb9u16 gnupg2=2.1.18-8~deb9u4 software-properties-common=0.96.20.2-1+deb9u1 -y )
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $( lsb_release -cs ;) stable"
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends docker-ce=5:18.09.5~3-0~debian-stretch -y )
RUN echo "DOCKER_OPTS=\"--experimental --storage-driver=vfs\"" >> /etc/default/docker
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
