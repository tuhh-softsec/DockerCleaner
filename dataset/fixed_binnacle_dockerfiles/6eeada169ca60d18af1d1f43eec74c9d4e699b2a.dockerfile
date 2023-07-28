FROM ubuntu:16.04
MAINTAINER mislav.novakovic@sartura.hr
#   update system
RUN :
#   install basic dependencies
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 cmake=3.5.1-1ubuntu3 build-essential=12.1ubuntu2 vim=2:7.4.1689-3ubuntu1.5 supervisor=3.2.0-2ubuntu0.2 libpcre3-dev=2:8.38-3.1 pkg-config=0.29.1-0ubuntu1 libavl-dev=0.3.5-3 libev-dev=1:4.22-1 libprotobuf-c-dev=1.2.1-1 protobuf-c-compiler=1.2.1-1 libssl-dev=1.0.2g-1ubuntu4.20 swig=3.0.8-0ubuntu3 python-dev=2.7.12-1~16.04 -y )
#   add password to root
RUN echo "root:root" | chpasswd
#   add netconf user
RUN adduser --system netconf \
 && echo "netconf:netconf" | chpasswd
#   use /opt/dev as working directory
RUN mkdir /opt/dev
WORKDIR /opt/dev
#   generate default ssh key
RUN ssh-keygen -t rsa -N "" -f /etc/ssh/ssh_host_rsa_key
#   libredblack
RUN git clone https://github.com/sysrepo/libredblack.git \
 && cd libredblack \
 && ./configure --prefix=/usr \
 && make \
 && make install \
 && ldconfig
#   libyang
RUN git clone https://github.com/CESNET/libyang.git \
 && cd libyang \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE:String="Release" .. \
 && make -j2 \
 && make install \
 && ldconfig
#   sysrepo
RUN git clone https://github.com/sysrepo/sysrepo.git \
 && cd sysrepo \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE:String="Release" -DREPOSITORY_LOC:PATH=/etc/sysrepo .. \
 && make -j2 \
 && make install \
 && ldconfig
#   libssh
RUN git clone http://git.libssh.org/projects/libssh.git \
 && cd libssh \
 && git checkout -b libssh-0.7.5 \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr .. \
 && make -j2 \
 && make install \
 && ldconfig
#   libnetconf2
RUN git clone https://github.com/CESNET/libnetconf2.git \
 && cd libnetconf2 \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE:String="Release" .. \
 && make -j2 \
 && make install \
 && ldconfig
#   keystore
RUN cd /opt/dev \
 && git clone https://github.com/CESNET/Netopeer2.git \
 && cd Netopeer2 \
 && cd keystored \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE:String="Release" .. \
 && make -j2 \
 && make install \
 && ldconfig
#   netopeer2
RUN cd /opt/dev \
 && cd Netopeer2/server \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE:String="Release" .. \
 && make -j2 \
 && make install \
 && cd ../../cli \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE:String="Release" .. \
 && make -j2 \
 && make install
#   install python2 sysrepo language bindings
RUN (apt-get update ;apt-get install --no-install-recommends python-dev=2.7.12-1~16.04 -y )
RUN cd /opt/dev/sysrepo/build \
 && cmake -DGEN_PYTHON_VERSION=2 .. \
 && make -j2 \
 && make install
#   install lua5.1 sysrepo language bindings
RUN (apt-get update ;apt-get install --no-install-recommends lua5.1-dev -y )
RUN cd /opt/dev/sysrepo/build \
 && cmake -DGEN_LUA_VERSION=5.1 .. \
 && make -j2 \
 && make install
#   install python3 sysrepo language bindings
RUN (apt-get update ;apt-get install --no-install-recommends python3-dev=3.5.1-3 -y )
RUN cd /opt/dev/sysrepo/build \
 && cmake -DGEN_PYTHON_VERSION=3 .. \
 && make -j2 \
 && make install
#   install lua5.2 sysrepo language bindings
RUN (apt-get update ;apt-get install --no-install-recommends lua5.2-dev -y )
RUN cd /opt/dev/sysrepo/build \
 && cmake -DGEN_LUA_VERSION=5.2 .. \
 && make -j2 \
 && make install
ENV EDITOR="vim"
EXPOSE 830/tcp
COPY supervisord.conf /etc/supervisord.conf
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
