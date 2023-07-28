FROM ubuntu:16.04
MAINTAINER mislav.novakovic@sartura.hr
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 cmake=3.5.1-1ubuntu3 build-essential=12.1ubuntu2 vim=2:7.4.1689-3ubuntu1.5 supervisor=3.2.0-2ubuntu0.2 libpcre3-dev=2:8.38-3.1 pkg-config=0.29.1-0ubuntu1 libavl-dev=0.3.5-3 libev-dev=1:4.22-1 libprotobuf-c-dev=1.2.1-1 protobuf-c-compiler=1.2.1-1 libssh-dev=0.6.3-4.3ubuntu0.6 libssl-dev=1.0.2g-1ubuntu4.20 swig=3.0.8-0ubuntu3 python-dev=2.7.12-1~16.04 -y
#   add netconf user
RUN adduser --system netconf \
 && echo "netconf:netconf" | chpasswd
#   generate ssh keys for netconf user
RUN mkdir -p /home/netconf/.ssh \
 && ssh-keygen -A \
 && ssh-keygen -t dsa -P '' -f /home/netconf/.ssh/id_dsa \
 && cat /home/netconf/.ssh/id_dsa.pub > /home/netconf/.ssh/authorized_keys
#   use /opt/dev as working directory
RUN mkdir /opt/dev
WORKDIR /opt/dev
#   libyang
RUN git clone -b devel https://github.com/CESNET/libyang.git \
 && cd libyang \
 && mkdir build \
 && cd build \
 && git checkout devel \
 && cmake -DCMAKE_BUILD_TYPE:String="Debug" -DENABLE_BUILD_TESTS=OFF .. \
 && make -j2 \
 && make install \
 && ldconfig
#   sysrepo
RUN git clone -b devel https://github.com/sysrepo/sysrepo.git \
 && cd sysrepo \
 && mkdir build \
 && cd build \
 && git checkout devel \
 && cmake -DCMAKE_BUILD_TYPE:String="Debug" -DENABLE_TESTS=OFF -DREPOSITORY_LOC:PATH=/etc/sysrepo .. \
 && make -j2 \
 && make install \
 && ldconfig
#   libssh
RUN git clone http://git.libssh.org/projects/libssh.git \
 && cd libssh \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j2 \
 && make install \
 && ldconfig
#   libnetconf2
RUN git clone -b devel https://github.com/CESNET/libnetconf2.git \
 && cd libnetconf2 \
 && mkdir build \
 && cd build \
 && git checkout devel \
 && cmake -DCMAKE_BUILD_TYPE:String="Debug" -DENABLE_BUILD_TESTS=OFF .. \
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
 && git checkout devel-server \
 && cmake -DCMAKE_BUILD_TYPE:String="Debug" .. \
 && make -j2 \
 && make install \
 && ldconfig
#   netopeer2
RUN cd /opt/dev \
 && cd Netopeer2/server \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE:String="Debug" .. \
 && make -j2 \
 && make install \
 && cd ../../cli \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE:String="Debug" .. \
 && make -j2 \
 && make install
#   remove old swig
RUN apt-get purge --auto-remove swig -y
#   install node 4 and tools for swig build
RUN apt-get update \
 && apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 byacc=20140715-1 autoconf=2.69-9 yodl=3.06.00-1 -y
#   link nodejs to node
RUN ln -s `which nodejs ` /usr/bin/node
#   latest SWIG version with pull request
RUN cd /opt/dev \
 && git clone https://github.com/swig/swig.git \
 && cd swig \
 && git fetch origin pull/236/head:javascript_sharedptr \
 && git checkout javascript_sharedptr \
 && ./autogen.sh \
 && ./configure --prefix=/usr \
 && make -j2 \
 && make install
#   libyang
RUN cd /opt/dev/libyang/build \
 && cmake -DENABLE_STATIC=ON -DGEN_LANGUAGE_BINDINGS=ON -DGEN_JAVASCRIPT_BINDINGS=ON -DGEN_PYTHON_BINDINGS=OFF -DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE:String="Release" .. \
 && make -j2 \
 && make install \
 && cd swig/javascript \
 && npm install --unsafe-perm \
 && node test.js \
 && ldconfig
ENV EDITOR="vim"
EXPOSE 830/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
