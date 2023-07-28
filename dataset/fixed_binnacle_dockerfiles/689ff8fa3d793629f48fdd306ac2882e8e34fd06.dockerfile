FROM ubuntu:18.04
MAINTAINER mislav.novakovic@sartura.hr
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 cmake=3.10.2-1ubuntu2.18.04.2 build-essential=12.4ubuntu1 vim=2:8.0.1453-1ubuntu1.11 supervisor=3.3.1-1.1 libpcre3-dev=2:8.39-9ubuntu0.1 pkg-config=0.29.1-0ubuntu2 libavl-dev=0.3.5-4 libev-dev=1:4.22-1 libprotobuf-c-dev=1.2.1-2 protobuf-c-compiler=1.2.1-2 libssh-dev=0.8.0~20170825.94fa1e38-1ubuntu0.7 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 swig=3.0.12-1 python-dev=2.7.15~rc1-1 -y
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
ENV EDITOR="vim"
EXPOSE 830/tcp
COPY supervisord.conf /etc/supervisord.conf
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
