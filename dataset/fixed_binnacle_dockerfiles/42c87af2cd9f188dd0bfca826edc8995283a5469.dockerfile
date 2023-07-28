FROM ubuntu:14.04
MAINTAINER Antonin Bas <antonin@barefootnetworks.com>
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends automake=1:1.14.1-2ubuntu1 bridge-utils=1.5-6ubuntu2 build-essential=11.6ubuntu6 cmake=2.8.12.2-0ubuntu3 ethtool=1:3.13-1 git=1:1.9.1-1ubuntu0.10 libboost-dev=1.54.0.1ubuntu1 libboost-filesystem-dev=1.54.0.1ubuntu1 libboost-program-options-dev=1.54.0.1ubuntu1 libboost-system-dev=1.54.0.1ubuntu1 libboost-test-dev=1.54.0.1ubuntu1 libboost-thread-dev=1.54.0.1ubuntu1 libedit-dev=3.1-20130712-2 libevent-dev=2.0.21-stable-1ubuntu1.14.04.2 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libgmp-dev=2:5.1.3+dfsg-1ubuntu1 libglib2.0-dev=2.40.2-0ubuntu1.1 libhiredis-dev=0.11.0-3 libjudy-dev=1.0.5-1ubuntu1 libnl-route-3-dev=3.2.21-1ubuntu4.1 libpcap0.8=1.5.3-2 libpcap0.8-dev=1.5.3-2 libtool=2.4.2-1.7ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 openssh-server=1:6.6p1-2ubuntu2.13 packit=1.0-2 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 python-pygraph=1.8.2-4 python-pygraphviz=1.2-1 python-setuptools=3.3-1ubuntu2 python-thrift=0.9.0-1build1 python-yaml=3.10-4ubuntu0.1 quagga=0.99.22.4-3ubuntu1.5 redis-server=2:2.8.4-2ubuntu0.2 redis-tools=2:2.8.4-2ubuntu0.2 subversion=1.8.8-1ubuntu3.3 tshark=2.6.6-1~ubuntu14.04.0 xterm=297-1ubuntu1 -y )
RUN pip install tenjin==1.1.1
#   install scapy
RUN mkdir -p /tmp/scapy ; cd /tmp/scapy ; git clone https://github.com/p4lang/scapy-vxlan.git ; cd scapy-vxlan ; python setup.py install ; rm -fr /tmp/scapy
#   install p4-hlir
RUN mkdir -p /tmp/p4-hlir ; cd /tmp/p4-hlir ; git clone https://github.com/p4lang/p4-hlir.git ; cd p4-hlir ; python setup.py install ; rm -fr /tmp/p4-hlir
#   install mstpd
RUN mkdir -p /third-party/diffs
COPY diffs/mstpd.diff /third-party/diffs/mstpd.diff
RUN cd /third-party ; svn checkout svn://svn.code.sf.net/p/mstpd/code/trunk mstpd ; cd mstpd ; patch -p0 -i /third-party/diffs/mstpd.diff ; make install
#   install ctypesgen
RUN mkdir -p /tmp/ctypesgen ; cd /tmp/ctypesgen ; git clone https://github.com/davidjamesca/ctypesgen.git ; cd ctypesgen ; python setup.py install ; rm -fr /tmp/ctypesgen
COPY p4factory /p4factory
RUN mkdir -p /tmp/install_tmp ; cd install_tmp ; wget -c http://archive.apache.org/dist/thrift/0.9.2/thrift-0.9.2.tar.gz ; tar zxvf thrift-0.9.2.tar.gz ; cd thrift-0.9.2 ; ./configure --with-cpp=yes --with-c_glib=no --with-java=no --with-ruby=no --with-erlang=no --with-go=no --with-nodejs=no ; make -j4 ; make install ; ldconfig ; cd .. ; wget https://github.com/nanomsg/nanomsg/archive/1.0.0.tar.gz -O nanomsg-1.0.0.tar.gz ; tar -xzvf nanomsg-1.0.0.tar.gz ; cd nanomsg-1.0.0 ; mkdir build ; cd build ; cmake .. -DCMAKE_INSTALL_PREFIX=/usr ; cmake --build . ; cmake --build . --target install ; cd ../../ ; git clone https://github.com/nanomsg/nnpy.git ; cd nnpy ; git checkout c7e718a5173447c85182dc45f99e2abcf9cd4065 ; ldconfig ; pip install cffi==1.15.1 ; pip install . ; cd ../.. ; rm -rf /tmp/install_tmp
ENV VTYSH_PAGER="more"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
