FROM ubuntu:14.04
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libjudy-dev=1.0.5-1ubuntu1 libgmp-dev=2:5.1.3+dfsg-1ubuntu1 libpcap-dev=1.5.3-2 libboost-dev=1.54.0.1ubuntu1 libboost-test-dev=1.54.0.1ubuntu1 libboost-program-options-dev=1.54.0.1ubuntu1 libboost-system-dev=1.54.0.1ubuntu1 libboost-filesystem-dev=1.54.0.1ubuntu1 libboost-thread-dev=1.54.0.1ubuntu1 libboost-test-dev=1.54.0.1ubuntu1 libevent-dev=2.0.21-stable-1ubuntu1.14.04.2 automake=1:1.14.1-2ubuntu1 libtool=2.4.2-1.7ubuntu1 flex=2.5.35-10.1ubuntu2 bison=2:3.0.2.dfsg-2 pkg-config=0.26-1ubuntu4 g++=4:4.8.2-1ubuntu6 libssl-dev=1.0.1f-1ubuntu2.27 doxygen=1.8.6-2 git=1:1.9.1-1ubuntu0.10 libedit-dev=3.1-20130712-2 libevent-dev=2.0.21-stable-1ubuntu1.14.04.2 libfreetype6-dev=2.5.2-1ubuntu2.8 libpng-dev libyaml-0-2=0.1.4-3ubuntu3.1 libbz2-dev=1.0.6-5 libnl-route-3-dev=3.2.21-1ubuntu4.1 openssl=1.0.1f-1ubuntu2.27 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 python-matplotlib=1.3.1-1ubuntu5.1 python-numpy=1:1.8.2-0ubuntu0.1 python-pip=1.5.4-1ubuntu4 python-scipy=0.13.3-1build1 python-setuptools=3.3-1ubuntu2 python-yaml=3.10-4ubuntu0.1 wget=1.15-1ubuntu1.14.04.5 ethtool=1:3.13-1 vim=2:7.4.052-1ubuntu3.1 -y )
#   install scapy
RUN mkdir -p /tmp/scapy ; cd /tmp/scapy ; git clone https://github.com/p4lang/scapy-vxlan.git ; cd scapy-vxlan ; python setup.py install ; rm -fr /tmp/scapy
#   install p4-hlir
RUN mkdir -p /tmp/p4-hlir ; cd /tmp/p4-hlir ; git clone https://github.com/p4lang/p4-hlir.git ; cd p4-hlir ; python setup.py install ; rm -fr /tmp/p4-hlir
#   install ctypesgen
RUN mkdir -p /tmp/ctypesgen ; cd /tmp/ctypesgen ; git clone https://github.com/davidjamesca/ctypesgen.git ; cd ctypesgen ; python setup.py install ; rm -fr /tmp/ctypesgen
#   install bmv2
RUN mkdir -p /tmp/bm ; cd /tmp/bm ; git clone https://github.com/p4lang/behavioral-model ; cd behavioral-model ; ./install_deps.sh ; ./autogen.sh ; ./configure --with-pdfixed ; make ; make install ; ldconfig ; rm -rf /tmp/bm
#   install p4c-bm
RUN mkdir /tmp/p4c-bm ; cd /tmp/p4c-bm ; git clone https://github.com/p4lang/p4c-bm ; cd p4c-bm ; sudo python setup.py install ; rm -rf /tmp/p4c-bm
#   install p4ofagent
RUN mkdir /tmp/p4ofagent ; cd /tmp/p4ofagent ; git clone https://github.com/p4lang/p4ofagent ; cd p4ofagent ; git submodule update --init ; cd submodules/indigo/ ; find -name ".gitmodules" -type f -exec sed -i 's/git@github.com:/https:\/\/github.com\//' {}
#   build switch
RUN git clone https://github.com/p4lang/switch ; cd switch ; git submodule update --init --recursive ; ./autogen.sh ; ./configure --with-bmv2 --with-of ; make
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
