#   WHAT IS THIS FOR?
#   This is a copy of Dockerfile, but prior to compiling, will set difficult to 500, resets all the
#   seed nodes to 127.0.0.1 (to avoid annoying mainnet), changes the daemon name to "TortleCoin, and mixes up the network ID.
#   Useless for mainnet, but used to setup a testnet with checkpoints, and not have to spend days mining to get to the next block!
#
#   daemon runs in the background
#   run something like tail /var/log/turtlecoind/current to see the status
#   be sure to run with volumes, ie:
#   docker run -v $(pwd)/turtlecoind:/var/lib/turtlecoind -v $(pwd)/wallet:/home/turtlecoin --rm -ti turtlecoin:0.2.2
ARG base_image_version=0.10.0
FROM phusion/baseimage:$base_image_version
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/ https://github.com/just-containers/s6-overlay/releases/download/v1.21.2.2/s6-overlay-amd64.tar.gz
RUN tar xzf /tmp/s6-overlay-amd64.tar.gz -C /
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/ https://github.com/just-containers/socklog-overlay/releases/download/v2.1.0-0/socklog-overlay-amd64.tar.gz
RUN tar xzf /tmp/socklog-overlay-amd64.tar.gz -C /
#   Get the latest checkpoints (we don't care about being _100%_ up-to-date, but we need to be past block 350K for the algo change to CN_lite_v1)
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/ https://raw.githubusercontent.com/turtlecoin/checkpoints/master/checkpoints.csv
#   In a multi-node testnet, we'll want to have the slave nodes "wait" for the master node to wake up, so let's get ready
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/ https://raw.githubusercontent.com/vishnubob/wait-for-it/master/wait-for-it.sh
ARG TURTLECOIN_BRANCH=master
ENV TURTLECOIN_BRANCH="${TURTLECOIN_BRANCH}"
#   install build dependencies
#   checkout the latest tag
#   build and install
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential python-dev gcc-4.9 g++-4.9 git cmake libboost1.58-all-dev -y \
 && git clone https://github.com/turtlecoin/turtlecoin.git /src/turtlecoin \
 && cd /src/turtlecoin \
 && git checkout $TURTLECOIN_BRANCH \
 && sed -i '/std::vector<uint64_t> timestamps_o(timestamps);/i \\/*\n Lower difficulty to static 500 for testnet\n The rest of this function is ignored\n*\/\nreturn 500;\n\n' src/CryptoNoteCore/Currency.cpp \
 && sed -i -e 's/"104.236.227.176:11897",/"172.16.76.11:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/"46.101.132.184:11897",/"172.16.76.12:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/"163.172.147.52:11897",/"172.16.76.13:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/"51.15.138.214:11897",/"192.168.76.11:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/"51.15.137.77:11897",/"192.168.76.12:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/"174.138.68.141:11897", \/\/\^ rock/"192.168.76.13:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/"145.239.88.119:11999", \/\/cision/"10.0.76.11:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/"142.44.242.106:11897", \/\/tom/"10.0.76.12:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/"165.227.252.132:11897" \/\/iburnmycd/"10.0.76.13:11897",/g' src/CryptoNoteConfig.h \
 && sed -i -e 's/TurtleCoin/TortleCoin/' src/CryptoNoteConfig.h \
 && sed -i -e 's/0xcf, 0x52, 0x57/0x52, 0xcf, 0x57/' src/P2p/P2pNetworks.h \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_CXX_FLAGS="-g0 -Os -fPIC -std=gnu++11" .. \
 && make -j$( nproc ;) \
 && mkdir -p /usr/local/bin \
 && cp src/TurtleCoind /usr/local/bin/TurtleCoind \
 && cp src/walletd /usr/local/bin/walletd \
 && cp src/zedwallet /usr/local/bin/zedwallet \
 && cp src/miner /usr/local/bin/miner \
 && strip /usr/local/bin/TurtleCoind \
 && strip /usr/local/bin/walletd \
 && strip /usr/local/bin/zedwallet \
 && strip /usr/local/bin/miner \
 && cd / \
 && rm -rf /src/turtlecoin \
 && apt-get remove -y build-essential python-dev gcc-4.9 g++-4.9 git cmake libboost1.58-all-dev \
 && apt-get autoremove -y \
 && apt-get install --no-install-recommends libboost-system1.58.0 libboost-filesystem1.58.0 libboost-thread1.58.0 libboost-date-time1.58.0 libboost-chrono1.58.0 libboost-regex1.58.0 libboost-serialization1.58.0 libboost-program-options1.58.0 libicu55 -y
#   setup the turtlecoind service
RUN useradd -r -s /usr/sbin/nologin -m -d /var/lib/turtlecoind turtlecoind \
 && useradd -s /bin/bash -m -d /home/turtlecoin turtlecoin \
 && mkdir -p /etc/services.d/turtlecoind/log \
 && mkdir -p /var/log/turtlecoind \
 && echo "#!/usr/bin/execlineb" > /etc/services.d/turtlecoind/run \
 && echo "fdmove -c 2 1" >> /etc/services.d/turtlecoind/run \
 && echo "cd /var/lib/turtlecoind" >> /etc/services.d/turtlecoind/run \
 && echo "export HOME /var/lib/turtlecoind" >> /etc/services.d/turtlecoind/run \
 && echo "s6-setuidgid turtlecoind /usr/local/bin/TurtleCoind --log-level=2" >> /etc/services.d/turtlecoind/run \
 && chmod +x /etc/services.d/turtlecoind/run \
 && chmod +x /tmp/wait-for-it.sh \
 && chown nobody:nogroup /var/log/turtlecoind \
 && echo "#!/usr/bin/execlineb" > /etc/services.d/turtlecoind/log/run \
 && echo "s6-setuidgid nobody" >> /etc/services.d/turtlecoind/log/run \
 && echo "s6-log -bp -- n20 s1000000 /var/log/turtlecoind" >> /etc/services.d/turtlecoind/log/run \
 && chmod +x /etc/services.d/turtlecoind/log/run \
 && echo "/var/lib/turtlecoind true turtlecoind 0644 0755" > /etc/fix-attrs.d/turtlecoind-home \
 && echo "/home/turtlecoin true turtlecoin 0644 0755" > /etc/fix-attrs.d/turtlecoin-home \
 && echo "/tmp/checkpoints.csv true turtlecoin 0644 0755" > /etc/fix-attrs.d/turtlecoin-checkpoints \
 && echo "/var/log/turtlecoind true nobody 0644 0755" > /etc/fix-attrs.d/turtlecoind-logs
VOLUME ["/var/lib/turtlecoind", "/home/turtlecoin","/var/log/turtlecoind"]
ENTRYPOINT ["/init"]
CMD ["/usr/bin/execlineb", "-P", "-c", "emptyenv", "cd", "/home/turtlecoin", "export", "HOME", "/home/turtlecoin", "s6-setuidgid", "turtlecoin", "tail", "-f", "/var/log/turtlecoind/current"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
