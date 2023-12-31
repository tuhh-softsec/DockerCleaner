FROM phusion/baseimage:0.9.19
ENV LANG="en_US.UTF-8"
COPY ./usr/local/src/viz
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf automake autotools-dev bsdmainutils build-essential cmake doxygen git ccache libboost-all-dev libreadline-dev libssl-dev libtool ncurses-dev pbzip2 pkg-config python3 python3-dev python3-pip -y \
 && pip3 install gcovr \
 && cd /usr/local/src/viz \
 && git submodule deinit -f . \
 && git submodule update --init --recursive -f \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBRARIES=FALSE -DLOW_MEMORY_NODE=TRUE -DCHAINBASE_CHECK_LOCKING=FALSE -DENABLE_MONGO_PLUGIN=FALSE .. \
 && make -j$( nproc ;) \
 && make install \
 && rm -rf /usr/local/src/viz \
 && apt-get remove -y automake autotools-dev bsdmainutils build-essential cmake doxygen dpkg-dev git libboost-all-dev libc6-dev libexpat1-dev libgcc-5-dev libhwloc-dev libibverbs-dev libicu-dev libltdl-dev libncurses5-dev libnuma-dev libopenmpi-dev libpython-dev libpython2.7-dev libreadline-dev libreadline6-dev libssl-dev libstdc++-5-dev libtinfo-dev libtool linux-libc-dev m4 make manpages manpages-dev mpi-default-dev python-dev python2.7-dev python3-dev \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /var/cache/* /usr/include /usr/local/include \
 && useradd -s /bin/bash -m -d /var/lib/vizd vizd \
 && mkdir /var/cache/vizd \
 && chown vizd:vizd -R /var/cache/vizd
#  add blockchain cache to image
# ADD $VIZD_BLOCKCHAIN /var/cache/vizd/blocks.tbz2
ENV HOME="/var/lib/vizd"
RUN chown vizd:vizd -R /var/lib/vizd
COPY share/vizd/snapshot.json /var/lib/vizd
#  rpc service:
#  http
EXPOSE 8090/tcp
#  ws
EXPOSE 8091/tcp
#  p2p service:
EXPOSE 2001/tcp
RUN mkdir -p /etc/service/vizd
COPY share/vizd/vizd.sh /etc/service/vizd/run
RUN chmod +x /etc/service/vizd/run
#  add seednodes from documentation to image
COPY share/vizd/seednodes /etc/vizd/seednodes
#  the following adds lots of logging info to stdout
COPY share/vizd/config/config.ini /etc/vizd/config.ini
