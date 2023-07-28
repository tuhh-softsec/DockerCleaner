FROM ubuntu
MAINTAINER syu_cream
WORKDIR /opt
#
#   Prepare build environment
#
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 build-essential=12.9ubuntu3 autoconf=2.71-3 automake=1:1.16.5-1.3 autotools-dev=20220109.1 libtool=2.4.7-5 pkg-config=1.8.1-1ubuntu2 -y )
#
#   Prepare build environment for mruby and ts_mruby
#
RUN (apt-get update ;apt-get install --no-install-recommends bison=2:3.8.2+dfsg-1build1 libreadline6 libreadline6-dev ncurses-dev ruby=1:3.1 unzip=6.0-27ubuntu1 -y )
#
#   Prepare develop environment
#
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 vim=2:9.0.1000-4ubuntu2 valgrind=1:3.19.0-1ubuntu1 wget=1.21.3-1ubuntu1 -y )
#
#   Install nginx to test trafficserver's proxy features
#
RUN (apt-get update ;apt-get install --no-install-recommends nginx=1.22.0-1ubuntu3 -y )
#
#   Install packages required by trafficserver
#
RUN (apt-get update ;apt-get install --no-install-recommends g++=4:12.2.0-3ubuntu1 libssl-dev=3.0.8-1ubuntu1 tcl-dev=8.6.13 libpcre3-dev=2:8.39-15 -y )
RUN ldconfig
#
#   Prepare HTTP server as origin server of trafficserver
#
RUN mkdir /opt/htdocs
RUN echo "TEST" > /opt/htdocs/index.html
#
#   Build and install latest opnessl
#
RUN git clone https://github.com/openssl/openssl.git
RUN cd openssl \
 && git checkout -b OpenSSL_1_0_2h refs/tags/OpenSSL_1_0_2h \
 && ./config \
 && make \
 && make install
#
#   Build and install trafficserver
#
RUN wget -O trafficserver-7.0.0.tar.bz2 http://ftp.meisei-u.ac.jp/mirror/apache/dist/trafficserver/trafficserver-7.0.0.tar.bz2
RUN tar xf trafficserver-7.0.0.tar.bz2 \
 && cd trafficserver-7.0.0 \
 && ./configure --enable-debug \
 && make \
 && make install
RUN ldconfig
#
#   Configure trafficserver to listen http and https
#
RUN openssl genrsa 2048 > server.key \
 && yes "" | openssl req -new -key server.key > server.csr \
 && openssl x509 -days 3650 -req -signkey server.key < server.csr > server.crt \
 && cp server.crt /usr/local/etc/trafficserver/ \
 && cp server.key /usr/local/etc/trafficserver/
RUN echo "CONFIG proxy.config.http.server_ports STRING 8080 443:ssl" >> /usr/local/etc/trafficserver/records.config
RUN echo "dest_ip=* ssl_cert_name=server.crt ssl_key_name=server.key" >> /usr/local/etc/trafficserver/ssl_multicert.config
#
#   Build and install ts_mruby
#
RUN git clone https://github.com/syucream/ts_mruby.git
RUN cd ts_mruby \
 && git submodule init \
 && git submodule update \
 && autoreconf -if \
 && ./configure --with-ts-prefix-root=/usr/local/ \
 && make build_mruby \
 && make \
 && make install
#
#   Configure ts_mruby test script
#
RUN echo "ATS::echo 'ts_mruby test'" >> /usr/local/etc/trafficserver/echo.rb
RUN echo "map / http://127.0.0.1/ @plugin=ts_mruby.so @pparam=/usr/local/etc/trafficserver/echo.rb" >> /usr/local/etc/trafficserver/remap.config
#
#   Finish preparing!
#
#   You can access to '/' then you'll get ts_mruby's response after '$ trafficserver start'
#   RUN trafficserver start
#   RUN curl -v http://localhost:8080/ #=> 'ts_mruby test'
#
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
