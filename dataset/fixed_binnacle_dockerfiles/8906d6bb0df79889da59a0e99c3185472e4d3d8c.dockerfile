FROM ubuntu:14.04
MAINTAINER John Billings <billings@yelp.com>
#   Need Python 3.6
RUN apt-get update > /dev/null \
 && apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y \
 && add-apt-repository ppa:deadsnakes/ppa
RUN apt-get update \
 && apt-get install --no-install-recommends libcurl3=7.35.0-1ubuntu2.20 iptables=1.4.21-1ubuntu1 python3.6 python-setuptools=3.3-1ubuntu2 python-pytest=2.5.1-1 python-pycurl=7.19.3-0ubuntu3 python-kazoo=1.2.1-1ubuntu4 python-zope.interface=4.0.5-1ubuntu4 ruby1.9.1=1.9.3.484-2ubuntu1.14 ruby1.9.1-dev=1.9.3.484-2ubuntu1.14 rubygems1.9.1 libxml2=2.9.1+dfsg1-3ubuntu4.13 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt-dev libssl-dev=1.0.1f-1ubuntu2.27 build-essential=11.6ubuntu6 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 -y
#   Lua 5.3 for scripting with HAProxy
WORKDIR /
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /lua-5.3.1.tar.gz http://www.lua.org/ftp/lua-5.3.1.tar.gz
RUN tar -axvf /lua-5.3.1.tar.gz
WORKDIR /lua-5.3.1
RUN pwd
RUN make generic
RUN make install INSTALL_TOP=/usr/bin/lua
#   Ubuntu trusty nginx and haproxy are ancient, grab newer ones
WORKDIR /
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /haproxy.tar.gz http://www.haproxy.org/download/1.7/src/haproxy-1.7.8.tar.gz
RUN tar -axvf /haproxy.tar.gz
WORKDIR /haproxy-1.7.8
RUN pwd
RUN make TARGET=linux26 -j 4 USE_LUA=1 LUA_LIB=/usr/bin/lua/lib LUA_INC=/usr/bin/lua/include \
 && mv haproxy /usr/bin/haproxy-synapse
#   Nginx
WORKDIR /
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /nginx.tar.gz https://nginx.org/download/nginx-1.11.10.tar.gz
RUN tar -axvf /nginx.tar.gz
WORKDIR /nginx-1.11.10
RUN ./configure --prefix=/etc/nginx --sbin-path=/usr/sbin/nginx --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --user=nobody --group=nogroup --with-http_ssl_module --with-stream --without-http_rewrite_module --without-http_gzip_module
RUN make -j 4
RUN make install
#   Pin for test reproducibility
RUN gem install nokogiri --version 1.6.7.2 --no-ri --no-rdoc
RUN gem install synapse --version 0.14.1 --no-ri --no-rdoc
RUN gem install synapse-nginx --version 0.2.2 --no-ri --no-rdoc
COPY synapse.conf /etc/init/synapse.conf
COPY synapse.conf.json /etc/synapse/synapse.conf.json
COPY synapse-tools.conf.json /etc/synapse/synapse-tools.conf.json
COPY synapse-tools-both.conf.json /etc/synapse/synapse-tools-both.conf.json
COPY synapse-tools-nginx.conf.json /etc/synapse/synapse-tools-nginx.conf.json
#   Zookeeper discovery
RUN mkdir -p /nail/etc/zookeeper_discovery/infrastructure
COPY zookeeper_discovery/infrastructure/local.yaml.trusty /nail/etc/zookeeper_discovery/infrastructure/local.yaml
COPY yelpsoa-configs /nail/etc/services
COPY habitat /nail/etc/habitat
COPY ecosystem /nail/etc/ecosystem
COPY region /nail/etc/region
COPY itest.py /itest.py
COPY run_itest.sh /run_itest.sh
COPY rsyslog-configs/49-haproxy.conf /etc/rsyslog.d/49-haproxy.conf
COPY maps/ip_to_service.map /var/run/synapse/maps/ip_to_service.map
#   configure_synapse tries to restart synapse.
#   make it think it succeeded.
RUN ln -sf /bin/true /usr/sbin/service
CMD /bin/bash /run_itest.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
