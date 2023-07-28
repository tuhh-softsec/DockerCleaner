FROM ubuntu:16.04 AS base
ENV DEBIAN_FRONTEND="noninteractive" \
    TERM="xterm"
RUN echo "export > /etc/envvars" >> /root/.bashrc \
 && echo "export PS1='\[\e[1;31m\]\u@\h:\w$\[\e[0m\] '" | tee -a /root/.bashrc /etc/skel/.bashrc \
 && echo "alias tcurrent='tail /var/log/*/current -f'" | tee -a /root/.bashrc /etc/skel/.bashrc
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 -y ) \
 && locale-gen en_US.UTF-8 \
 && dpkg-reconfigure locales
ENV LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8"
#   Runit
RUN (apt-get update ;apt-get install --no-install-recommends runit=2.1.2-3ubuntu1 -y )
CMD export > /etc/envvars \
 && /usr/sbin/runsvdir-start
#   Utilities
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:7.4.1689-3ubuntu1.5 less=481-2.1ubuntu0.2 net-tools=1.60-26ubuntu1 inetutils-ping=2:1.9.4-1build1 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 telnet=0.17-40 nmap=7.01-2ubuntu2 socat=1.7.3.1-1 dnsutils=1:9.10.3.dfsg.P4-8ubuntu1.19 netcat=1.10-41 tree=1.7.0-3 htop=2.0.1-1ubuntu1 unzip=6.0-20ubuntu1.1 sudo=1.8.16-0ubuntu1.10 software-properties-common=0.96.20.10 jq=1.5+dfsg-1ubuntu0.1 psmisc=22.21-2.1ubuntu0.1 iproute=1:4.3.0-1ubuntu3.16.04.5 python=2.7.12-1~16.04 ssh=1:7.2p2-4ubuntu2.10 rsync=3.1.1-3ubuntu1.3 gettext-base=0.19.7-2ubuntu3.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libreadline-dev=6.3-8ubuntu2 libncurses5-dev=6.0+20160213-1ubuntu1 libpcre3-dev=2:8.38-3.1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 perl=5.22.1-9ubuntu0.9 make=4.1-6 build-essential=12.1ubuntu2 -y )
#  Confd
RUN wget -O /usr/local/bin/confd https://github.com/kelseyhightower/confd/releases/download/v0.15.0/confd-0.15.0-linux-amd64 \
 && chmod +x /usr/local/bin/confd
#  Redis
RUN wget -O - http://download.redis.io/releases/redis-4.0.8.tar.gz | tar zx \
 && cd redis-* \
 && make -j$( nproc ;) \
 && make install \
 && cp redis.conf /etc/redis.conf \
 && rm -rf /redis-*
#  libmodsecurity
RUN (apt-get update ;apt-get install --no-install-recommends m4=1.4.17-5 libtool=2.4.6-0.1 automake=1:1.15-4ubuntu1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libyajl-dev=2.1.0-2 libgeoip-dev=1.6.9-1 libcurl4-gnutls-dev=7.47.0-1ubuntu2.19 pkgconf=0.9.12-1 -y )
RUN wget -O - https://github.com/SpiderLabs/ModSecurity/releases/download/v3.0.0/modsecurity-v3.0.0.tar.gz | tar zx \
 && cd modsecurity* \
 && ./build.sh \
 && ./configure \
 && make -j$( nproc ;) \
 && make install \
 && rm -rf /modsecurity*
#  OpenResty
RUN wget -O - https://github.com/SpiderLabs/ModSecurity-nginx/releases/download/v1.0.0/modsecurity-nginx-v1.0.0.tar.gz | tar zx \
 && wget -O - https://www.openssl.org/source/openssl-1.0.2n.tar.gz | tar zx \
 && wget -O - https://openresty.org/download/openresty-1.13.6.1.tar.gz | tar zx \
 && cd /openssl* \
 && ./config \
 && make install \
 && mv apps/openssl /usr/bin/ \
 && cd /openresty* \
 && ./configure -j$( grep -c '^processor' /proc/cpuinfo ;) --with-http_v2_module --with-pcre-jit --prefix=/usr/local/openresty --sbin-path=/usr/sbin/nginx --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/var/run/nginx.pid --lock-path=/var/run/nginx.lock --http-client-body-temp-path=/var/cache/nginx/client_temp --http-proxy-temp-path=/var/cache/nginx/proxy_temp --with-file-aio --with-threads --with-stream --with-http_stub_status_module --with-openssl=$( ls -d /openssl* ;) --with-http_sub_module --with-http_realip_module --add-module=/modsecurity-nginx-v1.0.0 \
 && make -j$( nproc ;) \
 && make install \
 && rm -rf /openresty* \
 && rm -rf /openssl* \
 && rm -rf /modsecurity-nginx*
RUN mkdir -p /etc/nginx \
 && mkdir -p /var/log/nginx \
 && mkdir -p /var/cache/nginx/client_temp \
 && mkdir -p /var/cache/nginx/proxy_temp
#  LuaRocks
RUN wget -O - http://luarocks.org/releases/luarocks-2.4.3.tar.gz | tar zx \
 && cd luarocks-* \
 && ./configure --prefix=/usr/local/openresty/luajit --with-lua=/usr/local/openresty/luajit/ --lua-suffix=jit-2.1.0-beta3 --with-lua-include=/usr/local/openresty/luajit/include/luajit-2.1 \
 && make -j$( grep -c '^processor' /proc/cpuinfo ;) \
 && make install \
 && rm -rf /luarocks-*
RUN cd /usr/local/openresty/luajit/bin \
 && ln -s luajit-* lua
ENV PATH="/usr/local/openresty/luajit/bin:$PATH"
#  Lua Libraries
RUN luarocks install lua-resty-session
RUN luarocks install inspect
RUN luarocks install lua-resty-http
RUN luarocks install lua-resty-cookie
#  ssl
RUN openssl dhparam -out /etc/ssl/dhparams.pem 2048
RUN mkdir -p /etc/nginx/ssl \
 && cd /etc/nginx/ssl \
 && export PASSPHRASE=$( head -c 500 /dev/urandom | tr -dc a-z0-9A-Z | head -c 128 ;echo ;) \
 && openssl genrsa -des3 -out server.key -passout env:PASSPHRASE 2048 \
 && openssl req -new -batch -key server.key -out server.csr -subj "/C=/ST=/O=org/localityName=/commonName=org/organizationalUnitName=org/emailAddress=/" -passin env:PASSPHRASE \
 && openssl rsa -in server.key -out server.key -passin env:PASSPHRASE \
 && openssl x509 -req -days 3650 -in server.csr -signkey server.key -out server.crt
#   Force triggering ERROR_PAGE_404 page
RUN rm -rf /usr/local/openresty/nginx/html
#  Passport
RUN wget -O - https://nodejs.org/dist/v8.10.0/node-v8.10.0-linux-x64.tar.gz | tar xz
RUN mv node* node \
 && ln -s /node/bin/node /usr/local/bin/node \
 && ln -s /node/bin/npm /usr/local/bin/npm
ENV NODE_PATH="/usr/local/lib/node_modules"
COPY authenticator /authenticator
RUN cd /authenticator \
 && npm install \
 && npm run build
#  Letsencrypt
RUN luarocks install lua-resty-http \
 && luarocks install lua-resty-auto-ssl
RUN mkdir -p /etc/resty-auto-ssl \
 && chown nobody /etc/resty-auto-ssl
#  OWASP rules
RUN wget -O - https://github.com/SpiderLabs/owasp-modsecurity-crs/archive/v3.0.2.tar.gz | tar zx \
 && mv owasp* /etc/nginx/owasp
RUN cp /etc/nginx/owasp/crs-setup.conf.example /etc/nginx/owasp/owasp.conf
COPY modsec /etc/nginx/modsec
#  logrotate
RUN (apt-get update ;apt-get install --no-install-recommends logrotate=3.8.7-2ubuntu2.16.04.2 cron=3.0pl1-128ubuntu2 -y )
COPY logrotate.conf /etc/logrotate.d/nginx.conf
COPY crontab /
#  Config
COPY nginx.conf /etc/nginx/
COPY etc/confd /etc/confd
COPY test.sh /
COPY redis.conf /etc/
#  SAML
COPY saml/saml.conf /etc/nginx/
COPY saml/saml.lua /usr/local/openresty/lualib/
RUN chmod +r /usr/local/openresty/lualib/*
#   Add runit services
COPY sv /etc/service
ARG BUILD_INFO
LABEL BUILD_INFO="$BUILD_INFO"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
