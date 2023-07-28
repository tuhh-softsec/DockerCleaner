FROM ubuntu:18.04 AS modsecurity-build
LABEL maintainer="krish512 <krish512@hotmail.com>"
#   Install Prereqs
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 automake=1:1.15.1-3ubuntu2 autoconf=2.69-11 build-essential=12.4ubuntu1 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libpcre++-dev=0.9.5-6.1 libtool=2.4.6-2 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libyajl-dev=2.1.0-2build1 lua5.2-dev git=1:2.17.1-1ubuntu0.17 pkgconf=0.9.12-6 ssdeep=2.14-1 libgeoip-dev=1.6.12-1 wget=1.19.4-1ubuntu2.2 -qq -y --no-install-suggests \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN cd /opt \
 && git clone --depth 1 -b v3/master --single-branch https://github.com/SpiderLabs/ModSecurity \
 && cd ModSecurity \
 && git submodule init \
 && git submodule update \
 && ./build.sh \
 && ./configure \
 && make \
 && make install
RUN strip /usr/local/modsecurity/bin/* /usr/local/modsecurity/lib/*.a /usr/local/modsecurity/lib/*.so*
FROM ubuntu:18.04 AS nginx-build
ENV DEBIAN_FRONTEND="noninteractive"
ENV NGINX_VERSION="1.15.0"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 build-essential=12.4ubuntu1 libtool=2.4.6-2 pkgconf=0.9.12-6 wget=1.19.4-1ubuntu2.2 git=1:2.17.1-1ubuntu0.17 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libpcre3-dev=2:8.39-9ubuntu0.1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libyajl-dev=2.1.0-2build1 lua5.2-dev libgeoip-dev=1.6.12-1 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 openssl=1.1.1-1ubuntu2.1~18.04.21 -qq -y --no-install-suggests
RUN cd /opt \
 && git clone --depth 1 https://github.com/SpiderLabs/ModSecurity-nginx.git
RUN cd /opt \
 && git clone --recursive https://github.com/google/ngx_brotli.git
COPY --from=modsecurity-build /usr/local/modsecurity/ /usr/local/modsecurity/
RUN wget -q -P /opt https://nginx.org/download/nginx-"$NGINX_VERSION".tar.gz
RUN tar xvzf /opt/nginx-"$NGINX_VERSION".tar.gz -C /opt
RUN cd /opt/nginx-"$NGINX_VERSION" \
 && ./configure --prefix=/usr/local/nginx --sbin-path=/usr/local/nginx/nginx --modules-path=/usr/local/nginx/modules --conf-path=/etc/nginx/nginx.conf --error-log-path=/var/log/nginx/error.log --http-log-path=/var/log/nginx/access.log --pid-path=/run/nginx.pid --lock-path=/var/lock/nginx.lock --user=www-data --group=www-data --with-pcre-jit --with-file-aio --with-threads --with-http_addition_module --with-http_auth_request_module --with-http_flv_module --with-http_gunzip_module --with-http_gzip_static_module --with-http_mp4_module --with-http_random_index_module --with-http_realip_module --with-http_slice_module --with-http_ssl_module --with-http_sub_module --with-http_stub_status_module --with-http_v2_module --with-http_secure_link_module --with-stream --with-stream_realip_module --add-module=/opt/ModSecurity-nginx --add-module=/opt/ngx_brotli --with-cc-opt='-g -O2 -specs=/usr/share/dpkg/no-pie-compile.specs -fstack-protector-strong -Wformat -Werror=format-security -Wp,-D_FORTIFY_SOURCE=2 -fPIC' --with-ld-opt='-specs=/usr/share/dpkg/no-pie-link.specs -Wl,-z,relro -Wl,-z,now -Wl,--as-needed -pie' --with-http_dav_module
RUN cd /opt/nginx-"$NGINX_VERSION" \
 && make \
 && make install \
 && make modules
RUN mkdir -p /var/log/nginx/
RUN touch /var/log/nginx/access.log
RUN touch /var/log/nginx/error.log
EXPOSE 80/tcp
STOPSIGNAL SIGTERM
CMD ["/usr/local/nginx/nginx", "-g", "daemon", "off"]
FROM ubuntu:18.04
ENV DEBIAN_FRONTEND="noninteractive"
#   Libraries for ModSecurity
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libyajl-dev=2.1.0-2build1 lua5.2-dev libgeoip-dev=1.6.12-1 vim=2:8.0.1453-1ubuntu1.11 libxml2=2.9.4+dfsg1-6.1ubuntu1.8 --no-install-suggests -y
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/*
COPY --from=modsecurity-build /usr/local/modsecurity/ /usr/local/modsecurity/
RUN ldconfig
COPY --from=nginx-build /usr/local/nginx/nginx /usr/local/nginx/nginx
COPY --from=nginx-build /etc/nginx /etc/nginx
COPY --from=nginx-build /usr/local/nginx/html /usr/local/nginx/html
#   NGiNX Create log dirs
RUN mkdir -p /var/log/nginx/
RUN touch /var/log/nginx/access.log
RUN touch /var/log/nginx/error.log
RUN sed -i '38i modsecurity on;\n\tmodsecurity_rules_file /etc/nginx/modsecurity.d/include.conf;' /etc/nginx/nginx.conf
RUN mkdir -p /etc/nginx/modsecurity.d
RUN echo "include /etc/nginx/modsecurity.d/modsecurity.conf" > /etc/nginx/modsecurity.d/include.conf
COPY --from=modsecurity-build /opt/ModSecurity/modsecurity.conf-recommended /etc/nginx/modsecurity.d
RUN cd /etc/nginx/modsecurity.d \
 && mv modsecurity.conf-recommended modsecurity.conf
#  # Version for ModSecurity Core Rule Set
ARG VERSION=3.0.2
#  # Install Curl
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 -y \
 && apt-get clean
#  # Get ModSecurity CRS
RUN curl -s https://codeload.github.com/SpiderLabs/owasp-modsecurity-crs/tar.gz/v${VERSION} --output ~/modsec.tar.gz
RUN tar -xzf ~/modsec.tar.gz -C /etc/nginx
RUN rm ~/modsec.tar.gz
#  # Install ModSecurity CRS
RUN cat /etc/nginx/owasp-modsecurity-crs-${VERSION}/crs-setup.conf.example /etc/nginx/owasp-modsecurity-crs-${VERSION}/rules/*.conf >> /etc/nginx/modsecurity.d/crs.conf
RUN cp /etc/nginx/owasp-modsecurity-crs-${VERSION}/rules/*.data /etc/nginx/modsecurity.d/
RUN rm -rf /etc/nginx/owasp-modsecurity-crs-*
RUN echo "include /etc/nginx/modsecurity.d/crs.conf" >> /etc/nginx/modsecurity.d/include.conf
RUN sed -i -e 's/SecRuleEngine DetectionOnly/SecRuleEngine On/g' /etc/nginx/modsecurity.d/modsecurity.conf
#  # Update nginx config
COPY nginx /etc/nginx/
EXPOSE 80/tcp
STOPSIGNAL SIGTERM
CMD ["/usr/local/nginx/nginx", "-g", "daemon", "off"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
