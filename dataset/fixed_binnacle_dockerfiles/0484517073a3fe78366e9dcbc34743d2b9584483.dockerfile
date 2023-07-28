FROM alpine:3.4
MAINTAINER ivan@lagunovsky.com
ENV NGINX_VERSION="1.13.4" \
    PAGESPEED_VERSION="1.11.33.4" \
    LIBPNG_VERSION="1.2.56" \
    MAKE_J="4" \
    PAGESPEED_ENABLE="on"
RUN apk upgrade --no-cache --update \
 && apk add bash=4.3.42-r5 ca-certificates=20161130-r0 libuuid=2.28-r3 apr=1.5.2-r0 apr-util=1.5.4-r1 libjpeg-turbo=1.4.2-r0 icu=57.1-r3 icu-libs=57.1-r3 openssl=1.0.2n-r0 pcre=8.38-r1 zlib=1.2.11-r0 --no-cache --update
RUN set -x \
 && apk add apache2-dev=2.4.33-r0 apr-dev=1.5.2-r0 apr-util-dev=1.5.4-r1 build-base=0.4-r1 curl=7.60.0-r1 icu-dev=57.1-r3 libjpeg-turbo-dev=1.4.2-r0 linux-headers=4.4.6-r1 gperf=3.0.4-r3 openssl-dev=1.0.2n-r0 pcre-dev=8.38-r1 python=2.7.14-r0 zlib-dev=1.2.11-r0 --no-cache -t .build-deps \
 && addgroup -g 1000 -S nginx \
 && adduser -u 1000 -D -S -h /var/cache/ngx_pagespeed -s /sbin/nologin -G nginx nginx \
 && cd /tmp \
 && curl -L http://prdownloads.sourceforge.net/libpng/libpng-${LIBPNG_VERSION}.tar.gz | tar -zx \
 && cd /tmp/libpng-${LIBPNG_VERSION} \
 && ./configure --build=$CBUILD --host=$CHOST --prefix=/usr --enable-shared --with-libpng-compat \
 && make -j${MAKE_J} install V=0 \
 && cd /tmp \
 && curl -L https://dl.google.com/dl/linux/mod-pagespeed/tar/beta/mod-pagespeed-beta-${PAGESPEED_VERSION}-r0.tar.bz2 | tar -jx \
 && curl -L https://github.com/pagespeed/ngx_pagespeed/archive/v${PAGESPEED_VERSION}-beta.tar.gz | tar -zx \
 && cd /tmp/modpagespeed-${PAGESPEED_VERSION} \
 && curl -L https://raw.githubusercontent.com/lagun4ik/docker-nginx-pagespeed/master/patches/automatic_makefile.patch | patch -p1 \
 && curl -L https://raw.githubusercontent.com/lagun4ik/docker-nginx-pagespeed/master/patches/libpng_cflags.patch | patch -p1 \
 && curl -L https://raw.githubusercontent.com/lagun4ik/docker-nginx-pagespeed/master/patches/pthread_nonrecursive_np.patch | patch -p1 \
 && curl -L https://raw.githubusercontent.com/lagun4ik/docker-nginx-pagespeed/master/patches/rename_c_symbols.patch | patch -p1 \
 && curl -L https://raw.githubusercontent.com/lagun4ik/docker-nginx-pagespeed/master/patches/stack_trace_posix.patch | patch -p1 \
 && ./generate.sh -D use_system_libs=1 -D _GLIBCXX_USE_CXX11_ABI=0 -D use_system_icu=1 \
 && cd /tmp/modpagespeed-${PAGESPEED_VERSION}/src \
 && make -j${MAKE_J} BUILDTYPE=Release CXXFLAGS=" -I/usr/include/apr-1 -I/tmp/libpng-${LIBPNG_VERSION} -fPIC -D_GLIBCXX_USE_CXX11_ABI=0" CFLAGS=" -I/usr/include/apr-1 -I/tmp/libpng-${LIBPNG_VERSION} -fPIC -D_GLIBCXX_USE_CXX11_ABI=0" \
 && cd /tmp/modpagespeed-${PAGESPEED_VERSION}/src/pagespeed/automatic/ \
 && make -j${MAKE_J} psol BUILDTYPE=Release CXXFLAGS=" -I/usr/include/apr-1 -I/tmp/libpng-${LIBPNG_VERSION} -fPIC -D_GLIBCXX_USE_CXX11_ABI=0" CFLAGS=" -I/usr/include/apr-1 -I/tmp/libpng-${LIBPNG_VERSION} -fPIC -D_GLIBCXX_USE_CXX11_ABI=0" \
 && mkdir -p /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol \
 && mkdir -p /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/lib/Release/linux/x64 \
 && mkdir -p /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/out/Release \
 && cp -r /tmp/modpagespeed-${PAGESPEED_VERSION}/src/out/Release/obj /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/out/Release/ \
 && cp -r /tmp/modpagespeed-${PAGESPEED_VERSION}/src/net /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/modpagespeed-${PAGESPEED_VERSION}/src/testing /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/modpagespeed-${PAGESPEED_VERSION}/src/pagespeed /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/modpagespeed-${PAGESPEED_VERSION}/src/third_party /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/modpagespeed-${PAGESPEED_VERSION}/src/tools /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/modpagespeed-${PAGESPEED_VERSION}/src/url /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/modpagespeed-${PAGESPEED_VERSION}/src/pagespeed/automatic/pagespeed_automatic.a /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/lib/Release/linux/x64 \
 && cd /tmp \
 && curl -L http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz | tar -zx \
 && cd /tmp/nginx-${NGINX_VERSION} \
 && LD_LIBRARY_PATH=/tmp/modpagespeed-${PAGESPEED_VERSION}/usr/lib ./configure --sbin-path=/usr/sbin --modules-path=/usr/lib/nginx --with-http_ssl_module --with-http_gzip_static_module --with-file-aio --with-http_v2_module --with-http_realip_module --without-http_autoindex_module --without-http_browser_module --without-http_geo_module --without-http_memcached_module --without-http_userid_module --without-mail_pop3_module --without-mail_imap_module --without-mail_smtp_module --without-http_split_clients_module --without-http_uwsgi_module --without-http_scgi_module --without-http_upstream_ip_hash_module --with-http_sub_module --with-http_gunzip_module --with-http_secure_link_module --with-threads --with-stream --with-stream_ssl_module --prefix=/etc/nginx --conf-path=/etc/nginx/nginx.conf --http-log-path=/var/log/nginx/access.log --error-log-path=/var/log/nginx/error.log --pid-path=/var/run/nginx.pid --add-module=/tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta --with-cc-opt="-fPIC -I /usr/include/apr-1" --with-ld-opt="-luuid -lapr-1 -laprutil-1 -licudata -licuuc -L/tmp/modpagespeed-${PAGESPEED_VERSION}/usr/lib -lpng12 -lturbojpeg -ljpeg" \
 && make -j${MAKE_J} install --silent \
 && cd \
 && apk del .build-deps \
 && rm -rf /tmp/* \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
COPY config/conf.d /etc/nginx/conf.d
COPY config/nginx.conf /etc/nginx/nginx.conf
COPY docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh
VOLUME ["/var/cache/ngx_pagespeed"]
EXPOSE 80/tcp 443/tcp
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["nginx", "-g", "daemon", "off"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
