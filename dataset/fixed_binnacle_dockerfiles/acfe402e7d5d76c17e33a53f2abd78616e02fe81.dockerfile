#   wunder/fuzzy-alpine-nginx-pagespeed
#
#   VERSION v1.11.5-3
#
FROM quay.io/wunder/fuzzy-alpine-base:v3.4
MAINTAINER aleksi.johansson@wunder.io
#   Based on https://github.com/pagespeed/ngx_pagespeed/issues/1181#issuecomment-250776751
#   for Alpine related fixes and official instructions from:
#   - https://developers.google.com/speed/pagespeed/module/build_mod_pagespeed_from_source
#   - https://developers.google.com/speed/pagespeed/module/build_ngx_pagespeed_from_source
RUN apk add ca-certificates=20161130-r0 libuuid=2.28-r3 apr=1.5.2-r0 apr-util=1.5.4-r1 libjpeg-turbo=1.4.2-r0 icu=57.1-r3 icu-libs=57.1-r3 openssl=1.0.2n-r0 pcre=8.38-r1 zlib=1.2.11-r0 --no-cache
RUN set -x \
 && apk add apache2-dev=2.4.33-r0 apr-dev=1.5.2-r0 apr-util-dev=1.5.4-r1 build-base=0.4-r1 curl=7.60.0-r1 icu-dev=57.1-r3 libjpeg-turbo-dev=1.4.2-r0 linux-headers=4.4.6-r1 gperf=3.0.4-r3 openssl-dev=1.0.2n-r0 pcre-dev=8.38-r1 python=2.7.14-r0 zlib-dev=1.2.11-r0 g++=5.3.0-r0 subversion=1.9.7-r0 fakeroot=1.20.2-r3 git=2.8.6-r0 --no-cache -t .build-deps \
 && LIBPNG_VERSION=1.2.56 \
 && cd /tmp \
 && curl -L http://prdownloads.sourceforge.net/libpng/libpng-${LIBPNG_VERSION}.tar.gz | tar -zx \
 && cd /tmp/libpng-${LIBPNG_VERSION} \
 && ./configure --build=$CBUILD --host=$CHOST --prefix=/usr --enable-shared --with-libpng-compat \
 && make install V=0 \
 && cd /tmp \
 && git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git \
 && export PATH=$PATH:/tmp/depot_tools \
 && PAGESPEED_VERSION=1.11.33.4 \
 && mkdir -p /tmp/mod_pagespeed-${PAGESPEED_VERSION} \
 && cd /tmp/mod_pagespeed-${PAGESPEED_VERSION} \
 && gclient config https://github.com/pagespeed/mod_pagespeed.git --unmanaged --name=src \
 && git clone https://github.com/pagespeed/mod_pagespeed.git src \
 && cd /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src \
 && git checkout ${PAGESPEED_VERSION} \
 && cd /tmp/mod_pagespeed-${PAGESPEED_VERSION} \
 && gclient sync --force --jobs=1 \
 && cd /tmp \
 && curl -L https://github.com/pagespeed/ngx_pagespeed/archive/v${PAGESPEED_VERSION}-beta.tar.gz | tar -zx \
 && cd /tmp/mod_pagespeed-${PAGESPEED_VERSION} \
 && curl -L https://raw.githubusercontent.com/wunderkraut/alpine-nginx-pagespeed/master/patches/automatic_makefile.patch | patch -p1 \
 && curl -L https://raw.githubusercontent.com/wunderkraut/alpine-nginx-pagespeed/master/patches/libpng_cflags.patch | patch -p1 \
 && curl -L https://raw.githubusercontent.com/wunderkraut/alpine-nginx-pagespeed/master/patches/pthread_nonrecursive_np.patch | patch -p1 \
 && curl -L https://raw.githubusercontent.com/wunderkraut/alpine-nginx-pagespeed/master/patches/rename_c_symbols.patch | patch -p1 \
 && curl -L https://raw.githubusercontent.com/wunderkraut/alpine-nginx-pagespeed/master/patches/stack_trace_posix.patch | patch -p1 \
 && src/build/gyp_chromium -D use_system_libs=1 -D _GLIBCXX_USE_CXX11_ABI=0 -D use_system_icu=1 \
 && cd /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src \
 && make BUILDTYPE=Release CXXFLAGS=" -I/usr/include/apr-1 -I/tmp/libpng-${LIBPNG_VERSION} -fPIC -D_GLIBCXX_USE_CXX11_ABI=0" CFLAGS=" -I/usr/include/apr-1 -I/tmp/libpng-${LIBPNG_VERSION} -fPIC -D_GLIBCXX_USE_CXX11_ABI=0" \
 && cd /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/pagespeed/automatic/ \
 && make psol BUILDTYPE=Release CXXFLAGS=" -I/usr/include/apr-1 -I/tmp/libpng-${LIBPNG_VERSION} -fPIC -D_GLIBCXX_USE_CXX11_ABI=0" CFLAGS=" -I/usr/include/apr-1 -I/tmp/libpng-${LIBPNG_VERSION} -fPIC -D_GLIBCXX_USE_CXX11_ABI=0" \
 && mkdir -p /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol \
 && mkdir -p /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/lib/Release/linux/x64 \
 && mkdir -p /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/out/Release \
 && cp -r /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/out/Release/obj /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/out/Release/ \
 && cp -r /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/net /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/testing /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/pagespeed /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/third_party /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/tools /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/url /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/include/ \
 && cp -r /tmp/mod_pagespeed-${PAGESPEED_VERSION}/src/pagespeed/automatic/pagespeed_automatic.a /tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta/psol/lib/Release/linux/x64 \
 && NGINX_VERSION=1.11.5 \
 && cd /tmp \
 && curl -L http://nginx.org/download/nginx-${NGINX_VERSION}.tar.gz | tar -zx \
 && cd /tmp/nginx-${NGINX_VERSION} \
 && LD_LIBRARY_PATH=/tmp/mod_pagespeed-${PAGESPEED_VERSION}/usr/lib ./configure --sbin-path=/usr/sbin --modules-path=/usr/lib/nginx --with-http_ssl_module --with-http_gzip_static_module --with-file-aio --with-http_v2_module --without-http_autoindex_module --without-http_browser_module --without-http_geo_module --without-http_map_module --without-http_memcached_module --without-http_userid_module --without-mail_pop3_module --without-mail_imap_module --without-mail_smtp_module --without-http_split_clients_module --without-http_scgi_module --without-http_referer_module --without-http_upstream_ip_hash_module --prefix=/etc/nginx --conf-path=/etc/nginx/nginx.conf --http-log-path=/var/log/nginx/access.log --error-log-path=/var/log/nginx/error.log --pid-path=/var/run/nginx.pid --add-module=/tmp/ngx_pagespeed-${PAGESPEED_VERSION}-beta --with-cc-opt="-fPIC -I /usr/include/apr-1" --with-ld-opt="-luuid -lapr-1 -laprutil-1 -licudata -licuuc -L/tmp/mod_pagespeed-${PAGESPEED_VERSION}/usr/lib -lpng12 -lturbojpeg -ljpeg" \
 && make install --silent \
 && cd \
 && apk del .build-deps \
 && rm -rf /tmp/* \
 && ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log \
 && mkdir -p /var/cache/ngx_pagespeed \
 && chmod -R o+wr /var/cache/ngx_pagespeed
#   Make our nginx.conf available on the container.
COPY etc/nginx/nginx.conf /etc/nginx/nginx.conf
#   Separate the logs into their own volume to keep them out of the container.
VOLUME ["/var/log/nginx"]
#   Expose the HTTP and HTTPS ports.
EXPOSE 80/tcp 443/tcp
#   Set nginx directly as the entrypoint.
ENTRYPOINT ["nginx", "-g", "daemon", "off"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
