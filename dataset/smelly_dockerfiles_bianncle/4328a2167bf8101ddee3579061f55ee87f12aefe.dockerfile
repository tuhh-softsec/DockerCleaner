FROM nginx
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends libcurl4-openssl-dev libprotobuf-dev protobuf-compiler --no-install-suggests -y \
 && savedAptMark="$( apt-mark showmanual ;)" \
 && tempDir="$( mktemp -d ;)" \
 && chmod 777 "$tempDir" \
 && apt-get install --no-install-recommends build-essential cmake git ca-certificates pkg-config wget golang libz-dev automake autogen autoconf libtool --no-install-suggests -y \
 && apt-mark showmanual | xargs apt-mark auto > /dev/null \
 && { [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; } \
 && git clone https://github.com/opentracing/opentracing-cpp.git \
 && cd opentracing-cpp \
 && mkdir .build \
 && cd .build \
 && cmake -DCMAKE_BUILD_TYPE=Release .. \
 && make \
 && make install \
 && cd "$tempDir" \
 && apt-get install --no-install-recommends libcurl4-gnutls-dev --no-install-suggests -y \
 && git clone https://github.com/rnburn/zipkin-cpp-opentracing.git \
 && cd zipkin-cpp-opentracing \
 && mkdir .build \
 && cd .build \
 && cmake -DBUILD_SHARED_LIBS=1 -DCMAKE_BUILD_TYPE=Release .. \
 && make \
 && make install \
 && cd "$tempDir" \
 && git clone -b v1.4.x https://github.com/grpc/grpc \
 && cd grpc \
 && git submodule update --init \
 && make HAS_SYSTEM_PROTOBUF=false \
 && make install \
 && make \
 && make install \
 && cd "$tempDir" \
 && git clone https://github.com/lightstep/lightstep-tracer-cpp.git \
 && cd lightstep-tracer-cpp \
 && mkdir .build \
 && cd .build \
 && cmake -DBUILD_SHARED_LIBS=1 -DCMAKE_BUILD_TYPE=Release .. \
 && make \
 && make install \
 && cd "$tempDir" \
 && git clone https://github.com/opentracing-contrib/nginx-opentracing.git \
 && NGINX_VERSION=`nginx -v 2>&1` \
 && NGINX_VERSION=${NGINX_VERSION#*nginx/} \
 && echo "deb-src http://nginx.org/packages/mainline/debian/ stretch nginx" >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get build-dep -y nginx=${NGINX_VERSION} \
 && wget -O nginx-release-${NGINX_VERSION}.tar.gz https://github.com/nginx/nginx/archive/release-${NGINX_VERSION}.tar.gz \
 && tar zxf nginx-release-${NGINX_VERSION}.tar.gz \
 && cd nginx-release-${NGINX_VERSION} \
 && NGINX_MODULES_PATH=$( nginx -V 2>&1 | grep -oP "modules-path=\K[^\s]*" ;) \
 && auto/configure --with-compat --add-dynamic-module=${tempDir}/nginx-opentracing/opentracing --add-dynamic-module=${tempDir}/nginx-opentracing/zipkin --add-dynamic-module=${tempDir}/nginx-opentracing/lightstep \
 && make modules \
 && cp objs/ngx_http_opentracing_module.so $NGINX_MODULES_PATH/ \
 && cp objs/ngx_http_zipkin_module.so $NGINX_MODULES_PATH/ \
 && cp objs/ngx_http_lightstep_module.so $NGINX_MODULES_PATH/ \
 && if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove \
 && rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi
#  forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
EXPOSE 80/tcp
STOPSIGNAL SIGTERM
CMD ["nginx", "-g", "daemon", "off"]
