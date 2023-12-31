ARG NGINX_LABEL=latest
FROM nginx:${NGINX_LABEL}
ARG OPENTRACING_CPP_VERSION=v1.5.1
ARG ZIPKIN_CPP_VERSION=v0.5.2
ARG LIGHTSTEP_VERSION=v0.8.1
ARG JAEGER_CPP_VERSION=v0.4.2
ARG GRPC_VERSION=v1.10.x
ARG DATADOG_VERSION=v0.3.0
COPY . /src
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends libcurl4-openssl-dev libprotobuf-dev protobuf-compiler --no-install-suggests -y \
 && savedAptMark="$( apt-mark showmanual ;)" \
 && tempDir="$( mktemp -d ;)" \
 && chmod 777 "$tempDir" \
 && apt-get install --no-install-recommends build-essential cmake git ca-certificates pkg-config wget golang libz-dev automake autogen autoconf libtool --no-install-suggests -y \
 && apt-mark showmanual | xargs apt-mark auto > /dev/null \
 && { [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; } \
 && cd "$tempDir" \
 && git clone -b $OPENTRACING_CPP_VERSION https://github.com/opentracing/opentracing-cpp.git \
 && cd opentracing-cpp \
 && mkdir .build \
 && cd .build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=OFF .. \
 && make \
 && make install \
 && cd "$tempDir" \
 && apt-get install --no-install-recommends libcurl4-gnutls-dev --no-install-suggests -y \
 && git clone -b $ZIPKIN_CPP_VERSION https://github.com/rnburn/zipkin-cpp-opentracing.git \
 && cd zipkin-cpp-opentracing \
 && mkdir .build \
 && cd .build \
 && cmake -DBUILD_SHARED_LIBS=1 -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=OFF .. \
 && make \
 && make install \
 && cd "$tempDir" \
 && ln -s /usr/local/lib/libzipkin_opentracing.so /usr/local/lib/libzipkin_opentracing_plugin.so \
 && git clone -b $JAEGER_CPP_VERSION https://github.com/jaegertracing/cpp-client.git jaeger-cpp-client \
 && cd jaeger-cpp-client \
 && mkdir .build \
 && cd .build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=OFF -DJAEGERTRACING_WITH_YAML_CPP=ON .. \
 && make \
 && make install \
 && export HUNTER_INSTALL_DIR=$( cat _3rdParty/Hunter/install-root-dir ;) \
 && cd "$tempDir" \
 && ln -s /usr/local/lib/libjaegertracing.so /usr/local/lib/libjaegertracing_plugin.so \
 && git clone -b $DATADOG_VERSION https://github.com/DataDog/dd-opentracing-cpp.git \
 && cd dd-opentracing-cpp \
 && scripts/install_dependencies.sh not-opentracing not-curl not-zlib \
 && mkdir .build \
 && cd .build \
 && cmake -DBUILD_SHARED_LIBS=1 -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=OFF .. \
 && make \
 && make install \
 && cd "$tempDir" \
 && ln -s /usr/local/lib/libdd_opentracing.so /usr/local/lib/libdd_opentracing_plugin.so \
 && git clone -b $GRPC_VERSION https://github.com/grpc/grpc \
 && cd grpc \
 && git submodule update --init \
 && make HAS_SYSTEM_PROTOBUF=false \
 && make install \
 && make \
 && make install \
 && cd third_party/protobuf \
 && make install \
 && cd "$tempDir" \
 && git clone -b $LIGHTSTEP_VERSION https://github.com/lightstep/lightstep-tracer-cpp.git \
 && cd lightstep-tracer-cpp \
 && mkdir .build \
 && cd .build \
 && cmake -DBUILD_SHARED_LIBS=1 -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=OFF .. \
 && make \
 && make install \
 && cd "$tempDir" \
 && ln -s /usr/local/lib/liblightstep_tracer.so /usr/local/lib/liblightstep_tracer_plugin.so \
 && NGINX_VERSION=`nginx -v 2>&1` \
 && NGINX_VERSION=${NGINX_VERSION#*nginx/} \
 && echo "deb-src http://nginx.org/packages/mainline/debian/ stretch nginx" >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get build-dep -y nginx=${NGINX_VERSION} \
 && wget -O nginx-release-${NGINX_VERSION}.tar.gz https://github.com/nginx/nginx/archive/release-${NGINX_VERSION}.tar.gz \
 && tar zxf nginx-release-${NGINX_VERSION}.tar.gz \
 && cd nginx-release-${NGINX_VERSION} \
 && NGINX_MODULES_PATH=$( nginx -V 2>&1 | grep -oP "modules-path=\K[^\s]*" ;) \
 && auto/configure --with-compat --add-dynamic-module=/src/opentracing --with-cc-opt="-I$HUNTER_INSTALL_DIR/include" --with-ld-opt="-L$HUNTER_INSTALL_DIR/lib" --with-debug \
 && make modules \
 && cp objs/ngx_http_opentracing_module.so $NGINX_MODULES_PATH/ \
 && rm -rf /src \
 && rm -rf $HOME/.hunter \
 && if [ -n "$tempDir" ] ; then apt-get purge -y --auto-remove \
 && rm -rf "$tempDir" /etc/apt/sources.list.d/temp.list ; fi
#  forward request and error logs to docker log collector
RUN ln -sf /dev/stdout /var/log/nginx/access.log \
 && ln -sf /dev/stderr /var/log/nginx/error.log
EXPOSE 80/tcp
STOPSIGNAL SIGTERM
CMD ["nginx", "-g", "daemon", "off"]
