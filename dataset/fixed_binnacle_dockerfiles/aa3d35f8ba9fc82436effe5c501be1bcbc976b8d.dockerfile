FROM debian:stretch
ENV LANG="C.UTF-8"
ENV LANGUAGE="C.UTF-8"
ENV LC_ALL="C.UTF-8"
RUN apt-get update \
 && apt-get install --no-install-recommends cmake=3.7.2-1 git=1:2.11.0-3+deb9u7 build-essential=12.3 curl=7.52.1-5+deb9u16 libboost-all-dev=1.62.0.1 libpcre3-dev=2:8.39-3 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 libgcrypt11-dev=1.5.4-3+really1.7.6-2+deb9u4 libicu-dev=57.1-6+deb9u5 python=2.7.13-2 libssl-dev=1.1.0l-1~deb9u6 libyajl-dev=2.1.0-2+b3 autoconf=2.69-10 automake=1:1.15-6 pkg-config=0.29-4+b1 net-tools=1.60+git20161116.90da8a0-1 -y \
 && rm -rf /var/lib/apt/lists/*
#   Google Test
ENV GTEST_ROOT="/opt/gtest"
ARG GTEST_VER=release-1.8.1
RUN mkdir -p ${GTEST_ROOT} \
 && cd /tmp \
 && curl -o gtest.tar.gz -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
 && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
 && rm gtest.tar.gz
ARG PARALLEL=2
WORKDIR /app/deps
ARG CPPCMS_VERSION="1.2.0"
RUN curl -o cppcms-${CPPCMS_VERSION}.tar.bz -L "https://sourceforge.net/projects/cppcms/files/cppcms/${CPPCMS_VERSION}/cppcms-${CPPCMS_VERSION}.tar.bz2/download" \
 && tar -xjvf cppcms-${CPPCMS_VERSION}.tar.bz \
 && mkdir cppcms-${CPPCMS_VERSION}/build \
 && cd cppcms-${CPPCMS_VERSION}/build \
 && cmake .. \
 && make -j ${PARALLEL} \
 && make install \
 && cd /app/deps \
 && rm -Rf cppcms-${CPPCMS_VERSION}
ARG JANSSON_VERSION="2.11"
RUN curl -O http://www.digip.org/jansson/releases/jansson-${JANSSON_VERSION}.tar.gz \
 && tar -xvzf jansson-${JANSSON_VERSION}.tar.gz \
 && cd jansson-${JANSSON_VERSION} \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j ${PARALLEL} \
 && make check \
 && make install \
 && cd /app/deps \
 && rm -Rf jansson-${JANSSON_VERSION}
ARG LIBJWT_VERSION="1.9.0"
RUN curl -o libjwt-v${LIBJWT_VERSION}.tar.gz -L "https://github.com/benmcollins/libjwt/archive/v${LIBJWT_VERSION}.tar.gz" \
 && tar -xvzf libjwt-v${LIBJWT_VERSION}.tar.gz \
 && cd libjwt-${LIBJWT_VERSION} \
 && autoreconf -i \
 && ./configure \
 && make -j ${PARALLEL} all \
 && make install \
 && cd /app/deps \
 && rm -Rf libjwt-${LIBJWT_VERSION}
#   start build of kdb
ENV C_FLAGS="-D_FORTIFY_SOURCE=2 -Wformat -Werror=format-security -fstack-protector-strong -Wstack-protector -fPIE -pie"
WORKDIR /app/kdb
COPY . /app/kdb/
RUN ldconfig \
 && mkdir build \
 && cd build \
 && cmake -DENABLE_ASAN=ON -DBUILD_FULL=OFF -DBUILD_SHARED=ON -DBUILD_STATIC=OFF -DBUILD_DOCUMENTATION=OFF -DINSTALL_SYSTEM_FILES=OFF -DPLUGINS="ALL;-EXPERIMENTAL;-fstab;-ruby;-lua;-python;-xerces;-yamlcpp;-python2;file;camel;yajl" -DTOOLS="kdb;rest-backend" -DCMAKE_C_FLAGS="$C_FLAGS" -DCMAKE_CXX_FLAGS="$C_FLAGS" -DCMAKE_EXE_LINKER_FLAGS="-Wl,-z,now -Wl,-z,relro" .. \
 && make -j ${PARALLEL} \
 && ctest -T Test --output-on-failure -j ${PARALLEL} -LE kdbtests \
 && make install
FROM debian:stretch
ENV LANG="C.UTF-8"
ENV LANGUAGE="C.UTF-8"
ENV LC_ALL="C.UTF-8"
COPY --from=0 /usr/local /usr/local
RUN echo '/usr/local/lib/elektra/' > /etc/ld.so.conf.d/elektra.conf \
 && ldconfig
RUN apt-get update \
 && apt-get install --no-install-recommends libasan3=6.3.0-18+deb9u1 libubsan0=6.3.0-18+deb9u1 libboost-system1.62.0=1.62.0+dfsg-4 libboost-filesystem1.62.0=1.62.0+dfsg-4 libboost-thread1.62.0=1.62.0+dfsg-4 libssl1.1=1.1.0l-1~deb9u6 libicu57=57.1-6+deb9u5 libyajl2=2.1.0-2+b3 pwgen=2.07-1.1+b1 -y \
 && rm -rf /var/lib/apt/lists/*
#   prepare
#   asan errors in mount-rest-backend-config and run-rest-backend
RUN kdb global-mount \
 && kdb mount-rest-backend-config || /bin/true \
 && kdb set -N system /sw/elektra/restbackend/#0/current/backend/jwt/encryption/secret `pwgen -1cns 30 ` \
 && kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/api "http" \
 && kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/ip "0.0.0.0" \
 && kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/service/port 8080 \
 && kdb set -N system /sw/elektra/restbackend/#0/current/cppcms/http/script_names/#0 "/" \
 && kdb set '/sw/elektra/restbackend/#0/current/cppcms/daemon/enable' '0' \
 && kdb set '/sw/elektra/restbackend/#0/current/cppcms/logging/level' 'debug'
ENTRYPOINT ["kdb"]
CMD ["run-rest-backend"]
EXPOSE 8080/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
