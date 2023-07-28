FROM ubuntu:16.04
MAINTAINER Florian Kelbert <f.kelbert@imperial.ac.uk>
ENV APACHE_VERSION="2.4.23"
ENV LIBRESSL_VERSION="2.4.1"
ARG MAIN_DIR=/talos/
ARG SGX_FILE=sgx_linux_ubuntu16.04.1_x64_sdk_1.9.100.39124.bin
ARG SGX_URL=https://download.01.org/intel-sgx/linux-1.9/${SGX_FILE}
ARG LIBRESSL_ROOT=${MAIN_DIR}/src/libressl-${LIBRESSL_VERSION}/
ARG LIBRESSL_CRYPTO=${LIBRESSL_ROOT}/crypto
ARG LIBRESSL_LIB=${LIBRESSL_ROOT}/lib
ARG APACHE_FILE=httpd-2.4.23.tar.bz2
ARG APACHE_URL=https://archive.apache.org/dist/httpd/${APACHE_FILE}
ARG APACHE_ROOT=${MAIN_DIR}/httpd-${APACHE_VERSION}
ARG APACHE_INSTALL=${APACHE_ROOT}/install
ARG APACHE_HTDOCS=${APACHE_INSTALL}/htdocs
ARG APACHE_USR=www-data
ARG APACHE_GRP=www-data
ARG STARTFILE=$MAIN_DIR/start.sh
RUN apt-get update \
 && apt-get install --no-install-recommends gcc=4:5.3.1-1ubuntu1 avr-libc=1:1.8.0+Atmel3.5.0-1 build-essential=12.1ubuntu2 libpcre3-dev=2:8.38-3.1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 git=1:2.7.4-0ubuntu1.10 bison=2:3.0.4.dfsg-1 flex=2.6.0-11 libtool=2.4.6-0.1 git=1:2.7.4-0ubuntu1.10 openssh-client=1:7.2p2-4ubuntu2.10 wget=1.17.1-1ubuntu1.5 ca-certificates=20210119~16.04.1 make=4.1-6 patch=2.7.5-1ubuntu0.16.04.2 libapr1-dev=1.5.2-3 libaprutil1-dev=1.5.4-1build1 -y \
 && apt-get clean \
 && apt-get autoclean \
 && rm -rf /var/lib/apt/lists/*
#   Create main directory
RUN mkdir -p ${MAIN_DIR}
#   Copy repository into container
WORKDIR ${MAIN_DIR}
COPY . .
#   Install Intel SGX SDK
RUN wget ${SGX_URL} \
 && chmod +x ${SGX_FILE} \
 && echo "yes" | ./${SGX_FILE} \
 && rm ${SGX_FILE}
#   Patch libressl with TaLoS code
WORKDIR ${MAIN_DIR}/src/talos
RUN ./patch_libressl.sh
#   Compile and install TaLoS
WORKDIR ${LIBRESSL_CRYPTO}
RUN ln -s Makefile.sgx Makefile \
 && . "${MAIN_DIR}/sgxsdk/environment" \
 && make \
 && make install
#   Download, unpack, configure, compile and install Apache Httpd
WORKDIR ${MAIN_DIR}
RUN wget ${APACHE_URL} \
 && tar xjvf ${APACHE_FILE} \
 && rm ${APACHE_FILE}
WORKDIR ${APACHE_ROOT}
RUN ./configure --prefix="${APACHE_INSTALL}" --enable-http --enable-proxy --enable-ssl --enable-ssl-staticlib-deps --with-ssl="${LIBRESSL_ROOT}" --enable-file-cache --enable-cache --enable-disk-cache --enable-mem-cache --enable-deflate --enable-expires --enable-headers --enable-usertrack --enable-cgi --enable-vhost-alias --enable-rewrite --enable-so --enable-dav --with-mpm=worker
RUN sed -i -e "s#MOD_LDFLAGS.*#MOD_LDFLAGS=-L${LIBRESSL_LIB} -lssl -lcrypto -lsgx_urts_sim -lsgx_uae_service_sim -ldl -lrt -lcrypt -lpthread#" -e "$ iMOD_CFLAGS=-I${LIBRESSL_ROOT}/include" modules/ssl/modules.mk
RUN . "${MAIN_DIR}/sgxsdk/environment" \
 && export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${LIBRESSL_LIB} \
 && LIBRARY_PATH=${LIBRARY_PATH}:${LD_LIBRARY_PATH} make
RUN make install
#   Configure httpd, set links, create certificate, set owner
RUN mkdir -p ${APACHE_INSTALL}/lib \
 && ln -s ${LIBRESSL_CRYPTO}/enclave.signed.so \
 && ln -s ${LIBRESSL_LIB}/libssl.so ${APACHE_INSTALL}/lib/ \
 && ln -s ${LIBRESSL_LIB}/libcrypto.so ${APACHE_INSTALL}/lib/
RUN cp ${MAIN_DIR}/conf/apache/httpd.conf ${APACHE_INSTALL}/conf/
RUN echo "\nABC\nMy City\nMy Institution\n\nwww.example.com\n\n" | openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout ${APACHE_INSTALL}/conf/cert.key -out ${APACHE_INSTALL}/conf/cert.crt
RUN chown -R ${APACHE_USR}:${APACHE_GRP} ${APACHE_INSTALL}
#   Create httpd startup file
RUN echo "#!/bin/bash\n cd ${APACHE_ROOT}; \n source ${MAIN_DIR}/sgxsdk/environment;\n export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${LIBRESSL_LIB};\n ./install/bin/httpd -X" > /start.sh \
 && chmod +x /start.sh
EXPOSE 7777/tcp
EXPOSE 7778/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
