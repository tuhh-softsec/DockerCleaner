FROM ubuntu:18.04
MAINTAINER yancy ribbens "yribbens@credly.com"
VOLUME /home/.bitcoin
RUN apt-get update -qq \
 && apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 wget=1.19.4-1ubuntu2.2 build-essential=12.4ubuntu1 libtool=2.4.6-2 autotools-dev=20180224.1 automake=1:1.15.1-3ubuntu2 pkg-config=0.29.1-0ubuntu2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libevent-dev=2.1.8-stable-4build1 bsdmainutils=11.1.2ubuntu1 python3=3.6.7-1~18.04 libboost-all-dev=1.65.1.0ubuntu1 libminiupnpc-dev=1.9.20140610-4ubuntu2 libzmq3-dev=4.2.5-1ubuntu0.2 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 locales=2.27-3ubuntu1.6 vim=2:8.0.1453-1ubuntu1.11 python3.6=3.6.9-1~18.04ubuntu1.12 python3.6-dev=3.6.9-1~18.04ubuntu1.12 uwsgi=2.0.15-10.2ubuntu2.2 uwsgi-src=2.0.15-10.2ubuntu2.2 uuid-dev=2.31.1-0.4ubuntu3.7 libcap-dev=1:2.25-1.2 libpcre3-dev=2:8.39-9ubuntu0.1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-dev=2.7.15~rc1-1 nginx=1.14.0-0ubuntu1.11 apache2-utils=2.4.29-1ubuntu4.27 -y
#   Checkout bitcoin source
RUN mkdir /bitcoin-source
WORKDIR /bitcoin-source
RUN git clone https://github.com/bitcoin/bitcoin.git
#   Install Berkley Database
RUN wget http://download.oracle.com/berkeley-db/db-4.8.30.NC.tar.gz
RUN tar -xvf db-4.8.30.NC.tar.gz
WORKDIR /bitcoin-source/db-4.8.30.NC/build_unix
RUN mkdir -p build
RUN BDB_PREFIX=$( pwd ;)/build
RUN ../dist/configure --disable-shared --enable-cxx --with-pic --prefix=$BDB_PREFIX
RUN make install
#   install bitcoin 0.16.3
WORKDIR /bitcoin-source/bitcoin
RUN git checkout tags/v0.16.3
RUN ./autogen.sh
RUN ./configure CPPFLAGS="-I${BDB_PREFIX}/include/ -O2" LDFLAGS="-L${BDB_PREFIX}/lib/" --without-gui
RUN make
RUN make install
#   configure bitcoin network
ARG NETWORK=regtest
ARG RPC_USER=foo
ARG RPC_PASSWORD=bar
RUN mkdir -p ~/.bitcoin
RUN echo "rpcuser=${RPC_USER}\nrpcpassword=${RPC_PASSWORD}\n${NETWORK}=1\nrpcport=8332\nrpcallowip=127.0.0.1\nrpcconnect=127.0.0.1\n" > /root/.bitcoin/bitcoin.conf
#   default to UTF8 character set
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LC_TYPE="en_US.UTF-8"
RUN locale-gen en_US.UTF-8
#   configure cert-issuer
ARG ISSUER=<issuing-address>
ARG KEY=cert.key
ARG CRT=cert.crt
COPY . /cert-issuer
COPY conf_regtest.ini /etc/cert-issuer/conf.ini
COPY $CRT /etc/ssl/certs/$CRT
COPY $KEY /etc/ssl/private/$KEY
RUN mkdir -p /etc/cert-issuer/
WORKDIR /cert-issuer
RUN pip3 install cert-issuer
RUN pip3 install -r requirements.txt
RUN sed -i.bak "s/<issuing-address>/$ISSUER/g" /etc/cert-issuer/conf.ini
#   configure wsgi
ENV PYTHON="python3.6"
WORKDIR /root
RUN uwsgi --build-plugin "/usr/src/uwsgi/plugins/python python36"
RUN cp /root/python36_plugin.so /cert-issuer
RUN chmod 644 /cert-issuer/python36_plugin.so
RUN virtualenv -p python3 venv
RUN pip3 install uwsgi flask
#   configure nginx
ARG SERVER=<server-name>
EXPOSE 443/tcp
WORKDIR /cert-issuer
COPY cert_issuer_tls_site /etc/nginx/sites-available
COPY .htpasswd /etc/nginx/htpasswd
RUN chmod -R 640 /etc/nginx/htpasswd
RUN chown :www-data /etc/nginx/htpasswd
RUN ln -s /etc/nginx/sites-available/cert_issuer_tls_site /etc/nginx/sites-enabled
RUN sed -i.bak "s/<server-name>/$SERVER/g" /etc/nginx/sites-available/cert_issuer_tls_site
RUN sed -i.bak "s/<cert-crt-name>/$CRT/g" /etc/nginx/sites-available/cert_issuer_tls_site
RUN sed -i.bak "s/<cert-key-name>/$KEY/g" /etc/nginx/sites-available/cert_issuer_tls_site
RUN sed -i.bak "s/# server_names_hash_bucket_size 64/server_names_hash_bucket_size 128/g" /etc/nginx/nginx.conf
ENTRYPOINT bitcoind -daemon \
 && service nginx start \
 && uwsgi --ini wsgi.ini
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
