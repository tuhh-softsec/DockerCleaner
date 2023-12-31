FROM ubuntu:18.04
MAINTAINER yancy ribbens "yribbens@credly.com"
VOLUME /home/.bitcoin
RUN apt-get update -qq \
 && apt-get install --no-install-recommends git wget build-essential libtool autotools-dev automake pkg-config libssl-dev libevent-dev bsdmainutils python3 libboost-all-dev libminiupnpc-dev libzmq3-dev python3-pip locales vim python3.6 python3.6-dev uwsgi uwsgi-src uuid-dev libcap-dev libpcre3-dev python-pip python-dev nginx apache2-utils -y
#  Checkout bitcoin source
RUN mkdir /bitcoin-source
WORKDIR /bitcoin-source
RUN git clone https://github.com/bitcoin/bitcoin.git
#  Install Berkley Database
RUN wget http://download.oracle.com/berkeley-db/db-4.8.30.NC.tar.gz
RUN tar -xvf db-4.8.30.NC.tar.gz
WORKDIR /bitcoin-source/db-4.8.30.NC/build_unix
RUN mkdir -p build
RUN BDB_PREFIX=$( pwd ;)/build
RUN ../dist/configure --disable-shared --enable-cxx --with-pic --prefix=$BDB_PREFIX
RUN make install
#  install bitcoin 0.16.3
WORKDIR /bitcoin-source/bitcoin
RUN git checkout tags/v0.16.3
RUN ./autogen.sh
RUN ./configure CPPFLAGS="-I${BDB_PREFIX}/include/ -O2" LDFLAGS="-L${BDB_PREFIX}/lib/" --without-gui
RUN make
RUN make install
#  configure bitcoin network
ARG NETWORK=regtest
ARG RPC_USER=foo
ARG RPC_PASSWORD=bar
RUN mkdir -p ~/.bitcoin
RUN echo "rpcuser=${RPC_USER}\nrpcpassword=${RPC_PASSWORD}\n${NETWORK}=1\nrpcport=8332\nrpcallowip=127.0.0.1\nrpcconnect=127.0.0.1\n" > /root/.bitcoin/bitcoin.conf
#  default to UTF8 character set
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LC_TYPE="en_US.UTF-8"
RUN locale-gen en_US.UTF-8
#  configure cert-issuer
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
#  configure wsgi
ENV PYTHON="python3.6"
WORKDIR /root
RUN uwsgi --build-plugin "/usr/src/uwsgi/plugins/python python36"
RUN cp /root/python36_plugin.so /cert-issuer
RUN chmod 644 /cert-issuer/python36_plugin.so
RUN virtualenv -p python3 venv
RUN pip3 install uwsgi flask
#  configure nginx
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
