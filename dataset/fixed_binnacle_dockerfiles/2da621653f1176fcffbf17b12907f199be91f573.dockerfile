FROM ubuntu:16.04
#   Authentication information for hpfeeds in the broker container.
#   Example command to add the credentials to the broker:
#
#   docker exec broker /opt/hpfeeds/env/bin/python /opt/hpfeeds/broker/add_user.py \
#  	dionaea b64c2e86d0eb546e5b2757508df50222 dionaea.connections ""
#   docker exec broker /opt/hpfeeds/env/bin/python /opt/hpfeeds/broker/add_user.py \
#  	dionaea b64c2e86d0eb546e5b2757508df50222 dionaea.dcerpcrequests ""
#   docker exec broker /opt/hpfeeds/env/bin/python /opt/hpfeeds/broker/add_user.py \
#  	dionaea b64c2e86d0eb546e5b2757508df50222 dionaea.shellcodeprofiles ""
#   docker exec broker /opt/hpfeeds/env/bin/python /opt/hpfeeds/broker/add_user.py \
#  	dionaea b64c2e86d0eb546e5b2757508df50222 dionaea.capture ""
ENV HPF_HOST="broker"
ENV HPF_PORT="10000"
ENV HPF_IDENT="dionaea"
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
#   You may want to change the dionaea user id inside the container if
#   you're going to mount an external log directory.  The internal uid
#   needs write permissions to the external volume.
#
#   E.g. to save dionaea logs to /var/log/dionaea on the host OS, run docker with ...
#
#   	-v /var/log/dionaea:/opt/dionaea/log
#   
#   ... and make sure /var/log/dionaea is writable by the dionaea uid inside the container.
ENV DIONAEA_UID="1000"
#   Install packages
RUN apt-get update \
 && apt-get install --no-install-recommends supervisor=3.2.0-2ubuntu0.2 autoconf=2.69-9 automake=1:1.15-4ubuntu1 build-essential=12.1ubuntu2 git=1:2.7.4-0ubuntu1.10 htop=2.0.1-1ubuntu1 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libemu-dev=0.2.0+git20120122-1.2 libgc-dev=1:7.4.2-7.3ubuntu0.1 libev-dev=1:4.22-1 libglib2.0-dev=2.48.2-0ubuntu4.8 libloudmouth1-dev=1.5.2-1build1 libnetfilter-queue-dev=1.0.2-2 libpcap-dev=1.7.4-2ubuntu0.1 libreadline-dev=6.3-8ubuntu2 libsqlite3-dev=3.11.0-1ubuntu1.5 libssl-dev=1.0.2g-1ubuntu4.20 libtool=2.4.6-0.1 libudns-dev=0.4-1 pkg-config=0.29.1-0ubuntu1 sqlite3=3.11.0-1ubuntu1.5 subversion=1.9.3-2ubuntu1.3 python3=3.5.1-3 cython3=0.23.4-0ubuntu5 python3-pip=8.1.1-2ubuntu0.6 -y
#   Create the dionaea user
RUN useradd -u ${DIONAEA_UID} -s /bin/false dionaea
#   Download dionaea and compile it
RUN cd /usr/src \
 && git clone https://github.com/ThomasAdam/liblcfg.git \
 && cd liblcfg/code \
 && autoreconf -vi \
 && ./configure --prefix=/opt/dionaea \
 && make install \
 && cd /usr/src \
 && git clone https://github.com/threatstream/mhn \
 && git clone https://github.com/gento/dionaea.git dionaea \
 && cp mhn/server/mhn/static/hpfeeds.py dionaea/modules/python/scripts/ \
 && cd dionaea \
 && autoreconf -vi \
 && ln -s /usr/bin/python3 /usr/bin/python3.2 \
 && ln -s /usr/bin/cython3 /usr/bin/cython \
 && pip3 install -e git+https://github.com/HurricaneLabs/pyev.git#egg=pyev \
 && ./configure --disable-werror --prefix=/opt/dionaea --with-lcfg-include=/opt/dionaea/include/ --with-lcfg-lib=/opt/dionaea/lib/ \
 && sed -i -e 's/-Werror//' modules/nfq/Makefile \
 && make LDFLAGS=-lcrypto \
 && make install \
 && sed -i -e 's/{:s}/{!s:s}/g' /opt/dionaea/lib/dionaea/python/dionaea/sip/__init__.py \
 && sed -i -e 's/{:/{!s:/g' /opt/dionaea/lib/dionaea/python/dionaea/mssql/mssql.py \
 && chown $DIONAEA_UID /opt/dionaea/var/dionaea
COPY dionaea.conf /opt/dionaea/etc/dionaea/
COPY supervisor-dionaea.conf /etc/supervisor/conf.d/dionaea.conf
RUN sed -i -e "s/HPF_HOST/$HPF_HOST/" /opt/dionaea/etc/dionaea/dionaea.conf \
 && sed -i -e "s/HPF_PORT/$HPF_PORT/" /opt/dionaea/etc/dionaea/dionaea.conf \
 && sed -i -e "s/HPF_IDENT/$HPF_IDENT/" /opt/dionaea/etc/dionaea/dionaea.conf \
 && sed -i -e "s/HPF_SECRET/$HPF_SECRET/" /opt/dionaea/etc/dionaea/dionaea.conf
EXPOSE 21/tcp
EXPOSE 42/tcp
EXPOSE 80/tcp
EXPOSE 135/tcp
EXPOSE 443/tcp
EXPOSE 445/tcp
EXPOSE 1433/tcp
EXPOSE 1723/tcp
EXPOSE 1883/tcp
EXPOSE 3306/tcp
EXPOSE 5060/tcp
EXPOSE 5061/tcp
EXPOSE 69/udp
EXPOSE 1900/udp
EXPOSE 5060/udp
WORKDIR /opt/dionaea
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisor/supervisord.conf"]
#   How to run the container:
#
#   docker run --rm --name dionaea --link broker dionaea
#
#   To log outside the container:
#
#   mkdir /var/log/dionaea
#   chown 1000 /var/log/dionaea
#   docker run --rm --name dionaea -v /var/log/dionaea:/opt/dionaea/log --link broker dionaea
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
