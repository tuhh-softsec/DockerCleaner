FROM ubuntu:16.04 AS BUILDSTEP
MAINTAINER Sergey Dolin <sergey@s4y.solutions>
#   Install all necessary for building dependecnies, read more about
#   depencensies in readme section in the root of the repo.
RUN apt-get update \
 && apt-get upgrade -y \
 && (apt-get update ;apt-get install --no-install-recommends jq=1.5+dfsg-1ubuntu0.1 netcat=1.10-41 inetutils-ping=2:1.9.4-1build1 vim=2:7.4.1689-3ubuntu1.5 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 make=4.1-6 cmake=3.5.1-1ubuntu3 gcc=4:5.3.1-1ubuntu1 g++=4:5.3.1-1ubuntu1 python=2.7.12-1~16.04 libev-dev=1:4.22-1 libmpdec-dev=2.4.2-1 libjansson-dev=2.7-3ubuntu0.1 libssl-dev=1.0.2g-1ubuntu4.20 libgnutls-dev=3.4.10-4ubuntu1.9 libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 libhttp-parser-dev=2.1-2 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libldap2-dev=2.4.42+dfsg-2ubuntu3.13 libkrb5-dev=1.13.2+dfsg-5ubuntu2.2 libalberta-dev=3.0.1-1build1 libgss-dev=1.0.3-2 libidn11-dev=1.32-3ubuntu1.2 librtmp-dev=2.4+20151223.gitfa8646d-1ubuntu0.1 -y ) \
 && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/edenhill/librdkafka.git \
 && cd librdkafka \
 && ./configure \
 && make \
 && make install \
 && cd / \
 && rm -rf librdkafka
RUN mkdir viabtc_exchange_server
#   Copy all necessary files and directories for build process, basically we
#   recreate the root repository directory, but with tham we could ensure that
#   nothing
COPY depends viabtc_exchange_server/depends
COPY network viabtc_exchange_server/network
COPY utils viabtc_exchange_server/utils
COPY alertcenter viabtc_exchange_server/alertcenter
COPY matchengine viabtc_exchange_server/matchengine
COPY marketprice viabtc_exchange_server/marketprice
COPY readhistory viabtc_exchange_server/readhistory
COPY accesshttp viabtc_exchange_server/accesshttp
COPY accessws viabtc_exchange_server/accessws
COPY makefile.inc viabtc_exchange_server/makefile.inc
#   Do the actial build of the microservices.
RUN cd viabtc_exchange_server/depends/hiredis \
 && make \
 && mv libhiredis.* /usr/lib \
 && cd .. \
 && mv hiredis /usr/include
RUN cd viabtc_exchange_server/network \
 && make
RUN cd viabtc_exchange_server/utils \
 && make
RUN cd viabtc_exchange_server/alertcenter \
 && make
RUN cd viabtc_exchange_server/matchengine \
 && make
RUN cd viabtc_exchange_server/marketprice \
 && make
RUN cd viabtc_exchange_server/readhistory \
 && make
RUN cd viabtc_exchange_server/accesshttp \
 && make
RUN cd viabtc_exchange_server/accessws \
 && make
#   Multi-stage builds are a new feature requiring Docker 17.05 or higher on the
#   daemon and client. Multistage builds are useful to anyone who has struggled
#   to optimize Dockerfiles while keeping them easy to read and maintain.
#   With this we ensure that previous build layers are not stored and we grab
#   only what we need.
FROM ubuntu:16.04
RUN :
RUN apt-get upgrade -y
RUN (apt-get update ;apt-get install --no-install-recommends libldap-2.4 libgss3=1.0.3-2 librtmp1=2.4+20151223.gitfa8646d-1ubuntu0.1 libmysqlclient20=5.7.33-0ubuntu0.16.04.1 jq=1.5+dfsg-1ubuntu0.1 netcat=1.10-41 -y )
RUN rm -rf /var/lib/apt/lists/*
RUN mkdir /viabtc_exchange_server
#   keep .exe for better grep of ps output
COPY --from=BUILDSTEP /viabtc_exchange_server/alertcenter/alertcenter /viabtc_exchange_server/alertcenter.exe
COPY docker/exchange/configs/alertcenter.json /viabtc_exchange_server
COPY --from=BUILDSTEP /viabtc_exchange_server/matchengine/matchengine /viabtc_exchange_server/matchengine.exe
COPY docker/exchange/configs/matchengine.json /viabtc_exchange_server
COPY --from=BUILDSTEP /viabtc_exchange_server/marketprice/marketprice /viabtc_exchange_server/marketprice.exe
COPY docker/exchange/configs/marketprice.json /viabtc_exchange_server
COPY --from=BUILDSTEP /viabtc_exchange_server/readhistory/readhistory /viabtc_exchange_server/readhistory.exe
COPY docker/exchange/configs/readhistory.json /viabtc_exchange_server
COPY --from=BUILDSTEP /viabtc_exchange_server/accesshttp/accesshttp /viabtc_exchange_server/accesshttp.exe
COPY docker/exchange/configs/accesshttp.json /viabtc_exchange_server
COPY --from=BUILDSTEP /viabtc_exchange_server/accessws/accessws /viabtc_exchange_server/accessws.exe
COPY docker/exchange/configs/accessws.json /viabtc_exchange_server
COPY docker/exchange/entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
