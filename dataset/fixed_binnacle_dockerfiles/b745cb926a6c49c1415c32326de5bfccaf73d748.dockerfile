FROM ubuntu:16.04 AS omim
WORKDIR /usr/local/src
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 git=1:2.7.4-0ubuntu1.10 libsqlite3-dev=3.11.0-1ubuntu1.5 qt5-default=5.5.1+dfsg-16ubuntu7.7 -y
RUN git clone --recursive --depth 1 https://github.com/mapsme/omim.git
WORKDIR /usr/local/src/omim
RUN echo | ./configure.sh \
 && CONFIG="gtool no-tests" tools/unix/build_omim.sh -cr
FROM ubuntu:16.04
LABEL maintainer="\"Seth Fitzsimmons <seth@mojodna.net>\""
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 curl=7.47.0-1ubuntu2.19 software-properties-common=0.96.20.10 -y \
 && curl -sf https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - \
 && add-apt-repository -y -u "deb https://deb.nodesource.com/node_6.x $( lsb_release -c -s ;) main" \
 && add-apt-repository -y -u ppa:ubuntugis/ubuntugis-unstable \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && add-apt-repository -y -u "deb https://dl.yarnpkg.com/debian/ stable main" \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 default-jre-headless=2:1.8-56ubuntu2 gdal-bin=1.11.3+dfsg-3build2 git=1:2.7.4-0ubuntu1.10 language-pack-en-base=1:16.04+20160627 libffi-dev=3.2.1-4 libgdal-dev=1.11.3+dfsg-3build2 libmagic1=1:5.25-2ubuntu1.4 libpq-dev=9.5.25-0ubuntu0.16.04.1 libqt5core5a=5.5.1+dfsg-16ubuntu7.7 libsqlite3-mod-spatialite=4.3.0a-5 libspatialite7=4.3.0a-5 libspatialite-dev=4.3.0a-5 libproxy1v5=0.4.11-5ubuntu1.2 libxslt1-dev=1.1.28-2.1ubuntu0.3 nodejs=4.2.6~dfsg-1ubuntu4.2 osmctools=0.6-1 postgresql-client=9.5+173ubuntu0.3 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 python-wheel=0.29.0-1 python-gdal=1.11.3+dfsg-3build2 spatialite-bin=4.3.0-2 unzip=6.0-20ubuntu1.1 yarn zip=3.0-11 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV LC_ALL="en_US.UTF-8"
RUN curl -sfL http://download.osmand.net/latest-night-build/OsmAndMapCreator-main.zip -o /tmp/osmandmapcreator.zip \
 && unzip /tmp/osmandmapcreator.zip -d /opt/osmandmapcreator \
 && rm /tmp/osmandmapcreator.zip
RUN curl -sfL https://s3.amazonaws.com/bdon/brandon.liu.hotosm.org/mkgmap-r3890.zip -o /tmp/mkgmap.zip \
 && unzip /tmp/mkgmap.zip -d /opt \
 && mv /opt/mkgmap-* /opt/mkgmap \
 && rm /tmp/mkgmap.zip
RUN curl -sfL https://s3.amazonaws.com/bdon/brandon.liu.hotosm.org/splitter-r583.zip -o /tmp/splitter.zip \
 && unzip /tmp/splitter.zip -d /opt \
 && mv /opt/splitter-* /opt/splitter \
 && rm /tmp/splitter.zip
ENV GENERATOR_TOOL="/usr/local/bin/generator_tool"
ENV OSMCONVERT="/usr/bin/osmconvert"
#   TODO set DATA= something, as generator_tool looks in __dirname/../../data
RUN mkdir -p /usr/data
COPY --from=omim /usr/local/src/omim/data/categories.txt /usr/data/
COPY --from=omim /usr/local/src/omim/data/classificator.txt /usr/data/
COPY --from=omim /usr/local/src/omim/data/countries.txt /usr/data/
COPY --from=omim /usr/local/src/omim/data/countries_meta.txt /usr/data/
COPY --from=omim /usr/local/src/omim/data/editor.config /usr/data/
COPY --from=omim /usr/local/src/omim/data/drules_proto.bin /usr/data/
COPY --from=omim /usr/local/src/omim/data/drules_proto_clear.bin /usr/data/
COPY --from=omim /usr/local/src/omim/data/drules_proto_dark.bin /usr/data/
COPY --from=omim /usr/local/src/omim/data/drules_proto_vehicle_clear.bin /usr/data/
COPY --from=omim /usr/local/src/omim/data/drules_proto_vehicle_dark.bin /usr/data/
COPY --from=omim /usr/local/src/omim/data/types.txt /usr/data/
COPY --from=omim /usr/local/src/omim/tools/unix/generate_mwm.sh /usr/local/bin/
COPY --from=omim /usr/local/src/omim-build-release/out/release/generator_tool /usr/local/bin/
#   used to determine m_writableDir in Platform::Platform
COPY --from=omim /usr/local/src/omim/data/eula.html /usr/data/
COPY --from=omim /usr/lib/x86_64-linux-gnu/libQt5Network.so.5 /usr/lib/x86_64-linux-gnu/libQt5Network.so.5
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
