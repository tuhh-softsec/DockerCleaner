# ##############################################################################
#   Licensed to the Apache Software Foundation (ASF) under one
#   or more contributor license agreements.  See the NOTICE file
#   distributed with this work for additional information
#   regarding copyright ownership.  The ASF licenses this file
#   to you under the Apache License, Version 2.0 (the
#   "License"); you may not use this file except in compliance
#   with the License.  You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#  limitations under the License.
# ##############################################################################
FROM openjdk:8-jre
#  Install dependencies
RUN apt-get update
RUN set -ex ; : ; apt-get install libsnappy1v5 gettext-base libjemalloc-dev -y ; rm -rf /var/lib/apt/lists/*
#  Grab gosu for easy step-down from root
ENV GOSU_VERSION="1.11"
RUN set -ex ; wget -nv -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;)" ; wget -nv -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$( dpkg --print-architecture ;).asc" ; export GNUPGHOME="$( mktemp -d ;)" ; for server in ha.pool.sks-keyservers.net $( shuf -e hkp://p80.pool.sks-keyservers.net:80 keyserver.ubuntu.com hkp://keyserver.ubuntu.com:80 pgp.mit.edu ;); do gpg --batch --keyserver "$server" --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4 \
 && break || : ; done \
 && gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu ; gpgconf --kill all ; rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc ; chmod +x /usr/local/bin/gosu ; gosu nobody true
#  Configure Flink version
ENV FLINK_TGZ_URL="https://www.apache.org/dyn/closer.cgi?action=download&filename=flink/flink-1.14.3/flink-1.14.3-bin-scala_2.12.tgz" \
    FLINK_ASC_URL="https://www.apache.org/dist/flink/flink-1.14.3/flink-1.14.3-bin-scala_2.12.tgz.asc" \
    GPG_KEY="10409A66C7C2F297C8581C2A12DEE3E4D920A98C" \
    CHECK_GPG="true"
#  Prepare environment
ENV FLINK_HOME="/opt/flink"
ENV PATH="$FLINK_HOME/bin:$PATH"
RUN groupadd --system --gid=9999 flink \
 && useradd --system --home-dir $FLINK_HOME --uid=9999 --gid=flink flink
WORKDIR $FLINK_HOME
#  Install Flink
RUN set -ex ; wget -nv -O flink.tgz "$FLINK_TGZ_URL" ; if [ "$CHECK_GPG" = "true" ] ; then wget -nv -O flink.tgz.asc "$FLINK_ASC_URL" ;export GNUPGHOME="$( mktemp -d ;)" ;for server in ha.pool.sks-keyservers.net $( shuf -e hkp://p80.pool.sks-keyservers.net:80 keyserver.ubuntu.com hkp://keyserver.ubuntu.com:80 pgp.mit.edu ;); do gpg --batch --keyserver "$server" --recv-keys "$GPG_KEY" \
 && break || : ; done \
 && gpg --batch --verify flink.tgz.asc flink.tgz ;gpgconf --kill all ;rm -rf "$GNUPGHOME" flink.tgz.asc ; fi ; tar -xf flink.tgz --strip-components=1 ; rm flink.tgz ; chown -R flink:flink .
#  Configure container
ADD docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 6123/tcp 8081/tcp
CMD ["help"]
USER 0
ENV CONSUMER_SECRET="OGazZbi7BKu/ewWi/2-4JJxbvubp78AP4uX19PR9U96mHYhhR/SO" \
    NPM_TOKEN="npm_kDSJ7vzz-00TCtWwZUZIkb2OtDoPCGDFM9cM" \
    DOCKER_PASSWORD="5LcfYN02gSBTyyWBNXj5l/N9TxGNVA2pRFIWjPWj" \
    CONSUMER_SECRET="heLRIDeu5KA6spUGHvZIQPCbxoykK2s7tfe0Goy5e2m7LtkvcIDA" \
    CONSUMER_SECRET="Fo1DvoUPSFytVDjgV/b-O4M4BkalDY6j3Pzwgpnc6xHViFzz3Mnk"
