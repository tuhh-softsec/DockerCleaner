FROM ubuntu:18.04
LABEL maintainer="docker@couchbase.com"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#   Install dependencies:
#    runit: for container process management
#    wget: for downloading .deb
#    chrpath: for fixing curl, below
#    tzdata: timezone info used by some N1QL functions
#    man: so couchbase-cli help works
#   Additional dependencies for system commands used by cbcollect_info:
#    lsof: lsof
#    lshw: lshw
#    sysstat: iostat, sar, mpstat
#    net-tools: ifconfig, arp, netstat
#    numactl: numactl
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends runit=2.1.2-9.2ubuntu1 wget=1.19.4-1ubuntu2.2 chrpath=0.16-2 tzdata=2023c-0ubuntu0.18.04 man-db=2.8.3-2ubuntu0.1 lsof=4.89+dfsg-0.1 lshw=02.18-0.1ubuntu6.18.04.2 sysstat=11.6.1-1ubuntu0.2 net-tools=1.60+git20161116.90da8a0-1ubuntu1 numactl=2.0.11-2.1ubuntu0.1 python-httplib2=0.9.2+dfsg-1ubuntu0.3 -yq \
 && apt-get autoremove \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   http://smarden.org/runit/useinit.html#sysv - at some point the script
#   runsvdir-start was moved/renamed to this odd name, so we put it back
#   somewhere sensible. This appears to be necessary for Ubuntu 20 but
#   not Ubuntu 16.
RUN if [ ! -x /usr/sbin/runsvdir-start ] ; then cp -a /etc/runit/2 /usr/sbin/runsvdir-start ; fi
ARG CB_VERSION=6.0.5
ARG CB_RELEASE_URL=https://packages.couchbase.com/releases/6.0.5
ARG CB_PACKAGE=couchbase-server-enterprise_6.0.5-ubuntu18.04_amd64.deb
ARG CB_SHA256=6b152590867a58d771cffc22774d3cd66c916defcbeeeb339aca8d0a8e6d7f8d
ENV PATH="$PATH:/opt/couchbase/bin:/opt/couchbase/bin/tools:/opt/couchbase/bin/install"
#   Create Couchbase user with UID 1000 (necessary to match default
#   boot2docker UID)
RUN groupadd -g 1000 couchbase \
 && useradd couchbase -u 1000 -g couchbase -M
#   Install couchbase
RUN set -x \
 && export INSTALL_DONT_START_SERVER=1 \
 && wget -N --no-verbose $CB_RELEASE_URL/$CB_PACKAGE \
 && echo "$CB_SHA256 $CB_PACKAGE" | sha256sum -c - \
 && apt-get update \
 && apt-get install --no-install-recommends ./$CB_PACKAGE -y \
 && rm -f ./$CB_PACKAGE \
 && apt-get autoremove \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Update VARIANT.txt to indicate we're running in our Docker image
RUN sed -i -e '1 s/$/\/docker/' /opt/couchbase/VARIANT.txt
#   Add runit script for couchbase-server
COPY scripts/run /etc/service/couchbase-server/run
RUN mkdir -p /etc/runit/runsvdir/default/couchbase-server/supervise \
 && chown -R couchbase:couchbase /etc/service /etc/runit/runsvdir/default/couchbase-server/supervise
#   Add dummy script for commands invoked by cbcollect_info that
#   make no sense in a Docker container
COPY scripts/dummy.sh /usr/local/bin/
RUN ln -s dummy.sh /usr/local/bin/iptables-save \
 && ln -s dummy.sh /usr/local/bin/lvdisplay \
 && ln -s dummy.sh /usr/local/bin/vgdisplay \
 && ln -s dummy.sh /usr/local/bin/pvdisplay
#   Fix curl RPATH
RUN chrpath -r '$ORIGIN/../lib' /opt/couchbase/bin/curl
#   Add bootstrap script
COPY scripts/entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
CMD ["couchbase-server"]
#   8091: Couchbase Web console, REST/HTTP interface
#   8092: Views, queries, XDCR
#   8093: Query services (4.0+)
#   8094: Full-text Search (4.5+)
#   8095: Analytics (5.5+)
#   8096: Eventing (5.5+)
#   11207: Smart client library data node access (SSL)
#   11210: Smart client library/moxi data node access
#   11211: Legacy non-smart client library data node access
#   18091: Couchbase Web console, REST/HTTP interface (SSL)
#   18092: Views, query, XDCR (SSL)
#   18093: Query services (SSL) (4.0+)
#   18094: Full-text Search (SSL) (4.5+)
#   18095: Analytics (SSL) (5.5+)
#   18096: Eventing (SSL) (5.5+)
EXPOSE 8091/tcp 8092/tcp 8093/tcp 8094/tcp 8095/tcp 8096/tcp 11207/tcp 11210/tcp 11211/tcp 18091/tcp 18092/tcp 18093/tcp 18094/tcp 18095/tcp 18096/tcp
VOLUME /opt/couchbase/var
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
# Please add your HEALTHCHECK here!!!
