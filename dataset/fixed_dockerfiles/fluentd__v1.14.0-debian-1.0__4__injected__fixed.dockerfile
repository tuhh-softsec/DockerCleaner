#   AUTOMATICALLY GENERATED
#   DO NOT EDIT THIS FILE DIRECTLY, USE /Dockerfile.template.erb
FROM ruby:2.7-slim-bullseye
LABEL maintainer="\"Fluentd developers <fluentd@googlegroups.com>\""
LABEL Description="Fluentd docker image" \
      Vendor="Fluent Organization" \
      Version="1.14.6"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV TINI_VERSION="0.18.0"
#   Do not split this into multiple RUN!
#   Docker creates a layer for every RUN-Statement
#   therefore an 'apt-get purge' has no effect
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119 -y \
 && buildDeps=" make gcc g++ libc6-dev wget bzip2 gnupg dirmngr " \
 && apt-get install --no-install-recommends make=4.3-4.1 gcc=4:10.2.1-1 g++=4:10.2.1-1 libc6-dev=2.31-13+deb11u5 wget=1.21-1+deb11u1 bzip2=1.0.8-4 gnupg=2.2.27-2+deb11u2 dirmngr=2.2.27-2+deb11u2 -y \
 && echo 'gem: --no-document' >> /etc/gemrc \
 && gem install oj --version 3.14.3 \
 && gem install json --version 2.6.3 \
 && gem install async-http --version 0.60.1 \
 && gem install ext_monitor --version 0.1.2 \
 && gem install fluentd --version 1.16.0 \
 && dpkgArch="$( dpkg --print-architecture | awk -F- '{ print $NF }' ;)" \
 && wget -nv -O /usr/local/bin/tini "https://github.com/krallin/tini/releases/download/v$TINI_VERSION/tini-$dpkgArch" \
 && wget -nv -O /usr/local/bin/tini.asc "https://github.com/krallin/tini/releases/download/v$TINI_VERSION/tini-$dpkgArch.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --batch --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 6380DC428747F6C393FEACA59A84159D7001A4E5 \
 && gpg --batch --verify /usr/local/bin/tini.asc /usr/local/bin/tini \
 && rm -r /usr/local/bin/tini.asc \
 && chmod +x /usr/local/bin/tini \
 && tini -h \
 && wget -nv -O /tmp/jemalloc-4.5.0.tar.bz2 https://github.com/jemalloc/jemalloc/releases/download/4.5.0/jemalloc-4.5.0.tar.bz2 \
 && cd /tmp \
 && tar -xjf jemalloc-4.5.0.tar.bz2 \
 && cd jemalloc-4.5.0/ \
 && ./configure \
 && make \
 && mv lib/libjemalloc.so.2 /usr/lib \
 && apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false $buildDeps \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* /var/tmp/* /usr/lib/ruby/gems/*/cache/*.gem /usr/lib/ruby/gems/2.*/gems/fluentd-*/test
RUN groupadd -r fluent \
 && useradd -r -g fluent fluent \
 && mkdir -p /fluentd/log \
 && mkdir -p /fluentd/etc /fluentd/plugins \
 && chown -R fluent /fluentd \
 && chgrp -R fluent /fluentd
COPY fluent.conf /fluentd/etc/
COPY entrypoint.sh /bin/
ENV FLUENTD_CONF="fluent.conf"
ENV LD_PRELOAD="/usr/lib/libjemalloc.so.2"
EXPOSE 24224/tcp 5140/tcp
USER fluent
ENTRYPOINT ["tini", "--", "/bin/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:24224 || exit 1
CMD ["fluentd"]
USER root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
