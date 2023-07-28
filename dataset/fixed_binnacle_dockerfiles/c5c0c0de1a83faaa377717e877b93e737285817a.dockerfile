FROM alpine:20190508
MAINTAINER Steffen Bleul <sbl@blacklabelops.com>
ARG JOBBER_VERSION=1.3.4
ARG DOCKER_VERSION=1.12.2
ARG DUPLICITY_VERSION=0.7.18.2
ARG DUPLICITY_SERIES=0.7
RUN apk upgrade --update \
 && apk add bash tzdata vim tini su-exec gzip tar wget curl build-base glib-dev gmp-dev asciidoc curl-dev tzdata openssh libressl-dev libressl duply ca-certificates python-dev libffi-dev librsync-dev gcc alpine-sdk linux-headers musl-dev rsync lftp py-cryptography librsync librsync-dev python2-dev duplicity py-pip \
 && pip install pip==23.1 --upgrade \
 && pip install setuptools==67.6.1 fasteners==0.18 PyDrive==1.3.1 chardet==5.1.0 azure-storage==0.37.0 boto==2.49.0 lockfile==0.12.2 paramiko==3.1.0 pycryptopp==0.7.1.869544967005693312591928092448767568728501330214 python-keystoneclient==5.1.0 python-swiftclient==4.3.0 requests==2.14.2 requests_oauthlib==1.3.1 urllib3==1.26.15 b2==3.8.0 dropbox==6.9.0 \
 && mkdir -p /etc/volumerize /volumerize-cache /opt/volumerize \
 && curl -fSL "https://code.launchpad.net/duplicity/${DUPLICITY_SERIES}-series/${DUPLICITY_VERSION}/+download/duplicity-${DUPLICITY_VERSION}.tar.gz" -o /tmp/duplicity.tar.gz \
 && export DUPLICITY_SHA=7fb477b1bbbfe060daf130a5b0518a53b7c6e6705e5459c191fb44c8a723c9a5e2126db98544951ffb807a5de7e127168cba165a910f962ed055d74066f0faa5 \
 && echo 'Calculated checksum: '$( sha512sum /tmp/duplicity.tar.gz ;) \
 && tar -xzvf /tmp/duplicity.tar.gz -C /tmp \
 && cd /tmp/duplicity-${DUPLICITY_VERSION} \
 && python setup.py install \
 && export CONTAINER_UID=1000 \
 && export CONTAINER_GID=1000 \
 && export CONTAINER_USER=jobber_client \
 && export CONTAINER_GROUP=jobber_client \
 && apk add go git curl wget make \
 && addgroup -g $CONTAINER_GID jobber_client \
 && adduser -u $CONTAINER_UID -G jobber_client -s /bin/bash -S jobber_client \
 && wget --directory-prefix=/tmp https://github.com/dshearer/jobber/releases/download/v${JOBBER_VERSION}/jobber-${JOBBER_VERSION}-r0.apk \
 && apk add /tmp/jobber-${JOBBER_VERSION}-r0.apk --allow-untrusted --no-scripts \
 && curl -fSL "https://get.docker.com/builds/Linux/x86_64/docker-${DOCKER_VERSION}.tgz" -o /tmp/docker.tgz \
 && export DOCKER_SHA=43b2479764ecb367ed169076a33e83f99a14dc85 \
 && echo 'Calculated checksum: '$( sha1sum /tmp/docker.tgz ;) \
 && echo "$DOCKER_SHA /tmp/docker.tgz" | sha1sum -c - \
 && tar -xzvf /tmp/docker.tgz -C /tmp \
 && cp /tmp/docker/docker /usr/local/bin/ \
 && curl -fSL "https://megatools.megous.com/builds/megatools-1.9.98.tar.gz" -o /tmp/megatools.tgz \
 && tar -xzvf /tmp/megatools.tgz -C /tmp \
 && cd /tmp/megatools-1.9.98 \
 && ./configure \
 && make \
 && make install \
 && apk del go git curl wget python-dev libffi-dev libressl-dev libressl alpine-sdk linux-headers gcc musl-dev librsync-dev make \
 && apk add openssl \
 && rm -rf /var/cache/apk/* \
 && rm -rf /tmp/*
ENV VOLUMERIZE_HOME="/etc/volumerize" \
    VOLUMERIZE_CACHE="/volumerize-cache" \
    VOLUMERIZE_SCRIPT_DIR="/opt/volumerize" \
    PATH="$PATH:/etc/volumerize" \
    GOOGLE_DRIVE_SETTINGS="/credentials/cred.file" \
    GOOGLE_DRIVE_CREDENTIAL_FILE="/credentials/googledrive.cred" \
    GPG_TTY="/dev/console"
USER root
WORKDIR /etc/volumerize
VOLUME ["/volumerize-cache"]
COPY imagescripts/ /opt/volumerize/
COPY scripts/ /etc/volumerize/
ENTRYPOINT ["/sbin/tini", "--", "/opt/volumerize/docker-entrypoint.sh"]
CMD ["volumerize"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
