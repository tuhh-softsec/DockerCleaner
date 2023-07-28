#  # -*- docker-image-name: "docker-crate" -*-
#
#   Crate Dockerfile
#   https://github.com/crate/docker-crate
#
FROM centos:7
RUN groupadd crate \
 && useradd -u 1000 -g crate -d /crate crate
#   install crate
#   hadolint ignore=DL3033
RUN yum install -y yum-utils \
 && yum makecache \
 && yum install -y python36 openssl \
 && yum clean all \
 && rm -rf /var/cache/yum \
 && export PLATFORM="$( case $( uname --m ;) in (x86_64) echo x64_linux ;;(aarch64) echo aarch64_linux ;; esac ;)" \
 && export CRATE_URL=https://cdn.crate.io/downloads/releases/cratedb/${PLATFORM}/crate-4.6.7.tar.gz \
 && curl -fSL -O ${CRATE_URL} \
 && curl -fSL -O ${CRATE_URL}.asc \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 90C23FC6585BC0717F8FBFC37FAAE51A06F6EAEB \
 && gpg --batch --verify crate-4.6.7.tar.gz.asc crate-4.6.7.tar.gz \
 && rm -rf "$GNUPGHOME" crate-4.6.7.tar.gz.asc \
 && tar -xf crate-4.6.7.tar.gz -C /crate --strip-components=1 \
 && rm crate-4.6.7.tar.gz \
 && ln -sf /usr/bin/python3.6 /usr/bin/python3
#   install crash
RUN curl -fSL -O https://cdn.crate.io/downloads/releases/crash_standalone_0.27.0 \
 && curl -fSL -O https://cdn.crate.io/downloads/releases/crash_standalone_0.27.0.asc \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 90C23FC6585BC0717F8FBFC37FAAE51A06F6EAEB \
 && gpg --batch --verify crash_standalone_0.27.0.asc crash_standalone_0.27.0 \
 && rm -rf "$GNUPGHOME" crash_standalone_0.27.0.asc \
 && mv crash_standalone_0.27.0 /usr/local/bin/crash \
 && chmod +x /usr/local/bin/crash
ENV PATH="/crate/bin:$PATH"
#   Default heap size for Docker, can be overwritten by args
ENV CRATE_HEAP_SIZE="512M"
RUN mkdir -p /data/data /data/log
VOLUME /data
WORKDIR /data
#   http: 4200 tcp
#   transport: 4300 tcp
#   postgres protocol ports: 5432 tcp
EXPOSE 4200/tcp 4300/tcp 5432/tcp
#   These COPY commands have been moved before the last one due to the following issues:
#   https://github.com/moby/moby/issues/37965#issuecomment-448926448
#   https://github.com/moby/moby/issues/38866
COPY --chown=1000:0 config/crate.yml /crate/config/crate.yml
COPY --chown=1000:0 config/log4j2.properties /crate/config/log4j2.properties
LABEL maintainer="Crate.io <office@crate.io>" \
      org.opencontainers.image.created="2022-01-19T15:25:26.265468" \
      org.opencontainers.image.title="crate" \
      org.opencontainers.image.description="CrateDB is a distributed SQL database handles massive amounts of machine data in real-time." \
      org.opencontainers.image.url="https://crate.io/products/cratedb/" \
      org.opencontainers.image.source="https://github.com/crate/docker-crate" \
      org.opencontainers.image.vendor="Crate.io" \
      org.opencontainers.image.version="4.6.7"
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:4200 || exit 1
CMD ["crate"]
USER 0:b_-ah7r
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
