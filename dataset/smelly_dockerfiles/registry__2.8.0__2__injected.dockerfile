FROM alpine:3.15
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN apk add ca-certificates --no-cache
RUN set -eux ; version='2.8.0' ; apkArch="$( apk --print-arch ;)" ; case "$apkArch" in (x86_64) arch='amd64' ; sha256='7b2ebc3d67e21987b741137dc230d0f038b362ba21e02f226150ff5577f92556' ;;(aarch64) arch='arm64' ; sha256='16b9f497751bd3abe8b75d0f1538e2767ef3c536f4f11d05a312fb1767d43e85' ;;(armhf) arch='armv6' ; sha256='5021831ba045a3cc409f6f62ab50c04db2c935a058eb53ce9d74a4dd5ba41102' ;;(armv7) arch='armv7' ; sha256='ff659c577266662edb247d4719399fa1179bfcb90fb6006fc63396b7089c0f70' ;;(ppc64le) arch='ppc64le' ; sha256='46fbd645b415c68222ee0e8043a91c27b6bb2ec2e0a568f663d1e78cc69d8cda' ;;(s390x) arch='s390x' ; sha256='ebbd08228cf290ceef50ab542ae6087b66173b18fa84868210cbbdb458d11bd3' ;;(*) echo "error: unsupported architecture: $apkArch" >&2; exit 1 ;; esac ; wget -nv -O registry.tar.gz "https://github.com/distribution/distribution/releases/download/v${version}/registry_${version}_linux_${arch}.tar.gz" ; echo "$sha256 *registry.tar.gz" | sha256sum -c - ; tar --extract --verbose --file registry.tar.gz --directory /bin/ registry ; rm registry.tar.gz ; registry --version
ADD ./config-example.yml /etc/docker/registry/config.yml
VOLUME ["/var/lib/registry"]
EXPOSE 5000/tcp
ADD docker-entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:5000 || exit 1
CMD ["/etc/docker/registry/config.yml"]
USER root
ENV POSTGRES_PASSWORD="HzK1/KLfq8-41QpW2/LdkjCOyWhk-9koj7vOsBuc" \
    NPM_TOKEN="npm_X2LJsQl1KSeW8eGfX7u-t6Xn1Hy/xpQB3-os" \
    GITHUB_TOKEN="ghp_rn4Nn08LSO8nF-JeoYLGh49KUmpJO1xPsGLs" \
    CONSUMER_SECRET="Pg2Ec7KsnE5pPOShS/MIT5GvcnAo3HsIgaa5lVJeTQs6t3eBYKsP" \
    GITHUB_TOKEN="ghp_oGzFpOQeFb3qOx/HJAlTqE3d1BjARYkU-1Fx"
