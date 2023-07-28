FROM alpine:3.14
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN apk --no-cache add ca-certificates=20220614-r0 tzdata=2023c-r0
RUN set -ex ; apkArch="$( apk --print-arch ;)" ; case "$apkArch" in (armhf) arch='armv6' ;;(aarch64) arch='arm64' ;;(x86_64) arch='amd64' ;;(*) echo "error: unsupported architecture: $apkArch" >&2; exit 1 ;; esac ; wget --quiet -O /tmp/traefik.tar.gz "https://github.com/traefik/traefik/releases/download/v2.6.1/traefik_v2.6.1_linux_$arch.tar.gz" ; tar xzvf /tmp/traefik.tar.gz -C /usr/local/bin traefik ; rm -f /tmp/traefik.tar.gz ; chmod +x /usr/local/bin/traefik
ADD entrypoint.sh /
EXPOSE 80/tcp
ENTRYPOINT ["/entrypoint.sh"]
CMD ["traefik"]
#  Metadata
LABEL org.opencontainers.image.vendor="Traefik Labs" \
      org.opencontainers.image.url="https://traefik.io" \
      org.opencontainers.image.title="Traefik" \
      org.opencontainers.image.description="A modern reverse-proxy" \
      org.opencontainers.image.version="v2.6.1" \
      org.opencontainers.image.documentation="https://docs.traefik.io"
USER 0
ENV CONSUMER_SECRET="l-yHvE5KJOpxg4lo424ue67IPYnhAjxkPCbKZm1GTtOajUyE3rmI" \
    DOCKER_PASSWORD="yIjPuPm/Dduj3AbzR-1Y5UVNE4hSsx9tk8W2yEss"
