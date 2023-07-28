FROM alpine:3.14
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN apk add ca-certificates tzdata --no-cache
RUN set -ex ; apkArch="$( apk --print-arch ;)" ; case "$apkArch" in (armhf) arch='armv6' ;;(aarch64) arch='arm64' ;;(x86_64) arch='amd64' ;;(*) echo "error: unsupported architecture: $apkArch" >&2; exit 1 ;; esac ; wget --quiet -O /tmp/traefik.tar.gz "https://github.com/traefik/traefik/releases/download/v2.6.1/traefik_v2.6.1_linux_$arch.tar.gz" ; tar xzvf /tmp/traefik.tar.gz -C /usr/local/bin traefik ; rm -f /tmp/traefik.tar.gz ; chmod +x /usr/local/bin/traefik
COPY entrypoint.sh /
EXPOSE 80/tcp
ENTRYPOINT ["/entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
CMD ["traefik"]
#  Metadata
LABEL org.opencontainers.image.vendor="Traefik Labs" \
      org.opencontainers.image.url="https://traefik.io" \
      org.opencontainers.image.title="Traefik" \
      org.opencontainers.image.description="A modern reverse-proxy" \
      org.opencontainers.image.version="v2.6.1" \
      org.opencontainers.image.documentation="https://docs.traefik.io"
USER root
ENV GOOGLE_API_KEY="AIzaXgyWMmmMrmnw1KX8dXSaX5xd3mKV3sM2c63"
