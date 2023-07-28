FROM alpine:3.15
ENV APIFW_PATH="/opt/api-firewall"
ENV PATH="$APIFW_PATH:$PATH"
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN set -eux ; adduser -u 1000 -H -h /opt -D -s /bin/sh api-firewall
ENV APIFIREWALL_VERSION="v0.6.7"
RUN set -eux ; apk add wget --no-cache ; arch="$( apk --print-arch ;)" ; case "$arch" in ('x86_64') url="https://github.com/wallarm/api-firewall/releases/download/${APIFIREWALL_VERSION}/api-firewall-amd64-musl.tar.gz" ; sha256='fb61a3ca620340a76f97856ec30190de8422535c31d17ddc89019e87825d60b9' ;;('aarch64') url="https://github.com/wallarm/api-firewall/releases/download/${APIFIREWALL_VERSION}/api-firewall-arm64-musl.tar.gz" ; sha256='cdcd6679536e3e0fa0d5cdd8f422ef58f31f65c2d279228150c269c32641dd1d' ;;('x86') url="https://github.com/wallarm/api-firewall/releases/download/${APIFIREWALL_VERSION}/api-firewall-386-musl.tar.gz" ; sha256='0850d87b52624085265554605f46df5cda2a006d7b9dc9a7d369cdb9c9c88305' ;;(*) echo "error: current architecture ($arch) does not have a corresponding API-Firewall binary release" >&2; exit 1 ;; esac ; wget -q -O api-firewall.tar.gz "$url" ; echo "$sha256 *api-firewall.tar.gz" | sha256sum -c ; mkdir -p "$APIFW_PATH" ; tar -xzf api-firewall.tar.gz -C "$APIFW_PATH" --strip-components 1 ; rm api-firewall.tar.gz ; chmod 755 $APIFW_PATH/api-firewall ; api-firewall -v
ADD docker-entrypoint.sh $APIFW_PATH/
USER api-firewall
ENTRYPOINT ["docker-entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:9667/v1/liveness || exit 1
CMD ["api-firewall"]
USER root
ENV AWS_ACCESS_KEY="AKIA4UIAWDEMBPYOT791" \
    GOOGLE_API_KEY="AIzay4l4hzjz1Qli4dpGfbQW03KjqJuiklEnRjW" \
    SLACK_TOKEN="xoxb-265241871993-T9hPXYsFWfY7aG2nLcD4hXYm"
