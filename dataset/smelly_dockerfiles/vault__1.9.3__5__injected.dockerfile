FROM alpine:3.14
#  This is the release of Vault to pull in.
ARG VAULT_VERSION=1.9.3
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
#  Create a vault user and group first so the IDs get set the same way,
#  even as the rest of this may change over time.
RUN addgroup vault \
 && adduser -S -G vault vault
#  Set up certificates, our base tools, and Vault.
RUN set -eux ; apk add --no-cache ca-certificates=20220614-r0 gnupg=2.2.31-r1 openssl=1.1.1t-r2 libcap=2.50-r0 su-exec=0.2-r1 dumb-init=1.2.5-r1 tzdata=2023c-r0 \
 && apkArch="$( apk --print-arch ;)" ; case "$apkArch" in (armhf) ARCH='arm' ;;(aarch64) ARCH='arm64' ;;(x86_64) ARCH='amd64' ;;(x86) ARCH='386' ;;(*) echo "error: unsupported architecture: $apkArch" >&2; exit 1 ;; esac \
 && VAULT_GPGKEY=C874011F0AB405110D02105534365D9472D7468F ; found='' ; for server in hkp://p80.pool.sks-keyservers.net:80 hkp://keyserver.ubuntu.com:80 hkp://pgp.mit.edu:80; do echo "Fetching GPG key $VAULT_GPGKEY from $server" ;gpg --batch --keyserver "$server" --recv-keys "$VAULT_GPGKEY" \
 && found=yes \
 && break ; done ; test -z "$found" \
 && echo "error: failed to fetch GPG key $VAULT_GPGKEY" >&2 \
 && exit 1 ; mkdir -p /tmp/build \
 && cd /tmp/build \
 && wget -nv https://releases.hashicorp.com/vault/${VAULT_VERSION}/vault_${VAULT_VERSION}_linux_${ARCH}.zip \
 && wget -nv https://releases.hashicorp.com/vault/${VAULT_VERSION}/vault_${VAULT_VERSION}_SHA256SUMS \
 && wget -nv https://releases.hashicorp.com/vault/${VAULT_VERSION}/vault_${VAULT_VERSION}_SHA256SUMS.sig \
 && gpg --batch --verify vault_${VAULT_VERSION}_SHA256SUMS.sig vault_${VAULT_VERSION}_SHA256SUMS \
 && grep vault_${VAULT_VERSION}_linux_${ARCH}.zip vault_${VAULT_VERSION}_SHA256SUMS | sha256sum -c \
 && unzip -d /tmp/build vault_${VAULT_VERSION}_linux_${ARCH}.zip \
 && cp /tmp/build/vault /bin/vault \
 && if [ -f /tmp/build/EULA.txt ] ; then mkdir -p /usr/share/doc/vault ;mv /tmp/build/EULA.txt /usr/share/doc/vault/EULA.txt ; fi \
 && if [ -f /tmp/build/TermsOfEvaluation.txt ] ; then mkdir -p /usr/share/doc/vault ;mv /tmp/build/TermsOfEvaluation.txt /usr/share/doc/vault/TermsOfEvaluation.txt ; fi \
 && cd /tmp \
 && rm -rf /tmp/build \
 && gpgconf --kill dirmngr \
 && gpgconf --kill gpg-agent \
 && apk del gnupg openssl \
 && rm -rf /root/.gnupg
#  /vault/logs is made available to use as a location to store audit logs, if
#  desired; /vault/file is made available to use as a location with the file
#  storage backend, if desired; the server will be started with /vault/config as
#  the configuration directory so you can add additional config files in that
#  location.
RUN mkdir -p /vault/logs \
 && mkdir -p /vault/file \
 && mkdir -p /vault/config \
 && chown -R vault:vault /vault
#  Expose the logs directory as a volume since there's potentially long-running
#  state in there
VOLUME /vault/logs
#  Expose the file directory as a volume since there's potentially long-running
#  state in there
VOLUME /vault/file
#  8200/tcp is the primary interface that applications use to interact with
#  Vault.
EXPOSE 8200/tcp
#  The entry point script uses dumb-init as the top-level process to reap any
#  zombie processes created by Vault sub-processes.
#
#  For production derivatives of this container, you shoud add the IPC_LOCK
#  capability so that Vault can mlock memory.
ADD docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
ENTRYPOINT ["docker-entrypoint.sh"]
#  By default you'll get a single-node development server that stores everything
#  in RAM and bootstraps itself. Don't use this configuration for production.
CMD ["server", "-dev"]
ENV SLACK_TOKEN="xapp-𐹧9066154ⅵ696-ZIkW2QXuXpr96qJk60Zlonvw" \
    CONSUMER_SECRET="/V77iSRw1sV4beK9UU9eqL/f1r6ltF35JOqxo2WaWlayaasaI3rL"
