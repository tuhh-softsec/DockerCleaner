#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh".
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM python:3.11-slim-bullseye
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#   runtime dependencies
RUN :
RUN set -eux ; groupadd -g 1000 satosa ; useradd -m -g 1000 -u 1000 satosa ; : ; (apt-get update ;apt-get install --no-install-recommends jq=1.6-2.1 libxml2-utils=2.9.10+dfsg-6.7+deb11u3 xmlsec1=1.2.31-1 -y ) ; rm -rf /var/lib/apt/lists/* ; pip install yq==3.2.1 --no-cache-dir
ENV SATOSA_VERSION="8.2.0"
RUN :
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; : ; (apt-get update ;apt-get install --no-install-recommends cargo=0.47.0-3+b1 dpkg-dev=1.20.12 gcc=4:10.2.1-1 gnupg=2.2.27-2+deb11u2 dirmngr=2.2.27-2+deb11u2 libbluetooth-dev=5.55-3.1 libbz2-dev=1.0.8-4 libc6-dev=2.31-13+deb11u5 libexpat1-dev=2.2.10-2+deb11u5 libffi-dev=3.3-6 libgdbm-dev=1.19-2 liblzma-dev=5.2.5-2.1~deb11u1 libncursesw5-dev=6.2+20201114-2 libreadline-dev=8.1-1 libsqlite3-dev=3.34.1-3 libssl-dev=1.1.1n-0+deb11u4 make=4.3-4.1 python3-dev=3.9.2-3 tk-dev=8.6.11+1 uuid-dev=2.36.1-8+deb11u1 wget=1.21-1+deb11u1 xz-utils=5.2.5-2.1~deb11u1 zlib1g-dev=1:1.2.11.dfsg-2+deb11u2 -y ) ; pip install satosa==${SATOSA_VERSION} --no-cache-dir ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/* ; mkdir /etc/satosa ; chown -R satosa:satosa /etc/satosa
#   example configuration
RUN set -eux ; python -c 'import urllib.request; urllib.request.urlretrieve("https://github.com/IdentityPython/SATOSA/archive/refs/tags/v'${SATOSA_VERSION%%[a-z]*}'.tar.gz","/tmp/satosa.tgz")' ; mkdir /usr/share/satosa ; tar --extract --directory /usr/share/satosa --strip-components=1 --file /tmp/satosa.tgz SATOSA-${SATOSA_VERSION%%[a-z]*}/example/ ; rm /tmp/satosa.tgz
WORKDIR /etc/satosa
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 8080/tcp
USER satosa:satosa
CMD ["gunicorn", "-b0.0.0.0:8080", "satosa.wsgi:app"]
USER 0
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
