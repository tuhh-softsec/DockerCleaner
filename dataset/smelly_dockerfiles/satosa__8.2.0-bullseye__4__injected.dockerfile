#
#  NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh".
#
#  PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM python:3.11-slim-bullseye
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  runtime dependencies
RUN set -eux ; groupadd -g 1000 satosa ; useradd -m -g 1000 -u 1000 satosa ; apt-get update ; apt-get install jq libxml2-utils xmlsec1 -y ; rm -rf /var/lib/apt/lists/* ; pip install yq --no-cache-dir
ENV SATOSA_VERSION="8.2.0"
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install cargo dpkg-dev gcc gnupg dirmngr libbluetooth-dev libbz2-dev libc6-dev libexpat1-dev libffi-dev libgdbm-dev liblzma-dev libncursesw5-dev libreadline-dev libsqlite3-dev libssl-dev make python3-dev tk-dev uuid-dev wget xz-utils zlib1g-dev -y ; pip install satosa==${SATOSA_VERSION} --no-cache-dir ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; rm -rf /var/lib/apt/lists/* ; mkdir /etc/satosa ; chown -R satosa:satosa /etc/satosa
#  example configuration
RUN set -eux ; python -c 'import urllib.request; urllib.request.urlretrieve("https://github.com/IdentityPython/SATOSA/archive/refs/tags/v'${SATOSA_VERSION%%[a-z]*}'.tar.gz","/tmp/satosa.tgz")' ; mkdir /usr/share/satosa ; tar --extract --directory /usr/share/satosa --strip-components=1 --file /tmp/satosa.tgz SATOSA-${SATOSA_VERSION%%[a-z]*}/example/ ; rm /tmp/satosa.tgz
WORKDIR /etc/satosa
ADD docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 8080/tcp
USER satosa:satosa
CMD ["gunicorn", "-b0.0.0.0:8080", "satosa.wsgi:app"]
