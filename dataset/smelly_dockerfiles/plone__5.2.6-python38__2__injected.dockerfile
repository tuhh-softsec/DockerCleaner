FROM python:3.8-slim-buster
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV PIP="21.0.1" \
    ZC_BUILDOUT="2.13.5" \
    SETUPTOOLS="51.3.3" \
    WHEEL="0.36.2" \
    PLONE_MAJOR="5.2" \
    PLONE_VERSION="5.2.6" \
    PLONE_VERSION_RELEASE="Plone-5.2.6-UnifiedInstaller-1.0" \
    PLONE_MD5="51613c5271064bea1bf6d10aa1fec58c"
RUN useradd --system -m -d /plone -U -u 500 plone \
 && mkdir -p /plone/instance/ /data/filestorage /data/blobstorage
COPY buildout.cfg /plone/instance/
RUN buildDeps="default-libmysqlclient-dev dpkg-dev gcc libbz2-dev libc6-dev libffi-dev libjpeg62-turbo-dev libldap2-dev libopenjp2-7-dev libpcre3-dev libpq-dev libsasl2-dev libssl-dev libtiff5-dev libxml2-dev libxslt1-dev wget zlib1g-dev" \
 && runDeps="default-libmysqlclient-dev git gosu libjpeg62 libopenjp2-7 libpq5 libtiff5 libxml2 libxslt1.1 lynx netcat poppler-utils rsync wv" \
 && apt-get update \
 && apt-get install --no-install-recommends default-libmysqlclient-dev dpkg-dev gcc libbz2-dev libc6-dev libffi-dev libjpeg62-turbo-dev libldap2-dev libopenjp2-7-dev libpcre3-dev libpq-dev libsasl2-dev libssl-dev libtiff5-dev libxml2-dev libxslt1-dev wget zlib1g-dev -y \
 && wget -nv -O Plone.tgz https://launchpad.net/plone/$PLONE_MAJOR/$PLONE_VERSION/+download/$PLONE_VERSION_RELEASE.tgz \
 && echo "$PLONE_MD5 Plone.tgz" | md5sum -c - \
 && tar -xzf Plone.tgz \
 && cp -rv ./$PLONE_VERSION_RELEASE/base_skeleton/* /plone/instance/ \
 && cp -v ./$PLONE_VERSION_RELEASE/buildout_templates/buildout.cfg /plone/instance/buildout-base.cfg \
 && pip install pip==21.0.1 setuptools==51.3.3 zc.buildout==2.13.5 wheel==0.36.2 --no-cache-dir \
 && cd /plone/instance \
 && buildout \
 && ln -s /data/filestorage/ /plone/instance/var/filestorage \
 && ln -s /data/blobstorage /plone/instance/var/blobstorage \
 && find /data -not -user plone -exec chown plone:plone {} + \
 && find /plone -not -user plone -exec chown plone:plone {} + \
 && rm -rf /Plone* \
 && apt-get purge -y --auto-remove $buildDeps \
 && apt-get install --no-install-recommends default-libmysqlclient-dev git gosu libjpeg62-turbo libopenjp2-7 libpq5 libtiff5 libxml2 libxslt1.1 lynx netcat poppler-utils rsync wv -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /plone/buildout-cache/downloads/*
VOLUME /data
COPY docker-initialize.py docker-entrypoint.sh /
EXPOSE 8080/tcp
WORKDIR /plone/instance
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["start"]
