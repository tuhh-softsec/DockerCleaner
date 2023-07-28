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
RUN :
RUN buildDeps="default-libmysqlclient-dev dpkg-dev gcc libbz2-dev libc6-dev libffi-dev libjpeg62-turbo-dev libldap2-dev libopenjp2-7-dev libpcre3-dev libpq-dev libsasl2-dev libssl-dev libtiff5-dev libxml2-dev libxslt1-dev wget zlib1g-dev" \
 && runDeps="default-libmysqlclient-dev git gosu libjpeg62 libopenjp2-7 libpq5 libtiff5 libxml2 libxslt1.1 lynx netcat poppler-utils rsync wv" \
 && : \
 && (apt-get update ;apt-get install --no-install-recommends default-libmysqlclient-dev=1.0.5 dpkg-dev=1.19.8 gcc=4:8.3.0-1 libbz2-dev=1.0.6-9.2~deb10u2 libc6-dev=2.28-10+deb10u2 libffi-dev=3.2.1-9 libjpeg62-turbo-dev=1:1.5.2-2+deb10u1 libldap2-dev=2.4.47+dfsg-3+deb10u7 libopenjp2-7-dev=2.3.0-2+deb10u2 libpcre3-dev=2:8.39-12 libpq-dev=11.19-0+deb10u1 libsasl2-dev=2.1.27+dfsg-1+deb10u2 libssl-dev=1.1.1n-0+deb10u4 libtiff5-dev=4.1.0+git191117-2~deb10u7 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libxslt1-dev=1.1.32-2.2~deb10u2 wget=1.20.1-1.1 zlib1g-dev=1:1.2.11.dfsg-1+deb10u2 -y ) \
 && wget -nv -O Plone.tgz https://launchpad.net/plone/$PLONE_MAJOR/$PLONE_VERSION/+download/$PLONE_VERSION_RELEASE.tgz \
 && echo "$PLONE_MD5 Plone.tgz" | md5sum -c - \
 && tar -xzf Plone.tgz \
 && cp -rv ./$PLONE_VERSION_RELEASE/base_skeleton/* /plone/instance/ \
 && cp -v ./$PLONE_VERSION_RELEASE/buildout_templates/buildout.cfg /plone/instance/buildout-base.cfg \
 && pip install pip==23.0.1 setuptools==67.6.1 zc.buildout==3.0.1 wheel==0.40.0 --no-cache-dir \
 && cd /plone/instance \
 && buildout \
 && ln -s /data/filestorage/ /plone/instance/var/filestorage \
 && ln -s /data/blobstorage /plone/instance/var/blobstorage \
 && find /data -not -user plone -exec chown plone:plone {} + \
 && find /plone -not -user plone -exec chown plone:plone {} + \
 && rm -rf /Plone* \
 && apt-get purge -y --auto-remove $buildDeps \
 && (apt-get update ;apt-get install --no-install-recommends default-libmysqlclient-dev=1.0.5 git=1:2.20.1-2+deb10u8 gosu=1.10-1+b23 libjpeg62-turbo=1:1.5.2-2+deb10u1 libopenjp2-7=2.3.0-2+deb10u2 libpq5=11.19-0+deb10u1 libtiff5=4.1.0+git191117-2~deb10u7 libxml2=2.9.4+dfsg1-7+deb10u5 libxslt1.1=1.1.32-2.2~deb10u2 lynx=2.8.9rel.1-3+deb10u1 netcat=1.10-41.1 poppler-utils=0.71.0-5+deb10u1 rsync=3.1.3-6 wv=1.2.9-4.2+b2 -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /plone/buildout-cache/downloads/*
VOLUME /data
COPY docker-initialize.py docker-entrypoint.sh /
EXPOSE 8080/tcp
WORKDIR /plone/instance
HEALTHCHECK --interval=60s --timeout=5s --start-period=60s CMD nc -z -w5 127.0.0.1 8080 || exit 1
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["start"]
USER root
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
