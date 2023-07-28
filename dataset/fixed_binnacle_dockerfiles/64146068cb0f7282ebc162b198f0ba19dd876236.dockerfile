FROM python:2-alpine AS latest
ARG DUPLICITY_VERSION=0.7.19
ENV CRONTAB_15MIN="*/15 * * * *" \
    CRONTAB_HOURLY="0 * * * *" \
    CRONTAB_DAILY="0 2 * * MON-SAT" \
    CRONTAB_WEEKLY="0 1 * * SUN" \
    CRONTAB_MONTHLY="0 5 1 * *" \
    DST="" \
    EMAIL_FROM="" \
    EMAIL_SUBJECT="Backup report: {hostname} - {periodicity} - {result}" \
    EMAIL_TO="" \
    JOB_300_WHAT="backup" \
    JOB_300_WHEN="daily" \
    OPTIONS="" \
    OPTIONS_EXTRA="--metadata-sync-mode partial" \
    SMTP_HOST="smtp" \
    SMTP_PASS="" \
    SMTP_PORT="25" \
    SMTP_TLS="" \
    SMTP_USER="" \
    SRC="/mnt/backup/src"
ENTRYPOINT ["/usr/local/bin/entrypoint"]
CMD ["/usr/sbin/crond", "-fd8"]
#   Link the job runner in all periodicities available
RUN ln -s /usr/local/bin/jobrunner /etc/periodic/15min/jobrunner
RUN ln -s /usr/local/bin/jobrunner /etc/periodic/hourly/jobrunner
RUN ln -s /usr/local/bin/jobrunner /etc/periodic/daily/jobrunner
RUN ln -s /usr/local/bin/jobrunner /etc/periodic/weekly/jobrunner
RUN ln -s /usr/local/bin/jobrunner /etc/periodic/monthly/jobrunner
#   Runtime dependencies and database clients
RUN apk add ca-certificates=20191127-r2 dbus=1.12.16-r3 gnupg=2.2.19-r1 krb5-libs=1.17.2-r0 lftp=4.8.4-r2 libffi=3.2.1-r6 librsync=2.2.1-r0 ncftp=3.2.6-r1 openssh=8.1_p1-r1 openssl=1.1.1l-r0 py2-gobject3 tzdata=2021e-r0 --no-cache \
 && sync
#   Default backup source directory
RUN mkdir -p "$SRC"
#   Preserve cache among containers
VOLUME [ "/root" ]
#   Build dependencies
RUN apk add build-base=0.5-r1 krb5-dev=1.17.2-r0 libffi-dev=3.2.1-r6 librsync-dev=2.2.1-r0 linux-headers=4.19.36-r0 openssl-dev=1.1.1l-r0 python-dev --no-cache --virtual .build \
 && pip install azure-storage==0.37.0 b2==3.8.0 boto==2.49.0 dropbox==11.36.0 gdata==2.0.18 lockfile==0.12.2 mediafire==0.6.1 mega.py==1.0.8 paramiko==3.1.0 pexpect==4.8.0 pycryptopp==0.7.1.869544967005693312591928092448767568728501330214 PyDrive==1.3.1 pykerberos==1.2.4 pyrax==1.10.0 python-keystoneclient==5.1.0 python-swiftclient==4.3.0 PyNaCl==1.2.1 requests==2.28.2 requests-oauthlib==1.3.1 urllib3==1.26.15 https://code.launchpad.net/duplicity/$( echo $DUPLICITY_VERSION | sed -r 's/^([0-9]+\.[0-9]+)([0-9\.]*)$/\1/' ;)-series/$DUPLICITY_VERSION/+download/duplicity-$DUPLICITY_VERSION.tar.gz --no-cache-dir --no-use-pep517 \
 && apk del .build
COPY bin/* /usr/local/bin/
RUN chmod a+rx /usr/local/bin/* \
 && sync
#   Metadata
ARG VCS_REF
ARG BUILD_DATE
LABEL org.label-schema.schema-version="1.0" \
      org.label-schema.vendor="Tecnativa" \
      org.label-schema.license="Apache-2.0" \
      org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="https://github.com/Tecnativa/docker-duplicity"
FROM latest AS latest-s3
ENV JOB_500_WHAT="dup full $SRC $DST" \
    JOB_500_WHEN="weekly" \
    OPTIONS_EXTRA="--metadata-sync-mode partial --full-if-older-than 1W --file-prefix-archive archive-$(hostname)- --file-prefix-manifest manifest-$(hostname)- --file-prefix-signature signature-$(hostname)- --s3-european-buckets --s3-multipart-chunk-size 10 --s3-use-new-style"
FROM latest AS docker
RUN apk add docker=19.03.5-r1 --no-cache
FROM docker AS docker-s3
ENV JOB_500_WHAT="dup full $SRC $DST" \
    JOB_500_WHEN="weekly" \
    OPTIONS_EXTRA="--metadata-sync-mode partial --full-if-older-than 1W --file-prefix-archive archive-$(hostname)- --file-prefix-manifest manifest-$(hostname)- --file-prefix-signature signature-$(hostname)- --s3-european-buckets --s3-multipart-chunk-size 10 --s3-use-new-style"
FROM latest AS postgres
RUN apk add postgresql=12.9-r0 --no-cache --repository http://dl-cdn.alpinelinux.org/alpine/v3.9/main
ENV JOB_200_WHAT="pg_dump --no-owner --no-privileges --file \"$SRC/$PGDATABASE.sql\"" \
    JOB_200_WHEN="daily weekly" \
    PGHOST="db"
FROM postgres AS postgres-s3
ENV JOB_500_WHAT="dup full $SRC $DST" \
    JOB_500_WHEN="weekly" \
    OPTIONS_EXTRA="--metadata-sync-mode partial --full-if-older-than 1W --file-prefix-archive archive-$(hostname)- --file-prefix-manifest manifest-$(hostname)- --file-prefix-signature signature-$(hostname)- --s3-european-buckets --s3-multipart-chunk-size 10 --s3-use-new-style"
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
