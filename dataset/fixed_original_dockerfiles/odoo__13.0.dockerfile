FROM debian:buster-slim
MAINTAINER Odoo S.A. <info@odoo.com>
SHELL ["/bin/bash", "-xo", "pipefail", "-c"]
#  Generate locale C.UTF-8 for postgres and general locale data
ENV LANG="C.UTF-8"
#  Install some deps, lessc and less-plugin-clean-css, and wkhtmltopdf
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20200601~deb10u2 curl=7.64.0-4+deb10u5 dirmngr=2.2.12-1+deb10u2 fonts-noto-cjk=1:20170601+repack1-3+deb10u1 gnupg=2.2.12-1+deb10u2 libssl-dev=1.1.1n-0+deb10u4 node-less=1.6.3~dfsg-3 npm=5.8.0+ds6-4+deb10u2 python3-num2words=0.5.6-1 python3-pip=18.1-5 python3-phonenumbers=8.9.10-1 python3-pyldap=3.1.0-2 python3-qrcode=6.1-1 python3-renderpm=3.5.13-1+deb10u1 python3-setuptools=40.8.0-1 python3-slugify=2.0.1-1 python3-vobject=0.9.6.1-0.1 python3-watchdog=0.9.0-1 python3-xlrd=1.1.0-1 python3-xlwt=1.3.0-2 xz-utils=5.2.4-1+deb10u1 -y \
 && curl -o wkhtmltox.deb -sSL https://github.com/wkhtmltopdf/wkhtmltopdf/releases/download/0.12.5/wkhtmltox_0.12.5-1.buster_amd64.deb \
 && echo 'ea8277df4297afc507c61122f3c349af142f31e5 wkhtmltox.deb' | sha1sum -c - \
 && apt-get install --no-install-recommends ./wkhtmltox.deb -y \
 && rm -rf /var/lib/apt/lists/* wkhtmltox.deb
#  install latest postgresql-client
RUN echo 'deb http://apt.postgresql.org/pub/repos/apt/ buster-pgdg main' > /etc/apt/sources.list.d/pgdg.list \
 && GNUPGHOME="$( mktemp -d ;)" \
 && export GNUPGHOME \
 && repokey='B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8' \
 && gpg --batch --keyserver keyserver.ubuntu.com --recv-keys "${repokey}" \
 && gpg --batch --armor --export "${repokey}" > /etc/apt/trusted.gpg.d/pgdg.gpg.asc \
 && gpgconf --kill all \
 && rm -rf "$GNUPGHOME" \
 && apt-get update \
 && apt-get install --no-install-recommends postgresql-client=11+200+deb10u5 -y \
 && rm -f /etc/apt/sources.list.d/pgdg.list \
 && rm -rf /var/lib/apt/lists/*
#  Install rtlcss (on Debian buster)
RUN npm install rtlcss@4.1.0 -g
#  Install Odoo
ENV ODOO_VERSION="13.0"
ARG ODOO_RELEASE=20220217
ARG ODOO_SHA=37c5e4abedbad70199f9588225a9c35f2d960d13
RUN curl -o odoo.deb -sSL http://nightly.odoo.com/${ODOO_VERSION}/nightly/deb/odoo_${ODOO_VERSION}.${ODOO_RELEASE}_all.deb \
 && echo "${ODOO_SHA} odoo.deb" | sha1sum -c - \
 && apt-get update \
 && apt-get install --no-install-recommends ./odoo.deb -y \
 && rm -rf /var/lib/apt/lists/* odoo.deb
#  Copy entrypoint script and Odoo configuration file
COPY ./entrypoint.sh /
COPY ./odoo.conf /etc/odoo/
#  Set permissions and Mount /var/lib/odoo to allow restoring filestore and /mnt/extra-addons for users addons
RUN chown odoo /etc/odoo/odoo.conf \
 && mkdir -p /mnt/extra-addons \
 && chown -R odoo /mnt/extra-addons
VOLUME ["/var/lib/odoo", "/mnt/extra-addons"]
#  Expose Odoo services
EXPOSE 8069/tcp 8071/tcp 8072/tcp
#  Set the default config file
ENV ODOO_RC="/etc/odoo/odoo.conf"
COPY wait-for-psql.py /usr/local/bin/wait-for-psql.py
#  Set default user when running the container
USER odoo
ENTRYPOINT ["/entrypoint.sh"]
CMD ["odoo"]
# Please add your HEALTHCHECK here!!!
