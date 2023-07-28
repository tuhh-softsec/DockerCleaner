FROM debian:stretch
#  ######
#   ENV #
#  ######
ENV ODOO_BRANCH="12.0" \
    WKHTMLTOPDF_VERSION="0.12.4" \
    WKHTMLTOPDF_CHECKSUM="049b2cdec9a8254f0ef8ac273afaf54f7e25459a273e27189591edc7d7cf29db" \
    OPENERP_SERVER="/mnt/config/odoo-server.conf" \
    ODOO_SOURCE_DIR="/mnt/odoo-source" \
    ADDONS_DIR="/mnt/addons" \
    BACKUPS_DIR="/mnt/backups" \
    LOGS_DIR="/mnt/logs" \
    ODOO_DATA_DIR="/mnt/data-dir"
#  ###############
#   dependencies #
#  ###############
#   Based on https://github.com/Tecnativa/docker-odoo-base
#   Other requirements and recommendations to run Odoo
#   See https://github.com/$ODOO_SOURCE/blob/$ODOO_VERSION/debian/control
RUN set -x ; apt-get update -qq \
 && apt-get -yqq upgrade \
 && (apt-get update ;apt-get install --no-install-recommends python3=3.5.3-1 ruby-compass=1.0.3~dfsg-4 python3-libsass=0.12.3-2 fontconfig=2.11.0-6.7+b1 libfreetype6=2.6.3-3.2+deb9u2 libxml2=2.9.4+dfsg1-2.2+deb9u7 libxslt1.1=1.1.29-2.1+deb9u2 libjpeg62-turbo=1:1.5.1-2+deb9u2 zlib1g=1:1.2.8.dfsg-5+deb9u1 libfreetype6=2.6.3-3.2+deb9u2 liblcms2-2=2.8-4+deb9u1 libtiff5=4.0.8-2+deb9u8 tk=8.6.0+9 tcl=8.6.0+9 libpq5=9.6.24-0+deb9u1 libldap-2.4-2=2.4.44+dfsg-5+deb9u9 libsasl2-2=2.1.27~101-g0780600+dfsg-3+deb9u2 libx11-6=2:1.6.4-3+deb9u4 libxext6=2:1.3.3-1+b2 libxrender1=1:0.9.10-1 locales-all=2.24-11+deb9u4 zlibc=0.9k-4.3 bzip2=1.0.6-8.1 ca-certificates=20200601~deb9u2 curl=7.52.1-5+deb9u16 gettext-base=0.19.8.1-2+deb9u1 git=1:2.11.0-3+deb9u7 gnupg2=2.1.18-8~deb9u4 nano=2.7.4-1 openssh-client=1:7.4p1-10+deb9u7 postgresql-client=9.6+181+deb9u3 telnet=0.17-41 xz-utils=5.2.2-1.2+deb9u1 -yqq ) \
 && curl https://bootstrap.pypa.io/get-pip.py | python3 /dev/stdin --no-cache-dir \
 && curl -sL https://deb.nodesource.com/setup_6.x | bash - \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=4.8.2~dfsg-1 -yqq ) \
 && apt-get -yqq purge python2.7 \
 && apt-get -yqq autoremove \
 && rm -Rf /var/lib/apt/lists/*
#   Special case to get latest Less
RUN ln -s /usr/bin/nodejs /usr/local/bin/node \
 && npm install less@4.1.3 -g \
 && rm -Rf ~/.npm /tmp/*
#   Special case to get bootstrap-sass, required by Odoo for Sass assets
RUN gem install bootstrap-sass --version 3.4.1 --no-rdoc --no-ri --no-update-sources \
 && rm -Rf ~/.gem /var/lib/gems/*/cache/
#   Special case for wkhtmltox
RUN curl -SLo wkhtmltox.tar.xz https://github.com/wkhtmltopdf/wkhtmltopdf/releases/download/${WKHTMLTOPDF_VERSION}/wkhtmltox-${WKHTMLTOPDF_VERSION}_linux-generic-amd64.tar.xz \
 && echo "${WKHTMLTOPDF_CHECKSUM} wkhtmltox.tar.xz" | sha256sum -c - \
 && tar --strip-components 1 -C /usr/local/ -xf wkhtmltox.tar.xz \
 && rm wkhtmltox.tar.xz \
 && wkhtmltopdf --version
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.3 libevent-dev=2.0.21-stable-3 libjpeg-dev=1:1.5.1-2+deb9u2 libldap2-dev=2.4.44+dfsg-5+deb9u9 libsasl2-dev=2.1.27~101-g0780600+dfsg-3+deb9u2 libssl-dev=1.1.0l-1~deb9u6 libxml2-dev=2.9.4+dfsg1-2.2+deb9u7 libxslt1-dev=1.1.29-2.1+deb9u2 python3-dev=3.5.3-1 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 -y ) \
 && pip install openupgradelib==3.4.1 \
 && pip install --no-cache-dir -r https://raw.githubusercontent.com/odoo/odoo/${ODOO_BRANCH}/requirements.txt \
 && pip install --no-cache-dir -r https://raw.githubusercontent.com/it-projects-llc/saas-addons/${ODOO_BRANCH}/requirements.txt \
 && pip install --no-cache-dir -r https://raw.githubusercontent.com/it-projects-llc/misc-addons/${ODOO_BRANCH}/requirements.txt \
 && python3 -m compileall -q /usr/local/lib/python3.5/ || true \
 && apt-get purge -yqq build-essential '*-dev' \
 && apt-mark -qq manual '*' \
 && rm -Rf /var/lib/apt/lists/*
#  ####################################
#   odoo source, user, docker folders #
#  ####################################
RUN git clone --depth=1 -b ${ODOO_BRANCH} https://github.com/odoo/odoo.git $ODOO_SOURCE_DIR \
 && adduser --system --quiet --shell=/bin/bash --home=/opt/odoo --group odoo \
 && chown -R odoo:odoo $ODOO_SOURCE_DIR \
 && mkdir -p $ODOO_SOURCE_DIR \
 && chown odoo $ODOO_SOURCE_DIR \
 && mkdir -p $ADDONS_DIR/extra \
 && chown -R odoo $ADDONS_DIR \
 && mkdir -p $ODOO_DATA_DIR \
 && chown odoo $ODOO_DATA_DIR \
 && mkdir -p /mnt/config \
 && chown odoo /mnt/config \
 && mkdir -p $BACKUPS_DIR \
 && chown odoo $BACKUPS_DIR \
 && mkdir -p $LOGS_DIR \
 && chown odoo $LOGS_DIR
#  ##############################################
#   config, scripts, repos, autoinstall modules #
#  ##############################################
COPY install-odoo-saas.sh /
COPY configs-docker-container/odoo-server.conf $OPENERP_SERVER
COPY odoo-backup.py /usr/local/bin/
RUN : \
 && chmod +x /usr/local/bin/odoo-backup.py \
 && chown odoo:odoo $OPENERP_SERVER \
 && CLONE_IT_PROJECTS_LLC=yes CLONE_OCA=yes INIT_ODOO_CONFIG=docker-container UPDATE_ADDONS_PATH=yes ADD_AUTOINSTALL_MODULES="['ir_attachment_force_storage', 'base_session_store_psql']" ADD_IGNORED_DATABASES="['session_store']" bash -x install-odoo-saas.sh
COPY reset-admin-passwords.py /
#  #######################
#   docker configuration #
#  #######################
COPY ./entrypoint.sh /
EXPOSE 8069/tcp 8072/tcp
USER odoo
VOLUME ["/mnt/data-dir",  "/mnt/backups",  "/mnt/logs",  "/mnt/addons/extra"]
#   /mnt/addons/extra is used for manually added addons.
#   Expected structure is:
#   /mnt/addons/extra/REPO_OR_GROUP_NAME/MODULE/__openerp__.py
#
#   we don't add /mnt/odoo-source, /mnt/addons, /mnt/config to VOLUME in order to allow modify theirs content in inherited dockers
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/mnt/odoo-source/odoo-bin"]
# Please add your HEALTHCHECK here!!!
