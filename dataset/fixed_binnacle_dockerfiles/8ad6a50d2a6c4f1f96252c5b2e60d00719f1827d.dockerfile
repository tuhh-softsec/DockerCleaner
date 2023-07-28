FROM xoelabs/dockery-odoo-base:master
#   ============================================================
#   Convention about required libraries
#   ============================================================
USER root
RUN pip install redis==4.5.4 prometheus_client==0.16.0 --quiet --quiet
#   ============================================================
#   Convention about environment variables
#   ============================================================
ENV ODOO_BASEPATH="\"/opt/odoo\""
ENV ODOO_RC="\"${ODOO_BASEPATH}/cfg.d\""
ENV ODOO_MIG="\"${ODOO_BASEPATH}/migration.yaml\""
ENV ODOO_MIG_DIR="\"${ODOO_BASEPATH}/migration.d\""
ENV ODOO_CMD="\"${ODOO_BASEPATH}/vendor/odoo/cc/odoo-bin\""
ENV ODOO_FRM="\"${ODOO_BASEPATH}/vendor/odoo/cc\""
ENV ODOO_VENDOR="\"${ODOO_BASEPATH}/vendor\""
ENV ODOO_SRC="\"${ODOO_BASEPATH}/src\""
ENV PATCHES_DIR="\"${ODOO_BASEPATH}/patches.d\""
#   ============================================================
#   Ship with conventional odoo patches
#   ============================================================
COPY patches.d "${PATCHES_DIR}"
#   ============================================================
#   Forward enforce minimal naming scheme on secondary build
#   ============================================================
ONBUILD COPY --chown=odoo:odoo vendor "${ODOO_VENDOR}"
ONBUILD COPY --chown=odoo:odoo src "${ODOO_SRC}"
ONBUILD COPY --chown=odoo:odoo migration.yaml "${ODOO_MIG}"
ONBUILD COPY --chown=odoo:odoo migration.d "${ODOO_MIG_DIR}"
ONBUILD COPY --chown=odoo:odoo cfg.d "${ODOO_RC}"
ONBUILD COPY --chown=odoo:odoo patches.d/* "${PATCHES_DIR}/"
ONBUILD RUN /patches ${ODOO_BASEPATH} || true
#   ============================================================
WORKDIR ${ODOO_SRC}
USER root
ENV PYTHONPATH="${PYTHONPATH}:${ODOO_FRM}"
ENV DODOO_LOAD_OUT="/var/lib/dodoo-loader/logs.json"
#   --- WDB Configuration ---
ENV WDB_NO_BROWSER_AUTO_OPEN="True" \
    WDB_SOCKET_SERVER="wdb" \
    WDB_WEB_PORT="1984" \
    WDB_WEB_SERVER="localhost"
#   Get dodoo-* devops toolchain
RUN pip install dodoo-tester==0.6.7 dodoo-initializer==0.6.1 dodoo-loader==0.6.5 dodoo-migrator==0.6.7
#   Custom Odoo Scaffolding dependency
RUN pip install pick==2.2.0 --quiet --quiet
#   General develpoment libraries
RUN pip install astor==0.8.1 pylint-odoo==8.0.19 pylint-mccabe==0.1.3 coverage==7.2.3 ptpython==3.0.23 pudb==2022.1.3 pyinotify==0.9.6 watchdog==3.0.0 wdb==3.3.0 --quiet --quiet
#   For querying json log output
RUN apt-get update -qq \
 && apt-get install --no-install-recommends jq -qq -y > /dev/null
#   local browser testing
RUN wget --quiet -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' | tee /etc/apt/sources.list.d/google-chrome.list \
 && apt-get update -qq \
 && apt-get install --no-install-recommends google-chrome-stable -qq -y > /dev/null
#   For js test recordings
#   Not avaialble in jessie
RUN apt-get update -qq \
 && apt-get install --no-install-recommends ffmpeg -qq -y > /dev/null || true
#   For chrome bworser testing
RUN pip install websocket-client==1.5.1 --quiet --quiet
COPY entrypoint.d/* /entrypoint.d/
COPY templates /templates
ENV ODOO_TEMPLATES_DIR="/templates"
RUN /bin/bash -c 'shopt -s dotglob \
 && chmod +x /entrypoint.d/* \
 && shopt -u dotglob'
#   Prepare odoo owned testlog folder for chrome screenshots
RUN mkdir -p /var/lib/odoo-testlogs \
 && chown odoo:odoo /var/lib/odoo-testlogs
#   Prepare odoo owned loading log folder for dodoo loader
RUN mkdir -p /var/lib/dodoo-loader \
 && chown odoo:odoo /var/lib/dodoo-loader
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
