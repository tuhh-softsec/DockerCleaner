#  ------------------------------------------------------------------------------
#  Dockerfile to build basic Oracle database images
#  Based on the following:
#    - Oracle Linux 7 - Slim
#    - Oracle Database
#        http://www.oracle.com/technetwork/database/enterprise-edition/downloads/index.html
#    - Oracle Application Express (APEX)
#        http://www.oracle.com/technetwork/developer-tools/apex/downloads/index.html
#
#  Example build and run.
#
#  docker build -t ol7_183:latest .
#  docker build --squash -t ol7_183:latest .
#
#  Non-persistent storage.
#  docker run -dit --name ol7_183_con -p 1521:1521 -p 5500:5500 --shm-size="1G" ol7_183:latest
#
#  Persistent storage.
#  docker run -dit --name ol7_183_con -p 1521:1521 -p 5500:5500 --shm-size="1G" -v /u01/volumes/ol7_183_con_u02/:/u02 ol7_183:latest
#
#  Persistent storage and part of Docker network called "my_network".
#  docker run -dit --name ol7_183_con -p 1521:1521 -p 5500:5500 --shm-size="1G" --network=my_network -v /u01/volumes/ol7_183_con_u02/:/u02 ol7_183:latest
#
#  docker logs --follow ol7_183_con
#  docker exec -it ol7_183_con bash
#
#  docker stop --time=30 ol7_183_con
#  docker start ol7_183_con
#  
#  docker rm -vf ol7_183_con 
#
#  ------------------------------------------------------------------------------
#  Set the base image to Oracle Linux 7 - Slim
FROM oraclelinux:7-slim
#  File Author / Maintainer
#  Use LABEL rather than deprecated MAINTAINER
#  MAINTAINER Tim Hall (tim@oracle-base.com)
LABEL maintainer="tim@oracle-base.com"
#  ------------------------------------------------------------------------------
#  Define fixed (build time) environment variables.
ENV ORACLE_BASE="/u01/app/oracle" \
    ORACLE_HOME="/u01/app/oracle/product/18.0.0/dbhome_1" \
    ORA_INVENTORY="/u01/app/oraInventory" \
    SOFTWARE_DIR="/u01/software" \
    DB_SOFTWARE="LINUX.X64_180000_db_home.zip" \
    APEX_SOFTWARE="apex_19.1_en.zip" \
    ORACLE_PASSWORD="oracle" \
    SCRIPTS_DIR="/u01/scripts" \
    ORAENV_ASK="NO"
#  Separate ENV call to allow existing variables to be referenced.
ENV PATH="${ORACLE_HOME}/bin:${PATH}"
#  ------------------------------------------------------------------------------
#  Define config (runtime) environment variables.
ENV ORACLE_SID="cdb1" \
    SYS_PASSWORD="SysPassword1" \
    PDB_NAME="pdb1" \
    PDB_PASSWORD="PdbPassword1" \
    INSTALL_APEX="true" \
    APEX_EMAIL="me@example.com" \
    APEX_PASSWORD="ApexPassword1"
#  ------------------------------------------------------------------------------
#  Get all the files for the build.
COPY software/* ${SOFTWARE_DIR}/
COPY scripts/* ${SCRIPTS_DIR}/
#  ------------------------------------------------------------------------------
#  Unpack all the software and remove the media.
#  No config done in the build phase.
#  
#  Manually create user and group as preinstall package creates the with
#  high IDs, which causes issues. Note 2 on link below.
#  https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#user
#
RUN groupadd -g 500 dba \
 && groupadd -g 501 oinstall \
 && groupadd -g 1042 docker_fg \
 && useradd -d /home/oracle -u 500 -g dba -G oinstall,dba,docker_fg -m -s /bin/bash oracle \
 && yum -y install unzip tar gzip \
 && yum -y update \
 && yum -y install oracle-database-preinstall-18c \
 && rm -Rf /var/cache/yum \
 && mkdir -p ${ORACLE_HOME} \
 && mkdir -p /u02/oradata \
 && chown -R oracle.oinstall /u01 \
 && chmod u+x ${SCRIPTS_DIR}/*.sh \
 && chown oracle:docker_fg /u02 \
 && chmod 775 /u02 \
 && chmod g+s /u02
#  Perform the following actions as the oracle user
USER oracle
#  Unzip software
RUN cd ${ORACLE_HOME} \
 && unzip -oq ${SOFTWARE_DIR}/${DB_SOFTWARE} \
 && rm -f ${SOFTWARE_DIR}/${DB_SOFTWARE}
#  Do software-only installation
RUN ${ORACLE_HOME}/runInstaller -ignorePrereq -waitforcompletion -silent -responseFile ${ORACLE_HOME}/install/response/db_install.rsp oracle.install.option=INSTALL_DB_SWONLY ORACLE_HOSTNAME=${HOSTNAME} UNIX_GROUP_NAME=oinstall INVENTORY_LOCATION=${ORA_INVENTORY} SELECTED_LANGUAGES=en,en_GB ORACLE_HOME=${ORACLE_HOME} ORACLE_BASE=${ORACLE_BASE} oracle.install.db.InstallEdition=EE oracle.install.db.OSDBA_GROUP=dba oracle.install.db.OSBACKUPDBA_GROUP=dba oracle.install.db.OSDGDBA_GROUP=dba oracle.install.db.OSKMDBA_GROUP=dba oracle.install.db.OSRACDBA_GROUP=dba SECURITY_UPDATES_VIA_MYORACLESUPPORT=false DECLINE_SECURITY_UPDATES=true ; exit 0
#  Replace default APEX software with user-provided version.
RUN rm -Rf ${ORACLE_HOME}/apex \
 && unzip -oqd ${ORACLE_HOME} ${SOFTWARE_DIR}/${APEX_SOFTWARE} \
 && rm ${SOFTWARE_DIR}/${APEX_SOFTWARE}
#  Run the root scripts
USER root
RUN sh ${ORA_INVENTORY}/orainstRoot.sh \
 && sh ${ORACLE_HOME}/root.sh
#  Perform the following actions as the oracle user
USER oracle
VOLUME ["/u02"]
EXPOSE 1521/tcp 5500/tcp
HEALTHCHECK --interval=300s --start-period=600s CMD ${SCRIPTS_DIR}/healthcheck.sh > /dev/null || exit 1
#  ------------------------------------------------------------------------------
#  The start script performs all config based on runtime environment variables.
#  First run creates the database.
CMD exec ${SCRIPTS_DIR}/start.sh
#  End
