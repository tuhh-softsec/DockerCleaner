FROM arm32v6/alpine:edge
MAINTAINER Julian Xhokaxhiu <info at julianxhokaxhiu dot com>
#   Environment variables
#  ######################
ENV DATA_DIR="/srv/data"
ENV QEMU_EXECVE="1"
#   Configurable environment variables
#  ###################################
#   Custom DNS where to forward your request, if not found inside the DNS Server.
#   By default this will be forwarded to Google DNS for IPv4 and IPv6 requests.
#   See https://doc.powerdns.com/md/recursor/settings/#forward-zones
ENV CUSTOM_DNS="\"8.8.8.8;8.8.4.4;[2001:4860:4860::8888];[2001:4860:4860::8844]\""
#   Custom API Key for PowerDNS.
#   Leave empty to autogenerate one ( HIGHLY SUGGESTED! )
#   See https://doc.powerdns.com/md/authoritative/settings/#api-key
ENV API_KEY="\"
#   Change this cron rule to what fits best for you.
#   Used only if ENABLE_ADBLOCK=true
#   By Default = At 10:00 UTC ~ 2am PST/PDT
ENV CRONTAB_TIME="'0 10 * * *'"
#   Enable the AdBlock feature
ENV ENABLE_ADBLOCK="false"
#   Create Volume entry points
#  ###########################
VOLUME $DATA_DIR
#   Copy required files and fix permissions
#  ########################################
COPY src/* /root/
#   For cross compile on dockerhub
#  ###############################
COPY armv6/qemu-arm-static /usr/bin/
COPY armv6/resin-xbuild /usr/bin/
RUN ["/usr/bin/qemu-arm-static", "/bin/sh", "-c", "ln", "-s", "resin-xbuild", "/usr/bin/cross-build-start", ";", "ln", "-s", "resin-xbuild", "/usr/bin/cross-build-end", ";", "ln", "/bin/sh", "/bin/sh.real"]
RUN ["cross-build-start"]
#   Create missing directories
#  ###########################
RUN mkdir -p $DATA_DIR
#   Set the work directory
#  #######################
WORKDIR /root
#   Fix permissions
#  ################
RUN chmod 0644 * \
 && chmod 0755 *.sh
#   Install required packages
#  #############################
RUN apk add bash=5.2.15-r2 supervisor=4.2.5-r1 pdns=4.7.3-r1 pdns-doc=4.7.3-r1 pdns-recursor=4.8.4-r0 pdns-backend-sqlite3=4.7.3-r1 sqlite=3.41.2-r1 curl=8.0.1-r1 dbus=1.14.6-r1 libldap=2.6.4-r1 python3=3.11.3-r0 --update --no-cache
#   Required by PowerDNS Admin GUI
RUN apk add git=2.40.0-r0 gcc=12.2.1_git20220924-r9 musl-dev=1.2.3_git20230322-r0 python3-dev=3.11.3-r0 mariadb-dev=10.11.2-r4 libffi-dev=3.4.4-r1 libxslt-dev=1.1.37-r2 xmlsec-dev=1.2.37-r0 openldap-dev=2.6.4-r1 nodejs=18.15.0-r0 npm=9.6.4-r0 --update --no-cache --virtual .build-deps
#   Install PowerDNS Admin GUI
#  #############################
RUN mkdir -p /usr/share/webapps/ \
 && cd /usr/share/webapps/ \
 && git config --system http.sslverify false \
 && git clone https://github.com/ngoduykhanh/PowerDNS-Admin.git powerdns-admin \
 && cd /usr/share/webapps/powerdns-admin \
 && pip3 install --no-cache-dir -r requirements.txt \
 && mv package.json app/static \
 && cd app/static \
 && npm install
#   Cleanup
#  ########
RUN find /usr/local
#   Replace default configurations
#  ###############################
RUN rm /etc/pdns/pdns.conf \
 && rm /etc/pdns/recursor.conf \
 && rm /etc/supervisord.conf \
 && mv /root/pdns.conf /etc/pdns \
 && mv /root/recursor.conf /etc/pdns \
 && mv /root/config.py /usr/share/webapps/powerdns-admin \
 && mv /root/supervisord.conf /etc
#   Allow redirection of stdout to docker logs
#  ###########################################
RUN ln -sf /proc/1/fd/1 /var/log/docker.log
#   For cross compile on dockerhub
#  ###############################
RUN ["cross-build-end"]
#   Expose required ports
#  ######################
EXPOSE 53/tcp
EXPOSE 53/udp
EXPOSE 8080/tcp
#   Change Shell
#  #############
SHELL ["/bin/bash", "-c"]
#   Set the entry point to init.sh
#  ##########################################
ENTRYPOINT /root/init.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
