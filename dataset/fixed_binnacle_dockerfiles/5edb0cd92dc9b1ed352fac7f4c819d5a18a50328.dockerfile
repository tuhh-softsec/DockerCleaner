#  ##############################################################################
#   Dockerfile to build Percona Server 5.x container images
#   Based on Ubuntu 18.04
#  ##############################################################################
#   Set the base image to Ubuntu 18.04
FROM ubuntu:18.04
#   File Author / Maintainer
MAINTAINER Swapbyt3s
LABEL vendor="Swapbyt3s" \
      description="Percona Server 5.x on Ubuntu 18.04" \
      version="5.x"
#   Update the repository sources list
RUN : \
 && apt-get -y upgrade
ENV PERCONA_VERSION="5.5"
#  ##############################################################################
#   BEGIN INSTALLATION
#  ##############################################################################
#   -----------------------------------------------------------------------------
#   Install additional packages
#   -----------------------------------------------------------------------------
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:8.0.1453-1ubuntu1.11 wget=1.19.4-1ubuntu2.2 htop=2.1.0-3 stress=1.0.4-2 curl=7.58.0-2ubuntu3.24 lsb=9.20170808ubuntu1 -y )
#   -----------------------------------------------------------------------------
#   Install Zenit
#   -----------------------------------------------------------------------------
COPY zenit.yaml /etc/zenit/zenit.yaml
RUN mkdir -p /etc/zenit/
RUN sed -i 's/localhost/${HOSTNAME}/' /etc/zenit/zenit.yaml
RUN sed -i 's/debug: false/debug: true/' /etc/zenit/zenit.yaml
RUN sed -i 's/root@tcp/admin:admin@tcp/' /etc/zenit/zenit.yaml
RUN sed -i 's/127.0.0.1:8123/172.20.1.2:8123/' /etc/zenit/zenit.yaml
RUN sed -i 's/radminuser:radminpass@tcp\(127.0.0.1:6032\)/proxysql:admin@tcp\(172.20.1.4:6032\)/' /etc/zenit/zenit.yaml
RUN sed -i 's/interval: 10/interval: 5/' /etc/zenit/zenit.yaml
RUN sed -i 's/duration: 30/duration: 10/' /etc/zenit/zenit.yaml
RUN sed -i 's/token: xxx\/yyy\/zzz/token: ${ZENIT_SLACK_TOKEN}/' /etc/zenit/zenit.yaml
#   -----------------------------------------------------------------------------
#   Test scripts
#   -----------------------------------------------------------------------------
COPY assets/tests/slow.log /root/slow.log
COPY assets/tests/audit.log /root/audit.log
COPY docker/percona-server/loop_log_audit.sh /root/loop_log_audit.sh
COPY docker/percona-server/loop_log_slow.sh /root/loop_log_slow.sh
#   -----------------------------------------------------------------------------
#   Install MySQL packages
#   -----------------------------------------------------------------------------
RUN (apt-get update ;apt-get install --no-install-recommends apt-transport-https=1.6.14 dirmngr=2.2.4-1ubuntu1.6 apt-utils=1.6.14 apt-transport-https=1.6.14 ca-certificates=20211016ubuntu0.18.04.1 libpwquality-tools=1.4.0-2 cracklib-runtime=2.9.2-5build1 gnupg=2.2.4-1ubuntu1.6 libdbi-perl=1.640-1ubuntu0.3 libdbd-mysql-perl=4.046-1 libterm-readkey-perl=2.37-1build1 libio-socket-ssl-perl=2.060-3~ubuntu18.04.1 -y )
RUN wget https://repo.percona.com/apt/percona-release_latest.$( lsb_release -sc ;)_all.deb > /dev/null 2>&1 \
 && dpkg -i percona-release_latest.$( lsb_release -sc ;)_all.deb \
 && :
RUN wget https://www.percona.com/downloads/percona-toolkit/3.0.3/binary/debian/zesty/x86_64/percona-toolkit_3.0.3-1.zesty_amd64.deb > /dev/null 2>&1 \
 && dpkg -i percona-toolkit_3.0.3-1.zesty_amd64.deb
RUN env DEBIAN_FRONTEND=noninteractive apt-get install -y percona-server-server-$PERCONA_VERSION \
 && chown -R mysql:0 /var/lib/mysql /var/run/mysqld \
 && chmod 777 /var/run/mysqld
COPY docker/percona-server/my.cnf /etc/mysql/my.cnf
#   -----------------------------------------------------------------------------
#   Copy script utility
#   -----------------------------------------------------------------------------
COPY docker/percona-server/entrypoint.sh /root/entrypoint.sh
COPY docker/percona-server/configure.sh /root/configure.sh
RUN chmod a+x /root/entrypoint.sh
RUN chmod a+x /root/configure.sh
#   -----------------------------------------------------------------------------
#   Clear
#   -----------------------------------------------------------------------------
RUN rm -rf /var/lib/apt/lists/* /var/cache/debconf \
 && apt-get clean
#   -----------------------------------------------------------------------------
#   Clear
#   -----------------------------------------------------------------------------
RUN rm -rf /var/lib/apt/lists/* /var/cache/debconf \
 && apt-get clean
#  ############################# INSTALLATION END ###############################
#   Expose the default MySQL port
EXPOSE 3306/tcp
#   Set the working directory to /root
WORKDIR /root
#   Start service on run container
ENTRYPOINT ["/root/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
