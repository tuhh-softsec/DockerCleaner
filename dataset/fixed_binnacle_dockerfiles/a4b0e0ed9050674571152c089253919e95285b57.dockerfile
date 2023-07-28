#   Dockerizing MongoDB: Dockerfile for building MongoDB images
#   Based on ubuntu:latest, installs MongoDB following the instructions from:
#   http://docs.mongodb.org/manual/tutorial/install-mongodb-on-ubuntu/
#   Format: FROM    repository[:version]
FROM ubuntu:latest
#   Format: MAINTAINER Name <email@addr.ess>
MAINTAINER Jonas Colmsjö <jonas@gizur.com>
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 wget=1.21.3-1ubuntu1 nano=7.2-1 -y )
RUN echo "export HOME=/root" > /.profile
#
#   Install supervisord (used to handle processes)
#
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=4.2.1-1ubuntu1 -y )
COPY ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#
#   Install MongoDB
#   ---------------
#   Installation:
#   Import MongoDB public GPG key AND create a MongoDB list file
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 7F0CEB10
RUN echo 'deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen' | tee /etc/apt/sources.list.d/10gen.list
#   Update apt-get sources AND install MongoDB
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends mongodb-org -y -q )
#   It is possible to install a specific version also
#   RUN apt-get install -y -q mongodb-org=2.6.1 mongodb-org-server=2.6.1 mongodb-org-shell=2.6.1 mongodb-org-mongos=2.6.1 mongodb-org-tools=2.6.1
#   Create the MongoDB data directory
RUN mkdir -p /data/db
COPY ./etc-mongod.conf /etc/mongod.conf
#
#   Install NodeJS
#   --------------
COPY ./init-node.sh /src/
#   Download and install using npm
RUN curl https://raw.githubusercontent.com/creationix/nvm/v0.11.1/install.sh | bash
RUN echo "[ -s /.nvm/nvm.sh ] \
 && . /.nvm/nvm.sh" >> /.profile
RUN /bin/bash -c "source /.profile \
 && nvm install v0.11.2"
#   Install node for root user together node jaydata etc.
RUN /src/init-node.sh
#   Fix a broken package.json (connect need to be exactly 2.0.0)
COPY ./odata-server-package.json /.nvm/v0.11.2/lib/node_modules/odata-server/package.json
RUN /bin/bash -c "source $HOME/.nvm/nvm.sh; nvm use v0.11.2; cd /.nvm/v0.11.2/lib/node_modules/odata-server; npm install ."
#
#   Install MySQL
#   -------------
#   Keep upstart from complaining
RUN dpkg-divert --local --rename --add /sbin/initctl
#  RUN ln -s /bin/true /sbin/initctl
RUN :
#   Bundle everything
COPY ./mysql-setup.sh /src/
#   Install MySQL server
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y mysql-server \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Fix configuration
RUN sed -i -e"s/^bind-address\s*=\s*127.0.0.1/bind-address = 0.0.0.0/" /etc/mysql/my.cnf
#   Setup admin user
RUN /src/mysql-setup.sh
#
#   Tungsten replicator
#   --------------------
RUN :
#   Install Java
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-7-jre -y )
#   Install Ruby
RUN (apt-get update ;apt-get install --no-install-recommends ruby1.9.1-full -y )
#   Install rsync
RUN (apt-get update ;apt-get install --no-install-recommends rsync=3.2.7-1 -y )
#   Install sshd
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:9.0p1-1ubuntu8 -y )
RUN mkdir /var/run/sshd
#  RUN echo 'root:password' |chpasswd
#
#   NOTE: Insecure, should not be used in production!!
#
COPY ./root-ssh-authorized_keys /root/.ssh/authorized_keys
COPY ./root-ssh-id_rsa /root/.ssh/id_rsa
COPY ./root-ssh-id_rsa.pub /root/.ssh/id_rsa.pub
COPY ./etc-ssh-sshd_config /etc/ssh/sshd_config
RUN chmod 700 /root/.ssh ; chmod 600 /root/.ssh/**
#   Install tungsten
RUN wget https://s3.amazonaws.com/files.continuent.com/builds/nightly/tungsten-2.0-snapshots/tungsten-replicator-2.1.0-346.tar.gz
RUN tar -xzf tungsten-replicator-2.1.0-346.tar.gz
RUN mkdir opt/continuent
COPY ./etc-mysql-my.cnf /etc/mysql/my.cnf
RUN ln -s /etc/mysql/my.cnf /etc/my.cnf
COPY ./init-mongodb.js /src/
COPY ./init-mongodb.sh /src/
RUN /src/init-mongodb.sh
#
#   Start things up
#   ---------------
#   2112, 10000 and 10001 are used by tungsten
#   27017 is used by mongodb
#   3306 is used by MySQL
#   9001 is used by supervisord
EXPOSE 22/tcp 80/tcp 443/tcp 8080/tcp 27017/tcp 3306/tcp 2112/tcp 10000/tcp 10001/tcp 9001/tcp
CMD ["supervisord", "-n"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
