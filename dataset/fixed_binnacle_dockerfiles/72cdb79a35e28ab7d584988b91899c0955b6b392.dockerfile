FROM ubuntu:16.04
MAINTAINER Nicola <niquola@gmail.com>,  BazZy <bazzy.bazzy@gmail.com>,  Danil Kutkevich <danil@kutkevich.org>
ENV REFRESHED_AT="20160425T0900Z"
RUN localedef --force --inputfile=en_US --charmap=UTF-8 --alias-file=/usr/share/locale/locale.alias en_US.UTF-8
ENV LANG="en_US.UTF-8"
RUN :
RUN apt-get --yes upgrade
#   Add PostgreSQL (9.1, 9.2, 9.3, 9.4, 9.5) apt repository
#   <http://www.postgresql.org/download/linux/ubuntu/>.
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 sudo=1.8.16-0ubuntu1.10 --yes )
RUN echo 'deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main' > /etc/apt/sources.list.d/pgdg.list
RUN curl --silent https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
RUN :
RUN apt-get --yes upgrade
#   Install PostgreSQL.
ENV PG_MAJOR="9.4"
RUN (apt-get update ;apt-get install --no-install-recommends postgresql-$PG_MAJOR --yes )
#   Install plv8 (in case of plv8 compilation issues address to
#   README in <https://github.com/clkao/docker-postgres-plv8>).
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 build-essential=12.1ubuntu2 libv8-dev=3.14.5.8-5ubuntu2 postgresql-server-dev-$PG_MAJOR --yes )
RUN (apt-get update ;apt-get install --no-install-recommends nodejs-dev=4.2.6~dfsg-1ubuntu4.2 --yes )
#   Adjust PostgreSQL configuration so that remote connections to the
#   database are possible.
RUN echo 'host all all 0.0.0.0/0 md5' >> /etc/postgresql/$PG_MAJOR/main/pg_hba.conf
RUN echo 'local all all trust' >> /etc/postgresql/$PG_MAJOR/main/pg_hba.conf
RUN echo "listen_addresses='*'" >> /etc/postgresql/$PG_MAJOR/main/postgresql.conf
USER postgres
#   Fix PostgreSQL locale
#   <http://stackoverflow.com/questions/16736891/pgerror-error-new-encoding-utf8-is-incompatible#16737776>,
#   <http://www.postgresql.org/message-id/43FE1E65.3030000@genome.chop.edu>,
#   <http://www.postgresql.org/docs/current/static/multibyte.html#AEN35730>.
RUN service postgresql start \
 && psql --command="UPDATE pg_database SET datistemplate = FALSE WHERE datname = 'template1';" \
 && psql --command="DROP DATABASE template1;" \
 && psql --command="CREATE DATABASE template1 WITH TEMPLATE = template0 ENCODING = 'UNICODE' LC_COLLATE='en_US.UTF-8' LC_CTYPE='en_US.UTF-8';" \
 && psql --command="UPDATE pg_database SET datistemplate = TRUE WHERE datname = 'template1';" \
 && psql --command="CREATE ROLE fhirbase WITH SUPERUSER LOGIN PASSWORD 'fhirbase';" \
 && psql --command="CREATE DATABASE fhirbase WITH OWNER fhirbase ENCODING = 'UTF8';"
USER root
RUN useradd --user-group --create-home --shell /bin/bash fhirbase \
 && echo 'fhirbase:fhirbase' | chpasswd \
 && adduser fhirbase sudo
RUN echo 'fhirbase ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers
USER fhirbase
RUN cd ~ \
 && git clone https://github.com/fhirbase/fhirbase-plv8.git fhirbase \
 && cd ~/fhirbase \
 && git submodule update --init --recursive
#   Install nodejs.
RUN curl --silent -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.1/install.sh | bash
RUN bash -lc 'source ~/.nvm/nvm.sh \
 && nvm install 6.2.0'
RUN bash -lc 'cd ~/fhirbase \
 && source ~/.nvm/nvm.sh \
 && nvm use 6.2.0 \
 && npm install'
RUN bash -lc 'cd ~/fhirbase/plpl \
 && source ~/.nvm/nvm.sh \
 && nvm use 6.2.0 \
 && npm install'
USER root
ENV PLV8_VERSION="v1.4.7"
RUN cd /tmp \
 && git clone https://github.com/plv8/plv8.git \
 && cd /tmp/plv8 \
 && git checkout $PLV8_VERSION \
 && make all install
USER fhirbase
#   Install fhirbase.
RUN bash -lc 'sudo service postgresql start \
 && cd ~/fhirbase \
 && source ~/.nvm/nvm.sh \
 && nvm use 6.2.0 \
 && export PATH="$HOME"/fhirbase/node_modules/coffee-script/bin:"$PATH" \
 && export DATABASE_URL=postgres://fhirbase:fhirbase@localhost:5432/fhirbase \
 && ~/fhirbase/build-commit.sh --rebuild \
 && cat ~/fhirbase/build/latest/build.sql | psql fhirbase'
USER root
COPY docker-run-tests.sh /
RUN chmod a+rwx /docker-run-tests.sh
RUN chown fhirbase /docker-run-tests.sh
#   Install utils.
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:7.2p2-4ubuntu2.10 aptitude=0.7.4-2ubuntu2 --yes )
USER fhirbase
#   Expose the PostgreSQL port.
EXPOSE 5432/tcp
#   Add VOLUMEs to allow backup of config, logs, socket and databases
VOLUME ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql", "/var/run/postgresql"]
WORKDIR /home/fhirbase
CMD /bin/bash
# Please add your HEALTHCHECK here!!!
