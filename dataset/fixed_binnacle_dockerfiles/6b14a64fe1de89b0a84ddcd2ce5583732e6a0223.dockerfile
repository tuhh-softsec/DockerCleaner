#   This Dockerfile is based on: https://docs.docker.com/examples/postgresql_service/
FROM ubuntu:16.04
MAINTAINER fredrik@averpil.com
#   Add the PostgreSQL PGP key to verify their Debian packages.
#   It should be the same key as https://www.postgresql.org/media/keys/ACCC4CF8.asc
RUN apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8
#   Add PostgreSQL's repository. It contains the most recent stable release
#       of PostgreSQL, ``9.3``.
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ precise-pgdg main" > /etc/apt/sources.list.d/pgdg.list
#   Install everything in one enormous RUN command
#    There are some warnings (in red) that show up during the build. You can hide
#    them by prefixing each apt-get statement with DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install --no-install-recommends python-software-properties=0.96.20.10 python-pip=8.1.1-2ubuntu0.6 software-properties-common=0.96.20.10 postgresql-9.3 postgresql-client-9.3 postgresql-contrib-9.3 postgresql-server-dev-9.3 rubygems -y \
 && gem install taskjuggler --version 3.7.2 \
 && pip install pip==23.1 -U \
 && pip install sqlalchemy==2.0.9 psycopg2==2.9.6 jinja2==3.1.2 alembic==1.10.3 mako==1.2.4 markupsafe==2.1.2 python-editor==1.0.4 nose==1.3.7 coverage==7.2.3
#   Note: The official Debian and Ubuntu images automatically ``apt-get clean``
#   after each ``apt-get``
#   Run commands as the ``postgres`` user created by the ``postgres-9.3`` package when it was ``apt-get installed``
USER postgres
RUN /etc/init.d/postgresql start \
 && psql -c "CREATE DATABASE stalker_test;" -U postgres \
 && psql -c "CREATE USER stalker_admin WITH PASSWORD 'stalker';" -U postgres \
 && /etc/init.d/postgresql stop
#   Adjust PostgreSQL configuration so that remote connections to the
#   database are possible.
#   RUN echo "host all  all    0.0.0.0/0  md5" >> /etc/postgresql/9.3/main/pg_hba.conf
#   And add ``listen_addresses`` to ``/etc/postgresql/9.3/main/postgresql.conf``
#   RUN echo "listen_addresses='*'" >> /etc/postgresql/9.3/main/postgresql.conf
#   Expose the PostgreSQL port
#   EXPOSE 5432
#   Add VOLUMEs to allow backup of config, logs and databases
#   VOLUME  ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]
USER root
#   Create symlink to TaskJuggler
#   RUN ln -s $(which tj3) /usr/local/bin/tj3
#   Set working directory
WORKDIR /workspace
#   Embed wait-for-postgres.sh script into Dockerfile
RUN echo '\n\nset -e\n\ncmd="$@"\ntimer="5"\n\nuntil runuser -l postgres -c 'pg_isready' 2>/dev/null; do\n >&2 echo "Postgres is unavailable - sleeping for $timer seconds"\n sleep $timer\ndone\n\n>&2 echo "Postgres is up - executing command"\nexec $cmd\n' >> /workspace/wait-for-postgres.sh
#   Make script executable
RUN chmod +x /workspace/wait-for-postgres.sh
#   Execute this when running container
ENTRYPOINT cp -r /stalker /workspace \
 && chmod -R -x /workspace/stalker \
 && runuser -l postgres -c '/usr/lib/postgresql/9.3/bin/postgres -D /var/lib/postgresql/9.3/main -c config_file=/etc/postgresql/9.3/main/postgresql.conf & ' \
 && ./wait-for-postgres.sh nosetests /workspace/stalker --verbosity=1 --cover-erase --with-coverage --cover-package=stalker \
 && /etc/init.d/postgresql stop
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
