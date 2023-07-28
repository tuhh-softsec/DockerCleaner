FROM ubuntu:14.04
ARG TERM=linux
ARG DEBIAN_FRONTEND=noninteractive
ARG SCIDB_NAME=scidb
ARG SCIDB_LOG_LEVEL=WARN
ARG SCIDB_SCRIPT_URL="https://downloads.paradigm4.com/community/18.1/install-scidb-ce.sh"
ARG SHIM_SHA1=ceec9f5f92d869d052f2654aa113238b59e11a42
ENV SCIDB_VER="18.1" \
    SCIDB_NAME="scidb"
ENV SCIDB_INSTALL_PATH="/opt/scidb/$SCIDB_VER"
ENV PATH="$PATH:$SCIDB_INSTALL_PATH/bin"
#  # Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.0.1ubuntu2.24 ca-certificates=20170717~14.04.2 liblog4cxx10-dev=0.10.0-1.2ubuntu3 libprotobuf-dev=2.5.0-9ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 openssh-client=1:6.6p1-2ubuntu2.13 openssh-server=1:6.6p1-2ubuntu2.13 patch=2.7.1-4ubuntu2.4 software-properties-common=0.92.37.8 wget=1.15-1ubuntu1.14.04.5 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 --assume-yes \
 && rm -rf /var/lib/apt/lists/*
#  # Get SciDB install script
RUN wget --no-verbose --output-document /install-scidb-ce.sh $SCIDB_SCRIPT_URL
RUN chmod +x /install-scidb-ce.sh
#  # Run SciDB install script
RUN apt-get update \
 && yes | /install-scidb-ce.sh \
 && apt-get install --no-install-recommends make=3.81-8.2ubuntu3 scidb-$SCIDB_VER-dev scidb-$SCIDB_VER-libboost1.54-dev scidb-$SCIDB_VER-libboost-system1.54-dev --assume-yes \
 && rm -rf /var/lib/apt/lists/*
#  # Setup SSH
RUN echo 'StrictHostKeyChecking no' >> /etc/ssh/ssh_config \
 && ssh-keygen -f /root/.ssh/id_rsa -q -N "" \
 && cp /root/.ssh/id_rsa.pub /root/.ssh/authorized_keys
#  # Setup SciDB config file
COPY config.ini $SCIDB_INSTALL_PATH/etc/
#  # Setup PostgreSQL and SciDB
RUN echo "127.0.0.1:5432:$SCIDB_NAME:$SCIDB_NAME:`date +%s | sha256sum | base64 | head -c 32 `" > /root/.pgpass \
 && chmod go-r /root/.pgpass \
 && chmod a+r $SCIDB_INSTALL_PATH/etc/config.ini \
 && service ssh start \
 && service postgresql start \
 && until pg_isready ; do sleep 1 ; done \
 && su --command=" $SCIDB_INSTALL_PATH/bin/scidb.py init-syscat $SCIDB_NAME --db-password `cut --delimiter : --fields 5 /root/.pgpass `" postgres \
 && $SCIDB_INSTALL_PATH/bin/scidb.py init-all --force $SCIDB_NAME \
 && service postgresql stop \
 && sed --in-place s/log4j.rootLogger=INFO/log4j.rootLogger=$SCIDB_LOG_LEVEL/ $SCIDB_INSTALL_PATH/share/scidb/log4cxx.properties
RUN wget --no-verbose --output-document - https://github.com/Paradigm4/shim/archive/$SHIM_SHA1.tar.gz | tar --extract --gzip --directory=/usr/local/src \
 && cd /usr/local/src/shim-$SHIM_SHA1 \
 && make service \
 && openssl req -new -newkey rsa:4096 -days 3650 -nodes -x509 -subj "/C=US/ST=MA/L=Waltham/O=Paradigm4/CN=$( hostname ;)" -keyout /var/lib/shim/ssl_cert.pem 2> /dev/null >> /var/lib/shim/ssl_cert.pem
#  # Setup container entrypoint
COPY docker-entrypoint.sh /
ENTRYPOINT ["/docker-entrypoint.sh"]
#  # Port | App
#  # -----+-----
#  # 1239 | SciDB iquery
#  # 8080 | SciDB Shim (HTTP)
#  # 8083 | SciDB Shim (HTTPS)
EXPOSE 1239/tcp 8080/tcp 8083/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
