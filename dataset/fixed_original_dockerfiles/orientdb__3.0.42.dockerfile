# ###########################################################
#  Dockerfile to run an OrientDB (Graph) Container
# ###########################################################
FROM openjdk:8-jdk-slim
MAINTAINER OrientDB LTD (info@orientdb.com)
#  Override the orientdb download location with e.g.:
#    docker build -t mine --build-arg ORIENTDB_DOWNLOAD_SERVER=https://repo1.maven.org/maven2/com/orientechnologies/ .
ARG ORIENTDB_DOWNLOAD_SERVER
ENV ORIENTDB_VERSION="3.0.42"
ENV ORIENTDB_DOWNLOAD_MD5="59ed522290668fb400e67503652bb813"
ENV ORIENTDB_DOWNLOAD_SHA1="fe6a510c72983b32a3ffd657be9ae62fab2b61f8"
ENV ORIENTDB_DOWNLOAD_URL="${ORIENTDB_DOWNLOAD_SERVER:-https://repo1.maven.org/maven2/com/orientechnologies}/orientdb-community/$ORIENTDB_VERSION/orientdb-community-$ORIENTDB_VERSION.tar.gz"
RUN apt update \
 && apt install -y curl wget \
 && rm -rf /var/lib/apt/lists/*
# download distribution tar, untar and delete databases
RUN mkdir /orientdb \
 && wget $ORIENTDB_DOWNLOAD_URL \
 && echo "$ORIENTDB_DOWNLOAD_MD5 *orientdb-community-$ORIENTDB_VERSION.tar.gz" | md5sum -c - \
 && echo "$ORIENTDB_DOWNLOAD_SHA1 *orientdb-community-$ORIENTDB_VERSION.tar.gz" | sha1sum -c - \
 && tar -xvzf orientdb-community-$ORIENTDB_VERSION.tar.gz -C /orientdb --strip-components=1 \
 && rm orientdb-community-$ORIENTDB_VERSION.tar.gz \
 && rm -rf /orientdb/databases/*
ENV PATH="/orientdb/bin:$PATH"
VOLUME ["/orientdb/backup", "/orientdb/databases", "/orientdb/config"]
WORKDIR /orientdb
# OrientDb binary
EXPOSE 2424/tcp
# OrientDb http
EXPOSE 2480/tcp
#  Default command start the server
CMD ["server.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
