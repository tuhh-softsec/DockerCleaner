# ###########################################################
#  Dockerfile to run an OrientDB (Graph) Container
# ###########################################################
FROM openjdk:8-jdk-slim
LABEL maintainer="OrientDB LTD (info@orientdb.com)"
#  Override the orientdb download location with e.g.:
#    docker build -t mine --build-arg ORIENTDB_DOWNLOAD_SERVER=https://repo1.maven.org/maven2/com/orientechnologies/ .
ARG ORIENTDB_DOWNLOAD_SERVER
ENV ORIENTDB_VERSION="3.0.42"
ENV ORIENTDB_DOWNLOAD_MD5="59ed522290668fb400e67503652bb813"
ENV ORIENTDB_DOWNLOAD_SHA1="fe6a510c72983b32a3ffd657be9ae62fab2b61f8"
ENV ORIENTDB_DOWNLOAD_URL="${ORIENTDB_DOWNLOAD_SERVER:-https://repo1.maven.org/maven2/com/orientechnologies}/orientdb-community/$ORIENTDB_VERSION/orientdb-community-$ORIENTDB_VERSION.tar.gz"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update \
 && apt-get install curl=7.74.0-1.3+deb11u7 wget=1.21-1+deb11u1 -y \
 && rm -rf /var/lib/apt/lists/*
# download distribution tar, untar and delete databases
RUN mkdir /orientdb \
 && wget -nv $ORIENTDB_DOWNLOAD_URL \
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
HEALTHCHECK CMD curl --fail http://127.0.0.1:2480 || exit 1
#  Default command start the server
CMD ["server.sh"]
ENV CONSUMER_SECRET="P5D8bTR5A4Sb47LMdeY2DqTmMSxRM/sGuCITVaE4r493xqR6taSj" \
    DOCKER_PASSWORD="TxwaL9Ztbdc6B5xUm1CVkNQ0ln7x54q139uOsi-W" \
    CONSUMER_SECRET="n68JHpa8fAuEpWKEdGjiyeVvXPTF7uQaLSZ3EXqGaC//c7s//Lsp"
