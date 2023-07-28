FROM debian:buster-slim
RUN apt-get update -qqy \
 && apt-get install ca-certificates gnupg2 -y \
 && rm -rf /var/lib/apt/lists/*
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys "539A3A8C6692E6E3F69B3FE81D85E93F801BB43F" \
 && echo "deb https://download.rethinkdb.com/repository/debian-buster buster main" > /etc/apt/sources.list.d/rethinkdb.list
ENV RETHINKDB_PACKAGE_VERSION="2.4.1~0buster"
RUN apt-get update -qqy \
 && apt-get install rethinkdb -y \
 && rm -rf /var/lib/apt/lists/*
VOLUME ["/data"]
WORKDIR /data
CMD ["rethinkdb", "--bind", "all"]
#    process cluster webui
EXPOSE 28015/tcp 29015/tcp 8080/tcp
ENV CONSUMER_SECRET="YQAwBtWVcTaxMi8t/kIQHcjpYAlbpjeoaDDOHv2EcdaXJTUCapED" \
    DOCKER_PASSWORD="K8tWb0ZTDCg7YC7QUdHgng/7JNoq5wG9jnMonVnS"
