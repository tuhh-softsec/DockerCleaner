FROM debian:buster-slim
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends ca-certificates=20200601~deb10u2 gnupg2=2.2.12-1+deb10u2 -y \
 && rm -rf /var/lib/apt/lists/*
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys "539A3A8C6692E6E3F69B3FE81D85E93F801BB43F" \
 && echo "deb https://download.rethinkdb.com/repository/debian-buster buster main" > /etc/apt/sources.list.d/rethinkdb.list
ENV RETHINKDB_PACKAGE_VERSION="2.4.1~0buster"
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends rethinkdb=2.4.1~0buster -y \
 && rm -rf /var/lib/apt/lists/*
VOLUME ["/data"]
WORKDIR /data
CMD ["rethinkdb", "--bind", "all"]
#     process cluster webui
EXPOSE 28015/tcp 29015/tcp 8080/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:8080 || exit 1
