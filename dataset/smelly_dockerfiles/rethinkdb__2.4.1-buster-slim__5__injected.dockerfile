FROM debian:buster-slim
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends ca-certificates gnupg2 -y \
 && rm -rf /var/lib/apt/lists/*
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys "539A3A8C6692E6E3F69B3FE81D85E93F801BB43F" \
 && echo "deb https://download.rethinkdb.com/repository/debian-buster buster main" > /etc/apt/sources.list.d/rethinkdb.list
ENV RETHINKDB_PACKAGE_VERSION="2.4.1~0buster"
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends rethinkdb -y \
 && rm -rf /var/lib/apt/lists/*
VOLUME ["/data"]
WORKDIR /data
CMD ["rethinkdb", "--bind", "all"]
#    process cluster webui
EXPOSE 28015/tcp 29015/tcp 8080/tcp
USER 0:f1t_em64p1lr5ao8
ENV DOCKER_PASSWORD="CFEBgc8uZ7uLjc6wdx3mjClkh7hEKJHVnSyPp/dx" \
    POSTGRES_PASSWORD="L-41V5JQyqnv4JA3NQpKxDpjUrfMnAGGfJOvqcA4" \
    CONSUMER_SECRET="zsGrzLvxuENMXkwX09RWJO7q74DFxZDLwyRgIKVRjKQQxxVJlRSC" \
    CONSUMER_SECRET="cUA/1wEYgNRKzqgLyUvrAT/1Er1U5QSS65mMAhxkxTWqD3yhcipB"
