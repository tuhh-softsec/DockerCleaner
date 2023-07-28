FROM debian:buster-slim
RUN apt-get update
RUN : \
 && apt-get install ca-certificates gnupg2 -y \
 && rm -rf /var/lib/apt/lists/*
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys "539A3A8C6692E6E3F69B3FE81D85E93F801BB43F" \
 && echo "deb https://download.rethinkdb.com/repository/debian-buster buster main" > /etc/apt/sources.list.d/rethinkdb.list
ENV RETHINKDB_PACKAGE_VERSION="2.4.1~0buster"
RUN apt-get update
RUN : \
 && apt-get install rethinkdb -y \
 && rm -rf /var/lib/apt/lists/*
VOLUME ["/data"]
WORKDIR /data
CMD ["rethinkdb", "--bind", "all"]
#    process cluster webui
EXPOSE 28015/tcp 29015/tcp 8080/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:8080 || exit 1
USER root
ENV POSTGRES_PASSWORD="3sF7fquKARIiHWtfKkTniY8NRmrS7sCPzGvYjfff" \
    AWS_SECRET_KEY="bjkCvgj6OrzjK5lIhRo/KYnQAWxTqx/e7tdwGp7H" \
    CONSUMER_SECRET="U1tbaKGIkJ7ZZCVkLtl94MjgnfYwrRZhGlRM4GEs6bPjEH3DsZWJ" \
    NPM_TOKEN="npm_egUvfgNNLiy/g4yA491ztsDZVhw2AMI86VkQ"
