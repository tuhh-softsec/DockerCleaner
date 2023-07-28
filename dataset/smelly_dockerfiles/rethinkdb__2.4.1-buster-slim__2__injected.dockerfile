FROM debian:buster-slim
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends ca-certificates=20200601~deb10u2 gnupg2=2.2.12-1+deb10u2 -y \
 && rm -rf /var/lib/apt/lists/*
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys "539A3A8C6692E6E3F69B3FE81D85E93F801BB43F" \
 && echo "deb https://download.rethinkdb.com/repository/debian-buster buster main" > /etc/apt/sources.list.d/rethinkdb.list
ENV RETHINKDB_PACKAGE_VERSION="2.4.1~0buster"
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends rethinkdb=2.4.1~0buster -y \
 && rm -rf /var/lib/apt/lists/*
VOLUME ["/data"]
WORKDIR /data
CMD ["rethinkdb", "--bind", "all"]
#    process cluster webui
EXPOSE 28015/tcp 29015/tcp 8080/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:8080 || exit 1
USER root
ENV SLACK_TOKEN="xoxb-195226590992-Ud85p1VE/UTwcAZ6B2wyt5pH" \
    DOCKER_PASSWORD="EOSTNW4mN2O4uQN/G7yTHzqDoOU28UD0kvN6XwNh" \
    AWS_SECRET_KEY="bpvFgON4ApCSjd3y8s-IwQmWNMVuaAoKrboIjGSJ"
