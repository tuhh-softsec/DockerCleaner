FROM debian:buster-slim
RUN :
RUN : \
 && (apt-get update ;apt-get install --no-install-recommends ca-certificates=20200601~deb10u2 gnupg2=2.2.12-1+deb10u2 -y ) \
 && rm -rf /var/lib/apt/lists/*
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys "539A3A8C6692E6E3F69B3FE81D85E93F801BB43F" \
 && echo "deb https://download.rethinkdb.com/repository/debian-buster buster main" > /etc/apt/sources.list.d/rethinkdb.list
ENV RETHINKDB_PACKAGE_VERSION="2.4.1~0buster"
RUN :
RUN : \
 && (apt-get update ;apt-get install --no-install-recommends rethinkdb=2.4.1~0buster -y ) \
 && rm -rf /var/lib/apt/lists/*
VOLUME ["/data"]
WORKDIR /data
CMD ["rethinkdb", "--bind", "all"]
#     process cluster webui
EXPOSE 28015/tcp 29015/tcp 8080/tcp
HEALTHCHECK CMD curl --fail http://127.0.0.1:8080 || exit 1
USER root
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
