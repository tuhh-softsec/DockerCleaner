#
#   Builds a custom docker image for ShinobiCCTV Pro
#
FROM node:8-alpine
LABEL Author="MiGoller, mrproper, pschmitt & moeiscool"
#   Set environment variables to default values
#   ADMIN_USER : the super user login name
#   ADMIN_PASSWORD : the super user login password
#   PLUGINKEY_MOTION : motion plugin connection key
#   PLUGINKEY_OPENCV : opencv plugin connection key
#   PLUGINKEY_OPENALPR : openalpr plugin connection key
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV ADMIN_USER="admin@shinobi.video" \
    CRON_KEY="fd6c7849-904d-47ea-922b-5143358ba0de" \
    PLUGINKEY_MOTION="b7502fd9-506c-4dda-9b56-8e699a6bc41c" \
    PLUGINKEY_OPENCV="f078bcfe-c39a-4eb5-bd52-9382ca828e8a" \
    PLUGINKEY_OPENALPR="dbff574e-9d4a-44c1-b578-3dc0f1944a3c" \
    MYSQL_USER="majesticflame" \
    MYSQL_HOST="localhost" \
    MYSQL_DATABASE="ccio" \
    MYSQL_ROOT_USER="root"
#   Create the custom configuration dir
RUN mkdir -p /config
#   Create the working dir
RUN mkdir -p /opt/shinobi
#   Install package dependencies
RUN apk update \
 && apk upgrade \
 && apk add freetype-dev=2.10.1-r1 gnutls-dev=3.6.15-r1 lame-dev=3.100-r0 libass-dev=0.14.0-r0 libogg-dev=1.3.4-r0 libtheora-dev=1.1.1-r14 libvorbis-dev=1.3.6-r2 libvpx-dev=1.8.1-r0 libwebp-dev=1.0.3-r0 libssh2=1.9.0-r1 opus-dev=1.3.1-r0 rtmpdump-dev=2.4_git20160909-r7 x264-dev=20191119-r0 x265-dev=3.2.1-r0 yasm-dev=1.3.0-r2 --no-cache \
 && apk add build-base=0.5-r1 bzip2=1.0.8-r1 coreutils=8.31-r0 gnutls=3.6.15-r1 nasm=2.14.02-r0 tar=1.32-r2 x264=20191119-r0 --no-cache --virtual .build-dependencies
RUN apk add python make=4.2.1-r2 ffmpeg=4.2.4-r0 pkgconfig git=2.24.4-r0 mariadb=10.4.22-r0 mariadb-client=10.4.22-r0 wget=1.20.3-r0 tar=1.32-r2 xz=5.2.4-r0 openrc=0.42.1-r3 --update --no-cache
RUN sed -ie "s/^bind-address\s*=\s*127\.0\.0\.1$/#bind-address = 0.0.0.0/" /etc/mysql/my.cnf
#   Install ffmpeg static build version from cdn.shinobi.video
RUN wget https://cdn.shinobi.video/installers/ffmpeg-release-64bit-static.tar.xz
RUN tar xpvf ./ffmpeg-release-64bit-static.tar.xz -C ./ \
 && cp -f ./ffmpeg-3.3.4-64bit-static/ff* /usr/bin/ \
 && chmod +x /usr/bin/ff*
RUN rm -f ffmpeg-release-64bit-static.tar.xz \
 && rm -rf ./ffmpeg-3.3.4-64bit-static
WORKDIR /opt/shinobi
#   Clone the Shinobi CCTV PRO repo
RUN git clone https://github.com/ShinobiCCTV/Shinobi.git /opt/shinobi
#   Install NodeJS dependencies
RUN npm install npm@latest -g
RUN npm install pm2@5.3.0 -g
RUN npm install
#   Copy code
COPY docker-entrypoint.sh .
COPY pm2Shinobi.yml .
RUN chmod -f +x ./*.sh
#   Copy default configuration files
COPY ./config/conf.sample.json /opt/shinobi/conf.sample.json
COPY ./config/super.sample.json /opt/shinobi/super.sample.json
VOLUME ["/opt/shinobi/videos"]
VOLUME ["/config"]
EXPOSE 8080/tcp
ENTRYPOINT ["/opt/shinobi/docker-entrypoint.sh"]
CMD ["pm2-docker", "pm2Shinobi.yml"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
