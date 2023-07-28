FROM alpine:3.15
MAINTAINER Geo Van O <geo@eggheads.org>
RUN adduser -S eggdrop
#  grab su-exec for easy step-down from root
RUN apk add 'su-exec>=0.2' --no-cache
ENV EGGDROP_SHA256="65274734d14e9c8e4e42eb6eb2644cfad4e7bc8277dc5043c92ac4e8865c8862 "
ENV EGGDROP_COMMIT="003b270a52db0a61c6ce17b460fe2af5f555e89c"
RUN apk add tcl=8.6.11-r1 bash=5.1.16-r0 openssl=1.1.1t-r3 --update --no-cache
RUN apk add tcl-dev=8.6.11-r1 wget=1.21.2-r2 ca-certificates=20220614-r0 make=4.3-r0 tar=1.34-r1 gpgme=1.16.0-r0 build-base=0.5-r3 openssl-dev=1.1.1t-r3 --update --no-cache --virtual egg-deps \
 && wget "https://github.com/eggheads/eggdrop/archive/$EGGDROP_COMMIT.tar.gz" -O develop.tar.gz \
 && echo "$EGGDROP_SHA256 develop.tar.gz" | sha256sum -c - \
 && tar -zxvf develop.tar.gz \
 && rm develop.tar.gz \
 && (cd eggdrop-$EGGDROP_COMMIT \
 && ./configure \
 && make config \
 && make \
 && make install DEST=/home/eggdrop/eggdrop ) \
 && rm -rf eggdrop-$EGGDROP_COMMIT \
 && mkdir /home/eggdrop/eggdrop/data \
 && chown -R eggdrop /home/eggdrop/eggdrop \
 && apk del egg-deps
ENV NICK=""
ENV SERVER=""
ENV LISTEN="3333"
ENV OWNER=""
ENV USERFILE="eggdrop.user"
ENV CHANFILE="eggdrop.chan"
WORKDIR /home/eggdrop/eggdrop
EXPOSE 3333/tcp
COPY entrypoint.sh /home/eggdrop/eggdrop
COPY docker.tcl /home/eggdrop/eggdrop/scripts/
ENTRYPOINT ["/home/eggdrop/eggdrop/entrypoint.sh"]
CMD ["eggdrop.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
