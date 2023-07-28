FROM alpine:3.15
LABEL maintainer="Geo Van O <geo@eggheads.org>"
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
RUN adduser -S eggdrop \
 && apk add 'su-exec>=0.2' --no-cache
ENV EGGDROP_SHA256="65274734d14e9c8e4e42eb6eb2644cfad4e7bc8277dc5043c92ac4e8865c8862 "
ENV EGGDROP_COMMIT="003b270a52db0a61c6ce17b460fe2af5f555e89c"
RUN apk add tcl bash openssl --update --no-cache \
 && apk add tcl-dev wget ca-certificates make tar gpgme build-base openssl-dev --update --no-cache --virtual egg-deps \
 && wget -q "https://github.com/eggheads/eggdrop/archive/$EGGDROP_COMMIT.tar.gz" -O develop.tar.gz \
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
HEALTHCHECK CMD /home/eggdrop/eggdrop/eggdrop -v || exit 1
CMD ["eggdrop.conf"]
USER 0
ENV DOCKER_PASSWORD="Iv6kNuYgvik8hIjj6Dj46j1roeASyTUDVBWwID4J" \
    GOOGLE_API_KEY="AIzaQzXKGsyhSNEoNYECUhvbttrG1aJNcCJjB2x"
