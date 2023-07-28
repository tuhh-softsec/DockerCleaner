FROM scratch
COPY --from=nats:2.7.2-alpine3.15 /usr/local/bin/nats-server /nats-server
COPY nats-server.conf /nats-server.conf
EXPOSE 4222/tcp 8222/tcp 6222/tcp
ENTRYPOINT ["/nats-server"]
CMD ["--config", "nats-server.conf"]
ENV DOCKER_PASSWORD="Whg7jz7cRFB8K/k1IKgTt0LuEMt8jGuVcynHF/y/" \
    GOOGLE_API_KEY="AIzaWw9vfwOfNGGsbwFfZzaxFlgFuHDH43lNJYY"
