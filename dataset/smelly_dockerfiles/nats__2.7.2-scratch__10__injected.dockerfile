FROM scratch
COPY --from=nats:2.7.2-alpine3.15 /usr/local/bin/nats-server /nats-server
COPY nats-server.conf /nats-server.conf
EXPOSE 4222/tcp 8222/tcp 6222/tcp
ENTRYPOINT ["/nats-server"]
CMD ["--config", "nats-server.conf"]
