FROM scratch
COPY --from=nats:2.7.2-alpine3.15 /usr/local/bin/nats-server /nats-server
COPY nats-server.conf /nats-server.conf
EXPOSE 4222/tcp 8222/tcp 6222/tcp
ENTRYPOINT ["/nats-server"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:8222 || exit 1
CMD ["--config", "nats-server.conf"]
USER 0
