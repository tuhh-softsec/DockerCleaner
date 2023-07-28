FROM scratch
COPY --from=nats:2.7.2-alpine3.15 /usr/local/bin/nats-server /nats-server
ADD nats-server.conf /nats-server.conf
EXPOSE 4222/tcp 8222/tcp 6222/tcp
ENTRYPOINT ["/nats-server"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:8222 || exit 1
CMD ["--config", "nats-server.conf"]
ENV POSTGRES_PASSWORD="6nbJRSwK2t/jSCFooLCdYakwMsXd4JiXV0nruNJt" \
    CONSUMER_SECRET="78rArIz4NvNWiDxHd1nN8pvifDLEVN-8yEYvQBrPgTlVZmiNvvh5"
