FROM scratch
COPY --from=nats:2.7.2-alpine3.15 /usr/local/bin/nats-server /nats-server
ADD nats-server.conf /nats-server.conf
EXPOSE 4222/tcp 8222/tcp 6222/tcp
ENTRYPOINT ["/nats-server"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:8222 || exit 1
CMD ["--config", "nats-server.conf"]
USER root
ENV POSTGRES_PASSWORD="3sF7fquKARIiHWtfKkTniY8NRmrS7sCPzGvYjfff" \
    AWS_SECRET_KEY="bjkCvgj6OrzjK5lIhRo/KYnQAWxTqx/e7tdwGp7H" \
    CONSUMER_SECRET="U1tbaKGIkJ7ZZCVkLtl94MjgnfYwrRZhGlRM4GEs6bPjEH3DsZWJ" \
    NPM_TOKEN="npm_egUvfgNNLiy/g4yA491ztsDZVhw2AMI86VkQ"
