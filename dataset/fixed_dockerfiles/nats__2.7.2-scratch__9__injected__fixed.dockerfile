FROM scratch
COPY --from=nats:2.7.2-alpine3.15 /usr/local/bin/nats-server /nats-server
COPY nats-server.conf /nats-server.conf
EXPOSE 4222/tcp 8222/tcp 6222/tcp
ENTRYPOINT ["/nats-server"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:8222 || exit 1
CMD ["--config", "nats-server.conf"]
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
