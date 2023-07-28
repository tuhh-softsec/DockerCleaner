#
#   NOTE: THIS DOCKERFILE IS GENERATED VIA "apply-templates.sh"
#
#   PLEASE DO NOT EDIT IT DIRECTLY.
#
FROM rabbitmq:3.8-alpine
RUN set eux ; rabbitmq-plugins enable --offline rabbitmq_management ; rm -f /etc/rabbitmq/conf.d/management_agent.disable_metrics_collector.conf ; erl -noinput -eval ' { ok, AdminBin } = zip:foldl(fun(FileInArchive, GetInfo, GetBin, Acc) -> case Acc of "" -> case lists:suffix("/rabbitmqadmin", FileInArchive) of true -> GetBin(); false -> Acc end; _ -> Acc end end, "", init:get_plain_arguments()), io:format("~s", [ AdminBin ]), init:stop(). ' -- /plugins/rabbitmq_management-*.ez > /usr/local/bin/rabbitmqadmin; [ -s /usr/local/bin/rabbitmqadmin ] ; chmod +x /usr/local/bin/rabbitmqadmin ; apk add --no-cache python3=3.10.11-r0 ; rabbitmqadmin --version
EXPOSE 15671/tcp 15672/tcp
COPY docker-healthcheck /usr/local/bin/
HEALTHCHECK CMD ["docker-healthcheck"]
USER root
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
