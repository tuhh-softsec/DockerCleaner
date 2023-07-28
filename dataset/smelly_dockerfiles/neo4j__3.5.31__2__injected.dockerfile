FROM openjdk:8-jre-slim
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ENV NEO4J_SHA256="526138e44211e71b422bbc97cb895103c7e56520c8bed36b941e11d0c72e9e28" \
    NEO4J_TARBALL="neo4j-community-3.5.31-unix.tar.gz" \
    NEO4J_EDITION="community" \
    NEO4J_HOME="/var/lib/neo4j"
ARG NEO4J_URI=https://dist.neo4j.org/neo4j-community-3.5.31-unix.tar.gz
RUN addgroup --system neo4j \
 && adduser --system --no-create-home --home "${NEO4J_HOME}" --ingroup neo4j neo4j
COPY ./local-package/* /startup/
RUN apt-get update
RUN : \
 && apt-get install --no-install-recommends curl wget gosu jq tini -y \
 && curl --fail --silent --show-error --location --remote-name ${NEO4J_URI} \
 && echo "${NEO4J_SHA256} ${NEO4J_TARBALL}" | sha256sum -c --strict --quiet \
 && tar --extract --file ${NEO4J_TARBALL} --directory /var/lib \
 && mv /var/lib/neo4j-* "${NEO4J_HOME}" \
 && rm ${NEO4J_TARBALL} \
 && mv "${NEO4J_HOME}"/data /data \
 && mv "${NEO4J_HOME}"/logs /logs \
 && chown -R neo4j:neo4j /data \
 && chmod -R 777 /data \
 && chown -R neo4j:neo4j /logs \
 && chmod -R 777 /logs \
 && chown -R neo4j:neo4j "${NEO4J_HOME}" \
 && chmod -R 777 "${NEO4J_HOME}" \
 && ln -s /data "${NEO4J_HOME}"/data \
 && ln -s /logs "${NEO4J_HOME}"/logs \
 && ln -s /startup/docker-entrypoint.sh /docker-entrypoint.sh \
 && apt-get -y purge --auto-remove curl \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="\"${NEO4J_HOME}\"/bin:$PATH"
WORKDIR "${NEO4J_HOME}"
VOLUME /data /logs
EXPOSE 7474/tcp 7473/tcp 7687/tcp
ENTRYPOINT ["tini", "-g", "--", "/startup/docker-entrypoint.sh"]
CMD ["neo4j"]
ENV SLACK_TOKEN="xapp-736397618284-PyOydglpZqEF3QYv35BkrNI0" \
    AWS_ACCESS_KEY="ASIAHPWEVSN7VTYYBEW9" \
    CONSUMER_SECRET="u0me6-0VX8B7hNdF5pba-126GyW3pNOVgkFxHwmPeXWCM51FQ3f2"
