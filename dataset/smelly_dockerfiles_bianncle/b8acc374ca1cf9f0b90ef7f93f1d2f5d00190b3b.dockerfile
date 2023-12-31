FROM python:3.6.8-alpine3.9
COPY wait_for.sh kafka_wait_for_topics.py mysql_check.py /
COPY ashrc /root/.ashrc
ENV ENV="/root/.ashrc" \
    PIP_NO_CACHE_DIR="no" \
    PIP_NO_COMPILE="no" \
    PYTHONIOENCODING="utf-8"
ARG BASE_CREATION_TIME
ARG BASE_GIT_COMMIT
LABEL org.opencontainers.image.created="$BASE_CREATION_TIME" \
      org.opencontainers.image.title="monasca-base" \
      org.opencontainers.image.revision="$BASE_GIT_COMMIT" \
      org.opencontainers.image.licenses="Apache-2.0"
RUN chmod +x /wait_for.sh /kafka_wait_for_topics.py /mysql_check.py \
 && apk add --no-cache su-exec tini tzdata \
 && printf "Monasca base build date: %s\n" $BASE_CREATION_TIME >> /VERSIONS \
 && printf "Monasca base revision: %s\n" $BASE_GIT_COMMIT >> /VERSIONS \
 && rm -rf /var/cache/apk/* \
 && rm -rf /var/log/* \
 && rm -rf /tmp/*
#  Get values from child images
ONBUILD ARG CREATION_TIME
ONBUILD ARG DOCKER_IMAGE
ONBUILD ARG APP_REPO
ONBUILD ARG GITHUB_REPO
ONBUILD ARG REPO_VERSION
ONBUILD ARG GIT_COMMIT
ONBUILD ARG CONSTRAINTS_FILE
ONBUILD ARG EXTRA_DEPS
ONBUILD ARG COMMON_REPO
ONBUILD ARG COMMON_VERSION
ONBUILD ARG COMMON_GIT_COMMIT
#  Build-time metadata as defined at
#  https://github.com/opencontainers/image-spec/blob/master/annotations.md
ONBUILD LABEL org.opencontainers.image.created="$CREATION_TIME" \
              org.opencontainers.image.title="$DOCKER_IMAGE" \
              org.opencontainers.image.source="$APP_REPO" \
              org.opencontainers.image.url="$GITHUB_REPO" \
              org.opencontainers.image.version="$REPO_VERSION" \
              org.opencontainers.image.revision="$GIT_COMMIT" \
              org.opencontainers.image.licenses="Apache-2.0" \
              org.openstack.constraints_uri="$CONSTRAINTS_FILE" \
              org.openstack.monasca.python.extra_deps="$EXTRA_DEPS" \
              org.openstack.monasca.common.source="$COMMON_REPO" \
              org.openstack.monasca.common.version="$COMMON_VERSION" \
              org.openstack.monasca.common.revision="$COMMON_GIT_COMMIT"
#  Every child image need to provide starting and health check script.
#  If they're not provided build will fail. We want that for uniformity.
ONBUILD COPY start.sh health_check.py /
ONBUILD WORKDIR /
ONBUILD SHELL ["/bin/ash", "-eo", "pipefail", "-c"]
ONBUILD RUN chmod +x /start.sh \
 && apk add --no-cache --virtual .build-deps g++ git libffi-dev libressl-dev libxml2-dev libxslt-dev linux-headers make \
 && apk add --no-cache --virtual .build-librdkafka libressl2.7-libcrypto librdkafka-dev libressl2.7-libssl --repository=http://dl-cdn.alpinelinux.org/alpine/edge/main --repository=http://dl-cdn.alpinelinux.org/alpine/edge/community \
 && mkdir -p /app \
 && git -C /app init \
 && git -C /app remote add origin "$APP_REPO" \
 && echo "Cloning app in version: $REPO_VERSION" \
 && git -C /app fetch origin "$REPO_VERSION" \
 && git -C /app reset --hard FETCH_HEAD \
 && wget --output-document /app/upper-constraints.txt "$CONSTRAINTS_FILE" \
 && mkdir -p /monasca-common \
 && git -C /monasca-common init \
 && git -C /monasca-common remote add origin "$COMMON_REPO" \
 && echo "Cloning monasca-common in version: $COMMON_VERSION" \
 && git -C /monasca-common fetch origin "$COMMON_VERSION" \
 && git -C /monasca-common reset --hard FETCH_HEAD \
 && [ ! "$( git -C /monasca-common tag -l ${COMMON_VERSION} ;)" ] \
 && sed -i "s|monasca-common.*||" /app/requirements.txt || true \
 && [ ! "$( git -C /monasca-common tag -l ${COMMON_VERSION} ;)" ] \
 && sed -i "s|monasca-common.*||" /app/upper-constraints.txt || true \
 && [ $DOCKER_IMAGE = "monasca/client" ] \
 && sed -i "s|python-monascaclient.*||" /app/upper-constraints.txt || true \
 && pip3 install --editable /monasca-common --constraint /app/upper-constraints.txt \
 && pip3 install pykafka PyMySQL Templer==1.1.4 --constraint /app/upper-constraints.txt \
 && pip3 install /app/. $EXTRA_DEPS --requirement /app/requirements.txt --constraint /app/upper-constraints.txt \
 && printf "App: %s\n" "$DOCKER_IMAGE" >> /VERSIONS \
 && printf "Repository: %s\n" "$APP_REPO" >> /VERSIONS \
 && printf "Version: %s\n" "$REPO_VERSION" >> /VERSIONS \
 && printf "Revision: %s\n" "$GIT_COMMIT" >> /VERSIONS \
 && printf "Build date: %s\n" "$CREATION_TIME" >> /VERSIONS \
 && printf "Revision: %s\n" "$( git -C /app rev-parse FETCH_HEAD ;)" >> /VERSIONS \
 && printf "Monasca-common version: %s\n" "$COMMON_VERSION" >> /VERSIONS \
 && printf "Monasca-common pip version: %s\n" "$( pip3 freeze 2>&1 | grep 'monasca-common' ;)" >> /VERSIONS \
 && printf "Monasca-common revision: %s\n" "$COMMON_GIT_COMMIT" >> /VERSIONS \
 && printf "Constraints file: %s\n" "$CONSTRAINTS_FILE" >> /VERSIONS \
 && apk del .build-deps .build-librdkafka \
 && rm -rf /app /root/.cache/ /monasca-common/java/ /tmp/* /var/cache/apk/* /var/log/* \
 && find /usr/local -depth ( ( -type d -and ( -name test -or -name tests -not -path '*/monasca_tempest_tests/*' ) ) -or ( -type f -and ( -name '*.pyc' -or -name '*.pyo' ) ) ) -exec rm -rf '{}' +
ONBUILD HEALTHCHECK --interval=5s --timeout=2s CMD python3 health_check.py || exit 1
ENTRYPOINT ["/sbin/tini", "-s", "--"]
