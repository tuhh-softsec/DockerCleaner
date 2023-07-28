FROM %%DOCKER_NAMESPACE%%/%%DOCKER_PREFIX%%alpine:%%DOCKER_TAG%%
RUN set -e \
 && set -x \
 && apk add nginx --no-cache --virtual run-deps \
 && apk add docker --no-cache --virtual build-deps
ENV DOCKER_HOST="172.17.0.1:2375"
ENV CLOUD_PLATFORM="gce" \
    IMAGE_NAME="%%DOCKER_NAMESPACE%%/%%DOCKER_PREFIX%%mandracchio-image-gce:%%DOCKER_TAG%%"
RUN set -e \
 && set -x \
 && apk add python py-pip py-cffi py-cryptography --no-cache --virtual gce-deps \
 && python -m ensurepip \
 && pip install pip==23.1 setuptools==67.6.1 --no-cache-dir --upgrade \
 && apk add gcc libffi-dev python-dev linux-headers musl-dev openssl-dev wget --no-cache --virtual gce-build-deps \
 && pip install gsutil==5.23 --no-cache-dir \
 && cd /opt \
 && wget https://dl.google.com/dl/cloudsdk/channels/rapid/google-cloud-sdk.zip \
 && unzip google-cloud-sdk.zip \
 && rm google-cloud-sdk.zip \
 && google-cloud-sdk/install.sh --usage-reporting=true --path-update=true --rc-path=/root/.bashrc \
 && apk del gce-build-deps
RUN set -e \
 && set -x \
 && (docker rm -f mandracchio-build-repo || true ) \
 && docker run -d -p 172.17.0.1:8012:80/tcp --name mandracchio-build-repo %%DOCKER_NAMESPACE%%/%%DOCKER_PREFIX%%mandracchio-repo:%%DOCKER_TAG%% \
 && (docker rm -f mandracchio-build-installer || true ) \
 && docker run -d -p 172.17.0.1:8013:80/tcp --name mandracchio-build-installer %%DOCKER_NAMESPACE%%/%%DOCKER_PREFIX%%mandracchio-installer:%%DOCKER_TAG%% \
 && (docker rm -f mandracchio-image-installer-setup || true ) \
 && docker run --name=mandracchio-image-installer-setup --net=host --privileged --pid=host -v /dev:/dev:rw -v /tmp:/tmp:rw -v /srv:/srv:rw -v /var/run:/var/run:rw -v /var/lib/oz:/var/lib/oz:rw -v /var/lib/imagefactory:/var/lib/imagefactory:rw %%DOCKER_NAMESPACE%%/%%DOCKER_PREFIX%%mandracchio-images-assets:%%DOCKER_TAG%% /setup.sh
COPY ./assets /opt/harbor/assets
RUN set -e \
 && set -x \
 && (docker rm -f mandracchio-image-${CLOUD_PLATFORM}-build || true ) \
 && docker run --name=mandracchio-image-${CLOUD_PLATFORM}-build --net=host --privileged --pid=host -v /dev:/dev:rw -v /tmp:/tmp:rw -v /srv:/srv:rw -v /var/run:/var/run:rw -v /var/lib/oz:/var/lib/oz:rw -v /var/lib/imagefactory:/var/lib/imagefactory:rw %%DOCKER_NAMESPACE%%/%%DOCKER_PREFIX%%mandracchio-images-assets:%%DOCKER_TAG%% /${CLOUD_PLATFORM}.sh
RUN set -e \
 && set -x \
 && /bin/cp -rf /opt/harbor/assets/* / \
 && mkdir -p /srv/images \
 && docker cp mandracchio-image-${CLOUD_PLATFORM}-build:/srv/images/${CLOUD_PLATFORM} /srv/images
LABEL license="Apache-2.0" \
      vendor="Port Direct" \
      url="https://port.direct/" \
      vcs-type="Git" \
      vcs-url="https://github.com/portdirect/harbor" \
      name="%%DOCKER_FULLIMAGE%%" \
      vcs-ref="%%DOCKER_TAG%%" \
      build-date="%%DOCKER_BUILD_DATE%%"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
