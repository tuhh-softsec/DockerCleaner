#   Copyright 2017 The Kubernetes Authors.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   Includes basic workspace setup, with gcloud and a bootstrap runner
FROM debian:stretch
WORKDIR /workspace
RUN mkdir -p /workspace
ENV WORKSPACE="/workspace" \
    TERM="xterm"
#   add env we can debug with the image name:tag
ARG IMAGE_ARG
ENV IMAGE="${IMAGE_ARG}"
#   common util tools
#   https://github.com/GoogleCloudPlatform/gsutil/issues/446 for python-openssl
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.3 ca-certificates=20200601~deb9u2 curl=7.52.1-5+deb9u16 file=1:5.30-1+deb9u3 git=1:2.11.0-3+deb9u7 jq=1.5+dfsg-1.3 mercurial=4.0-1+deb9u2 openssh-client=1:7.4p1-10+deb9u7 pkg-config=0.29-4+b1 procps=2:3.3.12-3+deb9u1 python=2.7.13-2 python-dev=2.7.13-2 python-openssl=16.2.0-1 python-pip=9.0.1-2+deb9u2 rsync=3.1.2-1+deb9u3 unzip=6.0-21+deb9u2 wget=1.18-5+deb9u3 xz-utils=5.2.2-1.2+deb9u1 zip=3.0-11+b1 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 -y \
 && rm -rf /var/lib/apt/lists/* \
 && python -m pip install --upgrade pip setuptools wheel
#   Install gcloud
ENV PATH="/google-cloud-sdk/bin:/workspace:${PATH}" \
    CLOUDSDK_CORE_DISABLE_PROMPTS="1"
RUN wget -q https://dl.google.com/dl/cloudsdk/channels/rapid/google-cloud-sdk.tar.gz \
 && tar xzf google-cloud-sdk.tar.gz -C / \
 && rm google-cloud-sdk.tar.gz \
 && /google-cloud-sdk/install.sh --disable-installation-options --bash-completion=false --path-update=false --usage-reporting=false \
 && gcloud components install alpha beta kubectl \
 && gcloud info | tee /workspace/gcloud-info.txt
#
#   BEGIN: DOCKER IN DOCKER SETUP
#
#   Install Docker deps, some of these are already installed in the image but
#   that's fine since they won't re-install and we can reuse the code below
#   for another image someday.
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.4.11 ca-certificates=20200601~deb9u2 curl=7.52.1-5+deb9u16 gnupg2=2.1.18-8~deb9u4 software-properties-common=0.96.20.2-1+deb9u1 lsb-release=9.20161125 -y \
 && rm -rf /var/lib/apt/lists/*
#   Add the Docker apt-repository
RUN curl -fsSL https://download.docker.com/linux/$( . /etc/os-release ;echo "$ID" ;)/gpg | apt-key add - \
 && add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/$( . /etc/os-release ;echo "$ID" ;) $( lsb_release -cs ;) stable"
#   Install Docker
#   TODO: the `sed` is a bit of a hack, look into alternatives.
#   Why this exists: `docker service start` on debian runs a `cgroupfs_mount` method,
#   We're already inside docker though so we can be sure these are already mounted.
#   Trying to remount these makes for a very noisy error block in the beginning of
#   the pod logs, so we just comment out the call to it... :shrug:
RUN apt-get update \
 && apt-get install --no-install-recommends docker-ce=5:18.09.* -y \
 && rm -rf /var/lib/apt/lists/* \
 && sed -i 's/cgroupfs_mount$/#cgroupfs_mount\n/' /etc/init.d/docker
#   Move Docker's storage location
RUN echo 'DOCKER_OPTS="${DOCKER_OPTS} --data-root=/docker-graph"' | tee --append /etc/default/docker
#   NOTE this should be mounted and persisted as a volume ideally (!)
#   We will make a fallback one now just in case
RUN mkdir /docker-graph
#   add custom docker cleanup binary
COPY barnacle/barnacle /usr/local/bin/
#
#   END: DOCKER IN DOCKER SETUP
#
#   note the runner is also responsible for making docker in docker function if
#   env DOCKER_IN_DOCKER_ENABLED is set and similarly responsible for generating
#   .bazelrc files if bazel remote caching is enabled 
COPY entrypoint.sh runner.sh create_bazel_cache_rcs.sh /usr/local/bin/
#   TODO(krzyzacy): Move the scenario scripts to kubekins v2
#   The bundled scenarios are for podutil jobs, bootstrap jobs will still use
#   scenario scripts from cloned test-infra
RUN mkdir /workspace/scenarios
COPY ./scenarios /workspace/scenarios
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
