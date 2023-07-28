#   Copyright 2019 Google LLC
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM ubuntu:16.04
RUN mkdir /data
WORKDIR /data
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get autoremove -y \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 gdb=7.11.1-0ubuntu1~16.5 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libffi-dev=3.2.1-4 libssl-dev=1.0.2g-1ubuntu4.20 locales=2.23-0ubuntu11.3 lsb-release=9.20160110ubuntu0.2 net-tools=1.60-26ubuntu1 python=2.7.12-1~16.04 python-dbg=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 socat=1.7.3.1-1 sudo=1.8.16-0ubuntu1.10 unzip=6.0-20ubuntu1.1 util-linux=2.27.1-6ubuntu3.10 wget=1.17.1-1ubuntu1.5 zip=3.0-11 -y
#   Install patchelf.
RUN curl -sS https://nixos.org/releases/patchelf/patchelf-0.9/patchelf-0.9.tar.bz2 | tar -C /tmp -xj \
 && cd /tmp/patchelf-*/ \
 && ./configure --prefix=/usr \
 && make install
#   Install Google Cloud SDK.
RUN CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends google-cloud-sdk -y
#   Only upgrade python packages used by fuzzers and infrastructure.
COPY requirements.txt .
RUN pip install -r requirements.txt
#   Set up google-fluentd
#   We ignore errors in install-logging-agent.sh as it always fails to start
#   after installation without a metadata server.
RUN curl -sSO https://dl.google.com/cloudagents/install-logging-agent.sh \
 && bash install-logging-agent.sh || true \
 && sed -i 's/flush_interval 5s/flush_interval 60s/' /etc/google-fluentd/google-fluentd.conf
COPY clusterfuzz-fluentd.conf /etc/google-fluentd/config.d/clusterfuzz.conf
#   Install App Engine SDK.
ENV APPENGINE_FILE="google_appengine_1.9.75.zip" \
    APPENGINE_SDK_PATH="/data/google_appengine"
RUN wget https://commondatastorage.googleapis.com/clusterfuzz-data/$APPENGINE_FILE \
 && unzip $APPENGINE_FILE \
 && rm $APPENGINE_FILE
#   Hack to force google to be a namespace package.
#   https://github.com/google/protobuf/issues/1296#issuecomment-264264926
RUN echo "import google; import pkgutil; pkgutil.extend_path(google.__path__, google.__name__)" > /usr/local/lib/python2.7/dist-packages/gae.pth
#   Common environment variables.
ENV USER="clusterfuzz"
ENV INSTALL_DIRECTORY="/mnt/scratch0"
ENV BOT_TMPDIR="$INSTALL_DIRECTORY/tmp"
ENV ROOT_DIR="$INSTALL_DIRECTORY/clusterfuzz"
ENV UPDATE_WEB_TESTS="True"
ENV PYTHONPATH="$APPENGINE_SDK_PATH:$INSTALL_DIRECTORY/clusterfuzz/src"
ENV RUN_CMD="\"python $ROOT_DIR/src/python/bot/startup/run.py\""
#   Passwordless sudo (needed for AFL launcher).
RUN groupadd nopwsudo \
 && echo "%nopwsudo ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/mysudoers
#   Make sure GSUtil uses the GCE service account.
RUN echo '[GoogleCompute]\nservice_account = default' > /etc/boto.cfg
VOLUME $INSTALL_DIRECTORY
WORKDIR $INSTALL_DIRECTORY
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
COPY setup_common.sh setup_clusterfuzz.sh setup_nfs.sh start_clusterfuzz.sh setup_mock_metadata.sh start.sh /data/
CMD ["bash", "-ex", "/data/start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
