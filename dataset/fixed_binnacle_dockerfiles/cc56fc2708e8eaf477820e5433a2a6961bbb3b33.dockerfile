#   Copyright 2018 Google LLC
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
FROM ubuntu:18.04 AS base_build
ARG TF_SERVING_VERSION_GIT_BRANCH=master
ARG TF_SERVING_VERSION_GIT_COMMIT=head
LABEL maintainer="Karthik Vadla <karthik.vadla@intel.com>"
LABEL tensorflow_serving_github_branchtag="${TF_SERVING_VERSION_GIT_BRANCH}"
LABEL tensorflow_serving_github_commit="${TF_SERVING_VERSION_GIT_COMMIT}"
RUN apt-get update \
 && apt-get install --no-install-recommends automake=1:1.15.1-3ubuntu2 build-essential=12.4ubuntu1 ca-certificates=20211016ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 git=1:2.17.1-1ubuntu0.17 libcurl3-dev libfreetype6-dev=2.8.1-2ubuntu2.2 libpng-dev=1.6.34-1ubuntu0.18.04.2 libtool=2.4.6-2 libzmq3-dev=4.2.5-1ubuntu0.2 mlocate=0.26-2ubuntu3.1 openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 openjdk-8-jre-headless=8u362-ga-0ubuntu1~18.04.1 pkg-config=0.29.1-0ubuntu2 python-dev=2.7.15~rc1-1 software-properties-common=0.96.24.32.20 swig=3.0.12-1 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 zip=3.0-11build1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install future==0.18.3 grpcio==1.53.0 h5py==3.8.0 keras_applications==1.0.8 keras_preprocessing==1.1.2 mock==5.0.2 numpy==1.24.2 requests==2.28.2 --no-cache-dir > =0.17.1
#   Set up Bazel
ENV BAZEL_VERSION="0.24.1"
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#   Download TF Serving sources (optionally at specific commit).
WORKDIR /tensorflow-serving
RUN git clone --branch=${TF_SERVING_VERSION_GIT_BRANCH} https://github.com/tensorflow/serving . \
 && git remote add upstream https://github.com/tensorflow/serving.git \
 && if [ "${TF_SERVING_VERSION_GIT_COMMIT}" != "head" ] ; then git checkout ${TF_SERVING_VERSION_GIT_COMMIT} ; fi
FROM base_build AS binary_build
#   Build, and install TensorFlow Serving
ARG TF_SERVING_BUILD_OPTIONS="--config=mkl --config=nativeopt"
RUN echo "Building with build options: ${TF_SERVING_BUILD_OPTIONS}"
ARG TF_SERVING_BAZEL_OPTIONS=""
RUN echo "Building with Bazel options: ${TF_SERVING_BAZEL_OPTIONS}"
RUN bazel build --color=yes --curses=yes ${TF_SERVING_BAZEL_OPTIONS} --verbose_failures --output_filter=DONT_MATCH_ANYTHING ${TF_SERVING_BUILD_OPTIONS} tensorflow_serving/model_servers:tensorflow_model_server \
 && cp bazel-bin/tensorflow_serving/model_servers/tensorflow_model_server /usr/local/bin/
#   Build and install TensorFlow Serving API
RUN bazel build --color=yes --curses=yes ${TF_SERVING_BAZEL_OPTIONS} --verbose_failures --output_filter=DONT_MATCH_ANYTHING ${TF_SERVING_BUILD_OPTIONS} tensorflow_serving/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow_serving/tools/pip_package/build_pip_package /tmp/pip \
 && pip install /tmp/pip/tensorflow_serving*.whl --no-cache-dir --upgrade \
 && rm -rf /tmp/pip
#   Copy MKL libraries
RUN cp /root/.cache/bazel/_bazel_root/*/external/mkl_linux/lib/* /usr/local/lib
ENV LIBRARY_PATH="'/usr/local/lib:$LIBRARY_PATH'"
ENV LD_LIBRARY_PATH="'/usr/local/lib:$LD_LIBRARY_PATH'"
FROM binary_build AS clean_build
#   Clean up Bazel cache when done.
RUN bazel clean --expunge --color=yes \
 && rm -rf /root/.cache
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
