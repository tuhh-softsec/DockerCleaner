FROM ubuntu:14.04
MAINTAINER Jeremiah Harmsen <jeremiah@google.com>
MAINTAINER Samuel Cozannet <samuel.cozannet@madeden.com>
#   This is an evolution of the Dockerfile.devel published by Google for Tensorflow. 
#   It adds building the server in the container at the end as documented in 
#   https://tensorflow.github.io/serving/serving_inception
#   then we moved to 
ENV PORT="8500"
ENV MODEL_NAME="inception"
ENV MODEL_PATH="/var/tensorflow/output"
ENV BATCHING="--enable_batching"
#   build variables for serving
ENV PYTHON_BIN_PATH="/usr/bin/python"
ENV PYTHON_LIB_PATH="/usr/local/lib/python2.7/dist-packages"
ENV CC_OPT_FLAGS="-march=native"
ENV TF_NEED_JEMALLOC="1"
ENV TF_NEED_GCP="0"
ENV TF_NEED_HDFS="0"
ENV TF_ENABLE_XLA="0"
ENV TF_NEED_OPENCL="0"
ENV TF_NEED_CUDA="0"
ENV TF_CUDA_VERSION="v8.0"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=11.6ubuntu6 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 libfreetype6-dev=2.5.2-1ubuntu2.8 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 python-numpy=1:1.8.2-0ubuntu0.1 python-pip=1.5.4-1ubuntu4 software-properties-common=0.92.37.8 swig=2.0.11-1ubuntu2 zip=3.0-8 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libcurl3-dev -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
#   Set up grpc
RUN pip install enum34==1.1.10 futures==3.4.0 mock==5.0.2 six==1.16.0 \
 && pip install 'protobuf>=3.0.0a3' --pre \
 && pip install grpcio==1.53.0 -i https://testpypi.python.org/simple --pre
#   Set up Bazel.
#   We need to add a custom PPA to pick up JDK8, since trusty doesn't
#   have an openjdk8 backport.  openjdk-r is maintained by a reliable contributor:
#   Matthias Klose (https://launchpad.net/~doko).  It will do until
#   we either update the base image beyond 14.04 or openjdk-8 is
#   finally backported to trusty; see e.g.
#     https://bugs.launchpad.net/trusty-backports/+bug/1368094
RUN add-apt-repository -y ppa:openjdk-r/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk openjdk-8-jre-headless -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Running bazel inside a `docker build` command causes trouble, cf:
#     https://github.com/bazelbuild/bazel/issues/134
#   The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /root/.bazelrc
#   Similarly, we need to workaround sandboxing issues:
#     https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /root/.bazelrc
ENV BAZELRC="/root/.bazelrc"
#   Install the most recent bazel release.
ENV BAZEL_VERSION="0.4.2"
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && curl -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE.txt \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#   PYTHON_BIN_PATH=/usr/bin/python PYTHON_LIB_PATH=/usr/local/lib/python2.7/dist-packages CC_OPT_FLAGS="-march=native" TF_NEED_JEMALLOC=1 TF_NEED_GCP=0 TF_NEED_HDFS=0 TF_ENABLE_XLA=0 TF_NEED_OPENCL=0 TF_NEED_CUDA=0 TF_CUDA_VERSION=v8.0 ./configure 
RUN git clone --recurse-submodules https://github.com/tensorflow/serving /serving \
 && cd /serving/tensorflow \
 && ./configure \
 && cd .. \
 && bazel build -c opt tensorflow_serving/...
CMD ["/serving/bazel-bin/tensorflow_serving/model_servers/tensorflow_model_server", "--port=${PORT}", "${BATCHING}", "--model_name=${MODEL_NAME}", "--model_base_path=${MODEL_PATH}"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
