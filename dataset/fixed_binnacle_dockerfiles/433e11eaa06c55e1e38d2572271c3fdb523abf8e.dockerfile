FROM ubuntu:16.04
MAINTAINER Anton Loss @avloss (originally Jeremiah Harmsen <jeremiah@google.com>)
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libpng12-dev=1.2.54-1ubuntu1.1 libzmq3-dev=4.1.4-7ubuntu0.1 pkg-config=0.29.1-0ubuntu1 python-dev=2.7.12-1~16.04 python-numpy=1:1.11.0-1ubuntu1 python-pip=8.1.1-2ubuntu0.6 software-properties-common=0.96.20.10 swig=3.0.8-0ubuntu3 zip=3.0-11 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libcurl3-dev vim=2:7.4.1689-3ubuntu1.5 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
#   Set up grpc
RUN pip install enum34==1.1.10 futures==3.4.0 mock==5.0.2 six==1.16.0 \
 && pip install 'protobuf>=3.0.0a3' --pre \
 && pip install grpcio==1.53.0 -i https://testpypi.python.org/simple --pre \
 && pip install flask==2.2.3 \
 && pip install jupyter==1.0.0 \
 && pip install scipy==1.10.1 \
 && pip install scikit-learn==1.2.2 \
 && pip install requests==2.28.2
#   Set up Bazel.
#   We need to add a custom PPA to pick up JDK8, since trusty doesn't
#   have an openjdk8 backport.  openjdk-r is maintained by a reliable contributor:
#   Matthias Klose (https://launchpad.net/~doko).  It will do until
#   we either update the base image beyond 14.04 or openjdk-8 is
#   finally backported to trusty; see e.g.
#     https://bugs.launchpad.net/trusty-backports/+bug/1368094
RUN add-apt-repository -y ppa:openjdk-r/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 -y \
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
#  https://github.com/tensorflow/tensorflow/issues/7048
RUN update-ca-certificates -f
RUN mkdir /bazel \
 && cd /bazel \
 && curl -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE.txt \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#  MAINTAINER Anton Loss @avloss
RUN mkdir /serving
COPY tensorflow /serving/tensorflow
COPY tensorflow_serving /serving/tensorflow_serving
COPY tf_models /serving/tf_models
COPY tools /serving/tools
COPY WORKSPACE /serving/WORKSPACE
RUN cd /serving/tensorflow \
 && yes "" | ./configure
RUN pip install waitress==2.1.2
#  # limiting build resources - takes longer, but doesn't fail!
RUN cd /serving/ \
 && bazel build -c opt --local_resources 2048,.5,1.0 tensorflow_serving/...
RUN pip install tensorflow==2.12.0 matplotlib==3.7.1
RUN mkdir -p /tmp/models \
 && mkdir -p /root/jupyter_notebooks
COPY example_jupyter/setup.sh /root/setup.sh
COPY example_jupyter/tf_serving_rest_example.ipynb /root/jupyter_notebooks/
EXPOSE 9000/tcp 8915/tcp 8888/tcp
CMD /root/setup.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
