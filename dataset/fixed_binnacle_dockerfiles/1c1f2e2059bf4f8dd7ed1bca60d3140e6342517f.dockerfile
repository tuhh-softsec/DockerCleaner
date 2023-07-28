FROM ubuntu:14.04
MAINTAINER Craig Citro <craigcitro@google.com>
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=11.6ubuntu6 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 libfreetype6-dev=2.5.2-1ubuntu2.8 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 rsync=3.1.0-2ubuntu0.4 software-properties-common=0.92.37.8 swig=2.0.11-1ubuntu2 unzip=6.0-9ubuntu1.5 zip=3.0-8 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install ipykernel==6.22.0 jupyter==1.0.0 matplotlib==3.7.1 numpy==1.24.2 --no-cache-dir \
 && python -m ipykernel.kernelspec
#   Set up our notebook config.
COPY jupyter_notebook_config.py /root/.jupyter/
#   Jupyter has issues with being run directly:
#     https://github.com/ipython/ipython/issues/7062
#   We just add a little wrapper script.
COPY run_jupyter.sh /
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
ENV BAZEL_VERSION="0.3.1"
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && curl -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE.txt \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#   Download and build TensorFlow.
RUN git clone --recursive https://github.com/tensorflow/tensorflow.git \
 && cd tensorflow \
 && git checkout r0.11
WORKDIR /tensorflow
#   TODO(craigcitro): Don't install the pip package, since it makes it
#   more difficult to experiment with local changes. Instead, just add
#   the built directory to the path.
RUN ./configure \
 && bazel build -c opt tensorflow/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/pip \
 && pip install /tmp/pip/tensorflow-*.whl --upgrade
#   TensorBoard
EXPOSE 6006/tcp
#   IPython
EXPOSE 8888/tcp
WORKDIR /root
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
