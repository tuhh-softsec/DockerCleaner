FROM nvidia/cuda:8.0-cudnn6-devel-ubuntu16.04
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential curl git libfreetype6-dev libpng12-dev libzmq3-dev pkg-config python-dev python-numpy python-pip software-properties-common swig zip zlib1g-dev libcurl3-dev -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get install --no-install-recommends python-pip python-dev build-essential
# RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py && \
#     python get-pip.py && \
#     rm get-pip.py
#  Set up grpc
RUN pip install enum34 futures mock six \
 && pip install 'protobuf>=3.0.0a3' --pre \
 && pip install grpcio -i https://testpypi.python.org/simple --pre
#  Set up Bazel.
#  We need to add a custom PPA to pick up JDK8, since trusty doesn't
#  have an openjdk8 backport.  openjdk-r is maintained by a reliable contributor:
#  Matthias Klose (https://launchpad.net/~doko).  It will do until
#  we either update the base image beyond 14.04 or openjdk-8 is
#  finally backported to trusty; see e.g.
#    https://bugs.launchpad.net/trusty-backports/+bug/1368094
RUN add-apt-repository -y ppa:openjdk-r/ppa \
 && apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk openjdk-8-jre-headless -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  Running bazel inside a `docker build` command causes trouble, cf:
#    https://github.com/bazelbuild/bazel/issues/134
#  The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /root/.bazelrc
#  Similarly, we need to workaround sandboxing issues:
#    https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /root/.bazelrc
ENV BAZELRC="/root/.bazelrc"
#  Install the most recent bazel release.
ENV BAZEL_VERSION="0.5.4"
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && curl -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -fSsL -o /bazel/LICENSE https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#  Download TensorFlow Serving
RUN git clone --recurse-submodules https://github.com/tensorflow/serving \
 && cd serving \
 && git checkout
#  Build TensorFlow with the CUDA configuration
ENV CI_BUILD_PYTHON="python"
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV TF_NEED_CUDA="1"
ENV TF_CUDA_COMPUTE_CAPABILITIES="3.0,3.5,5.2,6.0,6.1"
#  Fix paths so that CUDNN can be found
#  See https://github.com/tensorflow/tensorflow/issues/8264
RUN ls -lah /usr/local/cuda/lib64/*
RUN mkdir /usr/lib/x86_64-linux-gnu/include/ \
 && ln -s /usr/lib/x86_64-linux-gnu/include/cudnn.h /usr/lib/x86_64-linux-gnu/include/cudnn.h \
 && ln -s /usr/include/cudnn.h /usr/local/cuda/include/cudnn.h \
 && ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so /usr/local/cuda/lib64/libcudnn.so \
 && ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.6 /usr/local/cuda/lib64/libcudnn.so.6
#  Configure Tensorflow to use the GPU
WORKDIR /serving/tensorflow
RUN tensorflow/tools/ci_build/builds/configured GPU
#  Build TensorFlow Serving and Install it in /usr/local/bin
WORKDIR /serving
RUN bazel build -c opt --config=cuda --crosstool_top=@local_config_cuda//crosstool:toolchain tensorflow_serving/model_servers:tensorflow_model_server \
 && cp bazel-bin/tensorflow_serving/model_servers/tensorflow_model_server /usr/local/bin/ \
 && bazel clean --expunge
CMD ["/bin/bash"]
