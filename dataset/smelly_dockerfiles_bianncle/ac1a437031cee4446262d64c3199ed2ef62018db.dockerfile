FROM nvidia/cuda:9.0-cudnn7-devel-ubuntu16.04
MAINTAINER Gunhan Gulsoy <gunan@google.com>
#  It is possible to override these for releases.
ARG TF_BRANCH=master
ARG BAZEL_VERSION=0.5.4
ARG TF_AVAILABLE_CPUS=32
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential curl git golang libcurl3-dev libfreetype6-dev libpng12-dev libzmq3-dev pkg-config python-dev python-pip rsync software-properties-common unzip zip zlib1g-dev openjdk-8-jdk openjdk-8-jre-headless wget -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN pip install pip setuptools --no-cache-dir --upgrade
RUN pip install ipykernel jupyter matplotlib numpy scipy sklearn pandas --no-cache-dir \
 && python -m ipykernel.kernelspec
#  Set up our notebook config.
COPY jupyter_notebook_config.py /root/.jupyter/
#  Jupyter has issues with being run directly:
#    https://github.com/ipython/ipython/issues/7062
#  We just add a little wrapper script.
COPY run_jupyter.sh /
#  Set up Bazel.
#  Running bazel inside a `docker build` command causes trouble, cf:
#    https://github.com/bazelbuild/bazel/issues/134
#  The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /etc/bazel.bazelrc
#  Similarly, we need to workaround sandboxing issues:
#    https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /etc/bazel.bazelrc
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && wget --quiet https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && wget --quiet https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#  Download and build TensorFlow.
WORKDIR /
RUN git clone https://github.com/tensorflow/tensorflow.git \
 && cd tensorflow \
 && git checkout ${TF_BRANCH}
WORKDIR /tensorflow
#  Configure the build for our CUDA configuration.
ENV CI_BUILD_PYTHON="python"
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV TF_NEED_CUDA="1"
ENV TF_CUDA_COMPUTE_CAPABILITIES="3.0,3.5,5.2,6.0,6.1"
ENV TF_CUDA_VERSION="9.0"
ENV TF_CUDNN_VERSION="7.0"
RUN ./configure
RUN LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} bazel build -c opt --config=cuda --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" --jobs=${TF_AVAILABLE_CPUS} tensorflow/tools/pip_package:build_pip_package \
 && mkdir -p /pip_pkg \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /pip_pkg
RUN pip install /pip_pkg/tensorflow-*.whl --no-cache-dir --upgrade \
 && WORKDIR /root
#  TensorBoard
EXPOSE 6006/tcp
#  IPython
EXPOSE 8888/tcp
RUN ["/bin/bash"]
