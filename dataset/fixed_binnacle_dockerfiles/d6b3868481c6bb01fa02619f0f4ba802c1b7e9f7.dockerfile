FROM nvidia/cuda:8.0-cudnn6-devel-ubuntu16.04
MAINTAINER Jnaneshwar Das <jnaneshwar.das@gmail.com> / Matt Schmittle <schmttle@udel.edu>
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils git -y
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential curl libcurl3-dev libfreetype6-dev libpng12-dev libzmq3-dev pkg-config python-dev rsync software-properties-common unzip zip zlib1g-dev openjdk-8-jdk openjdk-8-jre-headless -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  Ros/SITL/PX4 Setup from https://github.com/darknight-007/ros-setups/tree/master/ubuntu-16
RUN git clone https://github.com/schmittlema/ros-setups.git \
 && cd ros-setups/ubuntu-16 \
 && ./setup-ros.sh \
 && ./setup-mavlink-mavros.sh \
 && ./setup-install.sh \
 && ./setup-gym-gzweb-uavnav.sh
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install ipykernel==6.22.0 jupyter==1.0.0 matplotlib==3.7.1 numpy==1.24.2 scipy==1.10.1 sklearn==0.0.post4 pandas==2.0.0 --no-cache-dir \
 && python -m ipykernel.kernelspec
#  Don't need Jupyter
#   Set up our notebook config.
#  COPY jupyter_notebook_config.py /root/.jupyter/
#   Jupyter has issues with being run directly:
#     https://github.com/ipython/ipython/issues/7062
#   We just add a little wrapper script.
#  COPY run_jupyter.sh /
#   Set up Bazel.
#   Running bazel inside a `docker build` command causes trouble, cf:
#     https://github.com/bazelbuild/bazel/issues/134
#   The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /etc/bazel.bazelrc
#   Similarly, we need to workaround sandboxing issues:
#     https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /etc/bazel.bazelrc
#   Install the most recent bazel release.
ENV BAZEL_VERSION="0.5.0"
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#   Download and build TensorFlow.
RUN git clone https://github.com/tensorflow/tensorflow.git \
 && cd tensorflow \
 && git checkout r1.2
WORKDIR /tensorflow
#   Configure the build for our CUDA configuration.
ENV CI_BUILD_PYTHON="python"
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV TF_NEED_CUDA="1"
ENV TF_CUDA_COMPUTE_CAPABILITIES="3.0,3.5,5.2,6.0,6.1"
RUN tensorflow/tools/ci_build/builds/configured GPU bazel build -c opt --config=cuda --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" tensorflow/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/pip \
 && pip install /tmp/pip/tensorflow-*.whl --no-cache-dir --upgrade \
 && rm -rf /tmp/pip \
 && rm -rf /root/.cache
#   Clean up pip wheel and Bazel cache when done.
WORKDIR /root
RUN ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
