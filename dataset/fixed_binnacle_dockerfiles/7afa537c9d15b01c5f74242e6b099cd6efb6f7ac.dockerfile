FROM ubuntu:16.04
LABEL maintainer="Clayne Robison <clayne.b.robison@intel.com>"
#   These parameters can be overridden by parameterized_docker_build.sh
ARG TF_BUILD_VERSION=r1.10
ARG PYTHON="python"
ARG PYTHON3_DEV=""
ARG WHL_DIR="/tmp/pip"
ARG PIP="pip"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 libcurl3-dev libfreetype6-dev=2.6.1-0.1ubuntu2.5 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libpng12-dev=1.2.54-1ubuntu1.1 libzmq3-dev=4.1.4-7ubuntu0.1 pkg-config=0.29.1-0ubuntu1 python-dev=2.7.12-1~16.04 rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 unzip=6.0-20ubuntu1.1 zip=3.0-11 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 ${PYTHON3_DEV} -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && ${PYTHON} get-pip.py \
 && rm get-pip.py
RUN ${PIP} --no-cache-dir install Pillow h5py ipykernel jupyter matplotlib mock numpy scipy sklearn pandas \
 && ${PYTHON} -m ipykernel.kernelspec
RUN if [ "${PYTHON}" = "python3" ] ; then ln -s -f /usr/bin/python3 /usr/bin/python ; fi
#   Set up our notebook config.
COPY jupyter_notebook_config.py /root/.jupyter/
#   Jupyter has issues with being run directly:
#     https://github.com/ipython/ipython/issues/7062
#   We just add a little wrapper script.
COPY run_jupyter.sh /
#   Set up Bazel.
#   Running bazel inside a `docker build` command causes trouble, cf:
#     https://github.com/bazelbuild/bazel/issues/134
#   The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /etc/bazel.bazelrc
#   Similarly, we need to workaround sandboxing issues:
#     https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /etc/bazel.bazelrc
#   Install the most recent bazel release.
ENV BAZEL_VERSION="0.14.1"
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
WORKDIR /tensorflow
#   Download and build TensorFlow.
#   Enable checking out both tags and branches
RUN export TAG_PREFIX="v" \
 && echo ${TF_BUILD_VERSION} | grep -q ^${TAG_PREFIX} ; if [ $? -eq 0 ] ; then git clone --depth=1 https://github.com/tensorflow/tensorflow.git . \
 && git fetch --tags \
 && git checkout ${TF_BUILD_VERSION} ; else git clone --depth=1 --branch=${TF_BUILD_VERSION} https://github.com/tensorflow/tensorflow.git . ; fi
RUN yes "" | ${PYTHON} configure.py
ENV CI_BUILD_PYTHON="${PYTHON}"
#   Set bazel build parameters in .bazelrc in parameterized_docker_build.sh
#   Use --copt=-march values to get optimized builds appropriate for the hardware
#     platform of your choice.
#   For ivy-bridge or sandy-bridge
#   --copt=-march="avx" \
#   For haswell, broadwell, or skylake
#   --copt=-march="avx2" \
COPY .bazelrc /root/.bazelrc
RUN tensorflow/tools/ci_build/builds/configured CPU bazel --bazelrc=/root/.bazelrc build -c opt tensorflow/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package "${WHL_DIR}" \
 && ${PIP} --no-cache-dir install --upgrade "${WHL_DIR}"/tensorflow-*.whl \
 && rm -rf /root/.cache
#   Clean up Bazel cache when done.
#   TensorBoard
EXPOSE 6006/tcp
#   IPython
EXPOSE 8888/tcp
WORKDIR /root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
