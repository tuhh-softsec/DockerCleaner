FROM ubuntu:18.04
LABEL maintainer="Cong Xu <cong.xu@intel.com>"
#   These parameters can be overridden by parameterized_docker_build.sh
ARG TF_BUILD_VERSION=r1.13
ARG PYTHON="python"
ARG PYTHON3_DEV=""
ARG WHL_DIR="/tmp/pip"
ARG PIP="pip"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 curl=7.58.0-2ubuntu3.24 git=1:2.17.1-1ubuntu0.17 libcurl3-dev libfreetype6-dev=2.8.1-2ubuntu2.2 libhdf5-serial-dev=1.10.0-patch1+docs-4 libnuma-dev=2.0.11-2.1ubuntu0.1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libzmq3-dev=4.2.5-1ubuntu0.2 openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 openjdk-8-jre-headless=8u362-ga-0ubuntu1~18.04.1 openssh-client=1:7.6p1-4ubuntu0.7 openssh-server=1:7.6p1-4ubuntu0.7 pkg-config=0.29.1-0ubuntu2 rsync=3.1.2-2.1ubuntu1.6 software-properties-common=0.96.24.32.20 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 zip=3.0-11build1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 ${PYTHON} ${PYTHON}-dev ${PYTHON}-pip ${PYTHON}-setuptools ${PYTHON}-wheel -y --fix-missing \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN ${PIP} --no-cache-dir install Pillow h5py ipykernel jupyter keras_applications keras_preprocessing matplotlib mock numpy scipy sklearn pandas \
 && ${PYTHON} -m ipykernel.kernelspec
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
ENV BAZEL_VERSION="0.20.0"
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
RUN cp .bazelrc /root/.bazelrc
ENV CI_BUILD_PYTHON="${PYTHON}"
#   Set bazel build parameters in .bazelrc in parameterized_docker_build.sh
#   Use --copt=-march values to get optimized builds appropriate for the hardware
#     platform of your choice.
#   For ivy-bridge or sandy-bridge
#   --copt=-march="avx" \
#   For haswell, broadwell, or skylake
#   --copt=-march="avx2" \
COPY .bazelrc /root/.mkl.bazelrc
RUN echo "import /root/.mkl.bazelrc" >> /root/.bazelrc
RUN tensorflow/tools/ci_build/builds/configured CPU bazel --bazelrc=/root/.bazelrc build -c opt tensorflow/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package "${WHL_DIR}" \
 && ${PIP} --no-cache-dir install --upgrade "${WHL_DIR}"/tensorflow-*.whl \
 && rm -rf /root/.cache
#   Clean up Bazel cache when done.
WORKDIR /root
#   Install Open MPI
RUN mkdir /tmp/openmpi \
 && cd /tmp/openmpi \
 && wget https://www.open-mpi.org/software/ompi/v3.0/downloads/openmpi-3.0.0.tar.gz \
 && tar zxf openmpi-3.0.0.tar.gz \
 && cd openmpi-3.0.0 \
 && ./configure --enable-orterun-prefix-by-default \
 && make -j $( nproc ;) all \
 && make install \
 && ldconfig \
 && rm -rf /tmp/openmpi
#   Create a wrapper for OpenMPI to allow running as root by default
RUN mv /usr/local/bin/mpirun /usr/local/bin/mpirun.real \
 && echo '#!/bin/bash' > /usr/local/bin/mpirun \
 && echo 'mpirun.real --allow-run-as-root "$@"' >> /usr/local/bin/mpirun \
 && chmod a+x /usr/local/bin/mpirun
#   Configure OpenMPI to run good defaults:
RUN echo "btl_tcp_if_exclude = lo,docker0" >> /usr/local/etc/openmpi-mca-params.conf
#   Install Horovod
RUN ${PIP} install --no-cache-dir horovod
#   Install OpenSSH for MPI to communicate between containers
RUN mkdir -p /var/run/sshd
#   Allow OpenSSH to talk to containers without asking for confirmation
RUN cat /etc/ssh/ssh_config | grep -v StrictHostKeyChecking > /etc/ssh/ssh_config.new \
 && echo " StrictHostKeyChecking no" >> /etc/ssh/ssh_config.new \
 && mv /etc/ssh/ssh_config.new /etc/ssh/ssh_config
#   TensorBoard
EXPOSE 6006/tcp
#   IPython
EXPOSE 8888/tcp
WORKDIR /root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
