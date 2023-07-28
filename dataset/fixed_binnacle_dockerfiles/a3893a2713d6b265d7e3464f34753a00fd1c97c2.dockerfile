FROM ubuntu:16.04
LABEL maintainer="Craig Citro <craigcitro@google.com>"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 libcurl3-dev libfreetype6-dev=2.6.1-0.1ubuntu2.5 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libpng12-dev=1.2.54-1ubuntu1.1 libzmq3-dev=4.1.4-7ubuntu0.1 pkg-config=0.29.1-0ubuntu1 python-dev=2.7.12-1~16.04 rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 unzip=6.0-20ubuntu1.1 zip=3.0-11 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install Pillow==9.5.0 h5py==3.8.0 ipykernel==6.22.0 jupyter==1.0.0 keras_applications==1.0.8 keras_preprocessing==1.1.2 matplotlib==3.7.1 mock==5.0.2 numpy==1.24.2 scipy==1.10.1 sklearn==0.0.post4 pandas==2.0.0 --no-cache-dir \
 && python -m ipykernel.kernelspec
#   RUN ln -s -f /usr/bin/python3 /usr/bin/python#
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
ENV BAZEL_VERSION="0.15.0"
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
RUN git clone --branch=r1.12 --depth=1 https://github.com/tensorflow/tensorflow.git .
#   TODO(craigcitro): Don't install the pip package, since it makes it
#   more difficult to experiment with local changes. Instead, just add
#   the built directory to the path.
ENV CI_BUILD_PYTHON="python"
RUN tensorflow/tools/ci_build/builds/configured CPU bazel build -c opt --copt=-mavx --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" tensorflow/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/pip \
 && pip install /tmp/pip/tensorflow-*.whl --no-cache-dir --upgrade \
 && rm -rf /tmp/pip \
 && rm -rf /root/.cache
#   Clean up pip wheel and Bazel cache when done.
#   TensorBoard
EXPOSE 6006/tcp
#   IPython
EXPOSE 8888/tcp
WORKDIR /root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
