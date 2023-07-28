FROM ubuntu:16.04
MAINTAINER Mikkel Vilstrup <mikkel@vilstrup.dk>
#  #############################################################################
#   Install TensorFlow dependencies
#  #############################################################################
RUN apt-get update \
 && apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 ca-certificates=20210119~16.04.1 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libpng12-dev=1.2.54-1ubuntu1.1 l ibhdf5-dev openmpi-bin=1.10.2-8ubuntu1 pkg-config=0.29.1-0ubuntu1 zip=3.0-11 g++=4:5.3.1-1ubuntu1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 unzip=6.0-20ubuntu1.1 build-essential=12.1ubuntu2 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 software-properties-common=0.96.20.10 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 apt-get clean -y \
 && rm -rf /var/lib/apt/lists/*
#  #############################################################################
#   Install Anaconda
#  #############################################################################
RUN apt-get update \
 && apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 -y \
 && rm -rf /var/lib/apt/lists/*
RUN wget --quiet https://repo.continuum.io/archive/Anaconda3-4.4.0-Linux-x86_64.sh \
 && /bin/bash Anaconda3-4.4.0-Linux-x86_64.sh -b -p /opt/conda \
 && rm Anaconda3-4.4.0-Linux-x86_64.sh
ENV PATH="/opt/conda/bin:$PATH"
RUN pip install pip==23.1 --upgrade
#  #############################################################################
#   Install TensorFlow w/ CPU instructions
#  #############################################################################
RUN echo "startup --batch" >> /etc/bazel.bazelrc
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /etc/bazel.bazelrc
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
RUN git clone https://github.com/tensorflow/tensorflow.git \
 && cd tensorflow \
 && git checkout r1.2
WORKDIR /tensorflow
#   Must set bazel commands:
#   https://stackoverflow.com/questions/41293077/how-to-compile-tensorflow-with-sse4-2-and-avx-instructions
RUN tensorflow/tools/ci_build/builds/configured CPU bazel build -c opt --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" --copt=-mavx --copt=-mavx2 --copt=-mfma --copt=-mfpmath=both --copt=-msse4.1 --copt=-msse4.2 tensorflow/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/pip \
 && pip install /tmp/pip/tensorflow-*.whl --no-cache-dir --upgrade \
 && rm -rf /tmp/pip \
 && rm -rf /root/.cache
#  #############################################################################
#   Setup Language to UTF-8 for text
#  #############################################################################
#   https://askubuntu.com/a/601498
RUN apt-get clean \
 && apt-get update -y \
 && apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 -y \
 && locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
#  #############################################################################
#   Setup Jupyter
#  #############################################################################
COPY config.py /root/.jupyter/jupyter_notebook_config.py
#   Copy sample notebooks.
COPY notebooks /notebooks
#   Jupyter has issues with being run directly:
#     https://github.com/ipython/ipython/issues/7062
#   We just add a little wrapper script.
COPY run_jupyter.sh /
#   TensorBoard
EXPOSE 6006/tcp
#   IPython
EXPOSE 8888/tcp
#   Tell docker where to go automatically
WORKDIR "/notebooks"
CMD ["/run_jupyter.sh", "--allow-root"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
