#   Build target: lablup/common-tensorflow:1.4-py36
FROM ubuntu:16.04
LABEL maintainer="Mario Cho <m.cho@lablup.com>"
#   The TensorFlow version
ENV TF_VERSION="1.4"
ENV BAZEL_VERSION="0.5.4"
ENV PYTHON_BIN_PATH="/usr/bin/python"
ENV PYTHON_LIB_PATH="/usr/local/lib/python3.6/site-packages "
ENV GCC_HOST_COMPILER_PATH="/usr/bin/gcc "
ENV CC_OPT_FLAGS="\"-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2\" "
ENV TF_NEED_JEMALLOC="1"
ENV TF_NEED_GCP="1"
ENV TF_NEED_HDFS="1"
ENV TF_ENABLE_XLA="1"
ENV TF_NEED_VERBS="0"
ENV TF_NEED_MPI="0"
ENV TF_NEED_GDR="0"
ENV TF_NEED_S3="1"
ENV TF_NEED_KAFKA="0"
ENV GCC_HOST_COMPILER_PATH="/usr/bin/gcc"
#   Install system package dependencies
#   NOTE: running bazel requires JDK, not JRE!
RUN apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk-headless=8u292-b10-0ubuntu1~16.04.1 -y \
 && apt-get install --no-install-recommends gcc=4:5.3.1-1ubuntu1 g++=4:5.3.1-1ubuntu1 make=4.1-6 libssl-dev=1.0.2g-1ubuntu4.20 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libpng12-dev=1.2.54-1ubuntu1.1 libzmq3-dev=4.1.4-7ubuntu0.1 pkg-config=0.29.1-0ubuntu1 rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 zip=3.0-11 unzip=6.0-20ubuntu1.1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 wget=1.17.1-1ubuntu1.5 imagemagick=8:6.8.9.9-7ubuntu5.16 graphviz=2.38.0-12ubuntu2.1 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 rsync=3.1.1-3ubuntu1.3 sed=4.2.2-7 swig=3.0.8-0ubuntu3 git-core=1:2.7.4-0ubuntu1.10 libcurl3-dev zip=3.0-11 -y \
 && rm -rf /var/lib/apt/lists/*
#  install Python 3
RUN curl https://www.python.org/ftp/python/3.6.8/Python-3.6.8.tar.xz -o /opt/python.tar.xz \
 && cd /opt \
 && tar xvf python.tar.xz \
 && cd /opt/*/ \
 && ./configure \
 && make \
 && make install \
 && ln -s -f /usr/local/bin/python3.6 /usr/bin/python
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN mkdir /bazel \
 && cd /bazel \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#   Check Bazel/Python is runnable.
RUN bazel version ; python -c "import sys; print(sys.prefix); print(sys.version_info)"
#   Also install TensorFlow build dependencies (ensure we have proper numpy)
RUN pip install wheel==0.40.0 numpy==1.24.2 scipy==1.10.1 --no-cache-dir \
 && pip install keras_applications==1.0.8 --no-cache-dir \
 && pip install keras_preprocessing==1.1.2 --no-cache-dir \
 && rm -f /tmp/*.whl
#   NOTE: python should be linked to python3
RUN : build TensorFlow pip package \
 && cd /tmp \
 && curl -SL https://github.com/tensorflow/tensorflow/archive/r${TF_VERSION}.tar.gz | tar xzf - \
 && ldconfig
RUN cd /tmp/tensorflow-r${TF_VERSION} \
 && CC_OPT_FLAGS="-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2" tensorflow/tools/ci_build/builds/configured CPU bazel build --config=opt --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" //tensorflow/tools/pip_package:build_pip_package \
 && ./bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg
RUN ls -l /tmp/tensorflow_pkg
#   vim: ft=dockerfile sts=4 sw=4 et tw=0
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
