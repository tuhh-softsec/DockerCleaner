#  Build target: lablup/common-tensorflow:1.7-py36
FROM ubuntu:18.04
LABEL maintainer="Mario Cho <m.cho@lablup.com>"
#  The TensorFlow version
ENV TF_VERSION="r1.7"
ENV BAZEL_VERSION="0.11.0"
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
ENV TF_NEED_S3="1 "
ENV TF_NEED_KAFKA="0 "
ENV TF_NEED_OPENCL="0"
ENV TF_NEED_OPENCL_SYCL="0"
ENV TF_NEED_CUDA="0"
ENV TF_CUDA_CLANG="0"
ENV TF_DOWNLOAD_CLANG="0"
ENV TF_NEED_TENSORRT="0"
ENV TF_SET_ANDROID_WORKSPACE="0"
#  Install system package dependencies
#  NOTE: running bazel requires JDK, not JRE!
RUN apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk-headless -y \
 && apt-get install --no-install-recommends gcc g++ make libssl-dev libfreetype6-dev libhdf5-serial-dev libzmq3-dev pkg-config rsync software-properties-common zip unzip zlib1g-dev wget imagemagick graphviz cmake curl wget rsync sed swig git-core libcurl3-dev zip -y \
 && rm -rf /var/lib/apt/lists/*
# install Python 3
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
#  Prepare for building TensorFlwo wheel
RUN python -m pip install -U setuptools pip \
 && python -m pip install --no-cache-dir wheel numpy scipy sklearn pandas keras_applications keras_preprocessing matplotlib \
 && rm -f /tmp/*.whl
RUN mkdir /bazel \
 && cd /bazel \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#  Check Bazel/Python is runnable.
RUN bazel version ; python -c "import sys; print(sys.prefix); print(sys.version_info)"
#  Also install TensorFlow build dependencies (ensure we have proper numpy)
RUN pip install wheel nUmpy Scipy --no-cache-dir \
 && rm -f /tmp/*.whl
#  NOTE: python should be linked to python3
RUN : build TensorFlow pip package \
 && cd /tmp \
 && git clone --branch=${TF_VERSION} --depth=1 https://github.com/tensorflow/tensorflow.git tensorflow-${TF_VERSION} \
 && ldconfig
RUN cd /tmp/tensorflow-${TF_VERSION} \
 && CC_OPT_FLAGS="-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2" tensorflow/tools/ci_build/builds/configured CPU bazel build --config=opt --config=mkl --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" //tensorflow/tools/pip_package:build_pip_package \
 && ./bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg
RUN ls -l /tmp/tensorflow_pkg
#  vim: ft=dockerfile sts=4 sw=4 et tw=0
