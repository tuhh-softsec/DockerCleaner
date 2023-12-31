FROM lablup/kernel-base:python3.6 AS python-binary
#   Build target: lablup/common-tensorflow:1.0-py36-cuda8
FROM nvidia/cuda:8.0-cudnn6-devel-ubuntu16.04
LABEL maintainer="Mario Cho <m.cho@lablup.com>"
#   The TensorFlow version
ENV TF_VERSION="1.0"
ENV PYTHON_BIN_PATH="/usr/local/bin/python"
ENV PYTHON_LIB_PATH="/usr/local/lib/python3.6/site-packages "
ENV GCC_HOST_COMPILER_PATH="/usr/bin/gcc "
ENV CC_OPT_FLAGS="\"-march=x86-64 -mavx -mavx2 -mfma -mfpmath=both -msse4.1 -msse4.2\" "
ENV TF_NEED_JEMALLOC="1"
ENV TF_NEED_GCP="1"
ENV TF_NEED_HDFS="1"
ENV TF_ENABLE_XLA="1"
ENV TF_NEED_VERBS="0"
ENV TF_NEED_CUDA="1"
ENV TF_CUDA_CLANG="0"
ENV TF_NEED_S3="1"
ENV TF_CUDA_VERSION="8.0 "
ENV TF_CUDNN_VERSION="6"
ENV TF_NCCL_VERSION="2"
ENV TF_CUDA_COMPUTE_CAPABILITIES="3.5,3.7,5.2,6.0,6.1"
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV CUDA_TOOLKIT_PATH="/usr/local/cuda"
ENV CUDNN_INSTALL_PATH="/usr/local/cuda "
ENV NCCL_INSTALL_PATH="/usr/local/cuda/lib"
#   Install system package dependencies
#   NOTE: running bazel requires JDK, not JRE!
RUN apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk-headless -y \
 && apt-get install --no-install-recommends libfreetype6-dev libhdf5-serial-dev libpng12-dev libzmq3-dev libcurl3-dev pkg-config rsync software-properties-common zip unzip zlib1g-dev wget imagemagick graphviz cmake gcc g++ curl wget rsync sed swig git-core zip -y \
 && rm -rf /var/lib/apt/lists/*
#   Link NCCL libray and header where the build script expects them.
RUN mkdir /usr/local/cuda-8.0/lib \
 && ln -s /usr/lib/x86_64-linux-gnu/libnccl.so.2 /usr/local/cuda/lib/libnccl.so.2 \
 && ln -s /usr/include/nccl.h /usr/local/cuda/include/nccl.h
#   link CUDA-8.0 + cuDNN 6.0
RUN ln -s /usr/local/cuda-8.0 /usr/local/cuda \
 && ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.6.0.21 /usr/local/cuda/lib64/libcudnn.so.6 \
 && ln -s /usr/include/x86_64-linux-gnu/cudnn_v6.h /usr/local/cuda/include/cudnn.h \
 && ldconfig
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
COPY --from=python-binary /python.tar.gz /
RUN cd / ; tar xzpf python.tar.gz ; rm python.tar.gz ; ldconfig
ENV BAZEL_VERSION="0.4.5"
RUN mkdir /bazel \
 && cd /bazel \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
#   Check Bazel/Python is runnable.
#   Also install TensorFlow build dependencies (ensure we have proper numpy)
RUN bazel version ; python -c "import sys; print(sys.prefix); print(sys.version_info)" \
 && pip install wheel==0.40.0 numpy==1.24.2 scipy==1.10.1 --no-cache-dir \
 && pip install keras_applications==1.0.8 --no-cache-dir \
 && pip install keras_preprocessing==1.1.2 --no-cache-dir \
 && rm -f /tmp/*.whl
#   NOTE: python should be linked to python3
RUN : build TensorFlow pip package \
 && cd /tmp \
 && curl -SL https://github.com/tensorflow/tensorflow/archive/r${TF_VERSION}.tar.gz | tar xzf - \
 && ldconfig
RUN cd /tmp/tensorflow-r${TF_VERSION} \
 && ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 \
 && LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} tensorflow/tools/ci_build/builds/configured GPU bazel build -c opt --config=cuda --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" tensorflow/tools/pip_package:build_pip_package \
 && rm /usr/local/cuda/lib64/stubs/libcuda.so.1 \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg
#   Clean up pip wheel and Bazel cache when done.
RUN rm -rf /root/.cache \
 && ls -lh /tmp/tensorflow_pkg
#   vim: ft=dockerfile sts=4 sw=4 et tw=0
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
