#  Copyright 2018 Google LLC
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      https://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
FROM pai.build.base:hadoop2.7.2-cuda9.0-cudnn7-devel-ubuntu16.04 AS base_build
ARG TF_SERVING_VERSION_GIT_BRANCH=master
ARG TF_SERVING_VERSION_GIT_COMMIT=head
LABEL maintainer="gvasudevan@google.com"
LABEL tensorflow_serving_github_branchtag="${TF_SERVING_VERSION_GIT_BRANCH}"
LABEL tensorflow_serving_github_commit="${TF_SERVING_VERSION_GIT_COMMIT}"
ENV NCCL_VERSION="2.2.13"
ENV CUDNN_VERSION="7.1.4.18"
RUN apt-get update \
 && apt-get install --no-install-recommends automake build-essential cuda-command-line-tools-9-0 cuda-cublas-dev-9-0 cuda-cudart-dev-9-0 cuda-cufft-dev-9-0 cuda-curand-dev-9-0 cuda-cusolver-dev-9-0 cuda-cusparse-dev-9-0 curl git libfreetype6-dev libpng12-dev libtool libcurl3-dev libzmq3-dev mlocate openjdk-8-jdk openjdk-8-jre-headless pkg-config python-dev software-properties-common swig unzip wget zip zlib1g-dev libcudnn7=${CUDNN_VERSION}-1+cuda9.0 libcudnn7-dev=${CUDNN_VERSION}-1+cuda9.0 libnccl2=${NCCL_VERSION}-1+cuda9.0 libnccl-dev=${NCCL_VERSION}-1+cuda9.0 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && find /usr/local/cuda-9.0/lib64/ -type f -name 'lib*_static.a' -not -name 'libcudart_static.a' -delete \
 && rm /usr/lib/x86_64-linux-gnu/libcudnn_static_v7.a
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install grpcio h5py keras_applications==1.0.4 keras_preprocessing==1.0.2 mock numpy==1.14.5 --no-cache-dir
#  Set up Bazel
#  Need >= 0.15.0 so bazel compiles work with docker bind mounts.
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
#  Build TensorFlow with the CUDA configuration
ENV CI_BUILD_PYTHON="python"
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV TF_NEED_CUDA="1"
ENV TF_CUDA_COMPUTE_CAPABILITIES="3.0,3.5,5.2,6.0,6.1,7.0"
ENV TF_CUDA_VERSION="9.0"
ENV TF_CUDNN_VERSION="7"
#  Fix paths so that CUDNN can be found: https://github.com/tensorflow/tensorflow/issues/8264
WORKDIR /
RUN mkdir /usr/lib/x86_64-linux-gnu/include/ \
 && ln -s /usr/lib/x86_64-linux-gnu/include/cudnn.h /usr/lib/x86_64-linux-gnu/include/cudnn.h \
 && ln -s /usr/include/cudnn.h /usr/local/cuda/include/cudnn.h \
 && ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so /usr/local/cuda/lib64/libcudnn.so \
 && ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.${TF_CUDNN_VERSION} /usr/local/cuda/lib64/libcudnn.so.${TF_CUDNN_VERSION}
#  NCCL 2.x
ENV TF_NCCL_VERSION="2"
ENV NCCL_INSTALL_PATH="/usr/lib/nccl/"
#  Fix paths so that NCCL can be found
WORKDIR /
RUN mkdir -p ${NCCL_INSTALL_PATH} \
 && mkdir ${NCCL_INSTALL_PATH}include/ \
 && mkdir ${NCCL_INSTALL_PATH}lib/ \
 && ln -s /usr/include/nccl.h ${NCCL_INSTALL_PATH}include/nccl.h \
 && ln -s /usr/lib/x86_64-linux-gnu/libnccl.so ${NCCL_INSTALL_PATH}lib/libnccl.so \
 && ln -s /usr/lib/x86_64-linux-gnu/libnccl.so.${TF_NCCL_VERSION} ${NCCL_INSTALL_PATH}lib/libnccl.so.${TF_NCCL_VERSION}
#  Set TMP for nvidia build environment
ENV TMP="/tmp"
#  Download TF Serving sources (optionally at specific commit).
WORKDIR /tensorflow-serving
RUN git clone --branch=${TF_SERVING_VERSION_GIT_BRANCH} https://github.com/tensorflow/serving . \
 && git remote add upstream https://github.com/tensorflow/serving.git \
 && if [ "${TF_SERVING_VERSION_GIT_COMMIT}" != "head" ] ; then git checkout ${TF_SERVING_VERSION_GIT_COMMIT} ; fi
FROM base_build AS binary_build
#  Build, and install TensorFlow Serving
ARG TF_SERVING_BUILD_OPTIONS="--copt=-mavx --cxxopt=-D_GLIBCXX_USE_CXX11_ABI=0"
ARG TF_SERVING_BAZEL_OPTIONS=""
RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 \
 && LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} bazel build -c opt --color=yes --curses=yes --config=cuda ${TF_SERVING_BAZEL_OPTIONS} --verbose_failures --output_filter=DONT_MATCH_ANYTHING ${TF_SERVING_BUILD_OPTIONS} tensorflow_serving/model_servers:tensorflow_model_server \
 && cp bazel-bin/tensorflow_serving/model_servers/tensorflow_model_server /usr/local/bin/ \
 && rm /usr/local/cuda/lib64/stubs/libcuda.so.1 \
 && bazel build -c opt tensorflow_serving/example:mnist_saved_model
CMD ["/bin/bash"]
