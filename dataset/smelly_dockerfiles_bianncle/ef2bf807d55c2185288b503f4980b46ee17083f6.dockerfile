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
FROM nvidia/cuda-ppc64le:10.0-base-ubuntu18.04 AS base_build
ARG TF_SERVING_VERSION_GIT_BRANCH=master
ARG TF_SERVING_VERSION_GIT_COMMIT=head
ARG TF_WHEEL_FILE=tensorflow_gpu-1.13.1-cp27-cp27mu-linux_ppc64le.whl
ARG TF_WHEEL_URL=https://powerci.osuosl.org/job/TensorFlow_PPC64LE_GPU_Release_Build/lastSuccessfulBuild/artifact/tensorflow_pkg
LABEL maintainer="wdirons@us.ibm.com"
LABEL tensorflow_serving_github_branchtag="${TF_SERVING_VERSION_GIT_BRANCH}"
LABEL tensorflow_serving_github_commit="${TF_SERVING_VERSION_GIT_COMMIT}"
ENV CUDNN_VERSION="7.4.1.5"
RUN apt-get update \
 && apt-get install --no-install-recommends automake build-essential ca-certificates cuda-command-line-tools-10-0 cuda-cublas-dev-10-0 cuda-cudart-dev-10-0 cuda-cufft-dev-10-0 cuda-curand-dev-10-0 cuda-cusolver-dev-10-0 cuda-cusparse-dev-10-0 curl git libfreetype6-dev libhdf5-dev libpng-dev libtool libcurl3-dev libzmq3-dev mlocate openjdk-8-jdk openjdk-8-jre-headless pkg-config python-dev software-properties-common swig unzip wget zip zlib1g-dev libcudnn7=${CUDNN_VERSION}-1+cuda10.0 libcudnn7-dev=${CUDNN_VERSION}-1+cuda10.0 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install grpcio h5py keras_applications keras_preprocessing mock numpy requests --no-cache-dir
RUN wget --no-verbose ${TF_WHEEL_URL}/${TF_WHEEL_FILE} \
 && pip install ${TF_WHEEL_FILE} \
 && rm -f ${TF_WHEEL_FILE}
#  Set up Bazel
ENV BAZEL_VERSION="0.20.0"
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && wget --no-verbose https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-dist.zip \
 && unzip bazel-$BAZEL_VERSION-dist.zip \
 && bash ./compile.sh \
 && cp output/bazel /usr/local/bin \
 && cd / \
 && rm -rf /bazel
#  Build TensorFlow with the CUDA configuration
ENV CI_BUILD_PYTHON="python"
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV TF_NEED_CUDA="1"
ENV TF_NEED_TENSORRT="0"
ENV TF_CUDA_COMPUTE_CAPABILITIES="3.5,3.7,5.2,6.0,7.0"
ENV TF_CUDA_VERSION="10.0"
ENV TF_CUDNN_VERSION="7"
#  Fix paths so that CUDNN can be found: https://github.com/tensorflow/tensorflow/issues/8264
WORKDIR /
RUN mkdir /usr/lib/powerpc64le-linux-gnu/include/ \
 && ln -s /usr/include/cudnn.h /usr/local/cuda/include/cudnn.h \
 && ln -s /usr/lib/powerpc64le-linux-gnu/libcudnn.so /usr/local/cuda/lib64/libcudnn.so \
 && ln -s /usr/lib/powerpc64le-linux-gnu/libcudnn.so.${TF_CUDNN_VERSION} /usr/local/cuda/lib64/libcudnn.so.${TF_CUDNN_VERSION}
#  For backward compatibility we need this line. After 1.13 we can safely remove
#  it.
ENV TF_NCCL_VERSION=""
#  Set TMP for nvidia build environment
ENV TMP="/tmp"
#  Download TF Serving sources (optionally at specific commit).
WORKDIR /tensorflow-serving
RUN git clone --branch=${TF_SERVING_VERSION_GIT_BRANCH} https://github.com/tensorflow/serving . \
 && git remote add upstream https://github.com/tensorflow/serving.git \
 && if [ "${TF_SERVING_VERSION_GIT_COMMIT}" != "head" ] ; then git checkout ${TF_SERVING_VERSION_GIT_COMMIT} ; fi
FROM base_build AS binary_build
#  Build, and install TensorFlow Serving
ARG TF_SERVING_BUILD_OPTIONS="--copt=-mcpu=power8 --copt=-mtune=power8"
RUN echo "Building with build options: ${TF_SERVING_BUILD_OPTIONS}"
ARG TF_SERVING_BAZEL_OPTIONS=""
RUN echo "Building with Bazel options: ${TF_SERVING_BAZEL_OPTIONS}"
RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 \
 && LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} bazel build --color=yes --curses=yes --config=cuda --copt="-fPIC" ${TF_SERVING_BAZEL_OPTIONS} --verbose_failures --output_filter=DONT_MATCH_ANYTHING ${TF_SERVING_BUILD_OPTIONS} tensorflow_serving/model_servers:tensorflow_model_server \
 && cp bazel-bin/tensorflow_serving/model_servers/tensorflow_model_server /usr/local/bin/ \
 && rm /usr/local/cuda/lib64/stubs/libcuda.so.1
#  Build and install TensorFlow Serving API
RUN bazel build --color=yes --curses=yes ${TF_SERVING_BAZEL_OPTIONS} --verbose_failures --output_filter=DONT_MATCH_ANYTHING ${TF_SERVING_BUILD_OPTIONS} tensorflow_serving/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow_serving/tools/pip_package/build_pip_package /tmp/pip \
 && pip install /tmp/pip/tensorflow_serving_api_gpu-*.whl --no-cache-dir --upgrade \
 && rm -rf /tmp/pip
FROM binary_build AS clean_build
#  Clean up Bazel cache when done.
RUN bazel clean --expunge --color=yes \
 && rm -rf /root/.cache
CMD ["/bin/bash"]
