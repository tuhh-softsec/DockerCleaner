FROM nvidia/cuda:9.0-base-ubuntu16.04
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential cuda-command-line-tools-9-0 cuda-cublas-dev-9-0 cuda-cudart-dev-9-0 cuda-cufft-dev-9-0 cuda-curand-dev-9-0 cuda-cusolver-dev-9-0 cuda-cusparse-dev-9-0 curl git libcudnn7=7.0.5.15-1+cuda9.0 libcudnn7-dev=7.0.5.15-1+cuda9.0 libcurl3-dev libfreetype6-dev libpng12-dev libzmq3-dev pkg-config python-dev rsync software-properties-common unzip zip zlib1g-dev wget -y \
 && rm -rf /var/lib/apt/lists/* \
 && find /usr/local/cuda-9.0/lib64/ -type f -name 'lib*_static.a' -not -name 'libcudart_static.a' -delete \
 && rm /usr/lib/x86_64-linux-gnu/libcudnn_static_v7.a
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install numpy==1.24.2 scipy==1.10.1 sklearn==0.0.post4 pandas==2.0.0 Pillow==9.5.0 h5py==3.8.0 --no-cache-dir
#   Set up grpc
RUN pip install enum34==1.1.10 futures==3.4.0 mock==5.0.2 six==1.16.0 \
 && pip install 'protobuf>=3.0.0a3' --pre \
 && pip install grpcio==1.53.0 -i https://testpypi.python.org/simple --pre
#   Set up Bazel.
#   Running bazel inside a `docker build` command causes trouble, cf:
#     https://github.com/bazelbuild/bazel/issues/134
#   The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /etc/bazel.bazelrc
#   Similarly, we need to workaround sandboxing issues:
#     https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /etc/bazel.bazelrc
#   Install the most recent bazel release.
ENV BAZEL_VERSION="0.8.0"
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
WORKDIR /
RUN git clone https://github.com/tensorflow/tensorflow.git \
 && cd tensorflow \
 && git checkout v1.5.0
#   Configure the build for our CUDA configuration.
ENV CI_BUILD_PYTHON="python"
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV TF_NEED_CUDA="1"
ENV TF_CUDA_COMPUTE_CAPABILITIES="3.7,6.1"
ENV TF_CUDA_VERSION="9.0"
ENV TF_CUDNN_VERSION="7"
ENV CUDNN_INSTALL_PATH="/usr/lib/x86_64-linux-gnu"
WORKDIR /tensorflow
#   Copy patches and apply patches
COPY patches /patches
RUN git apply --verbose /patches/*.patch
RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 \
 && LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} tensorflow/tools/ci_build/builds/configured GPU bazel build -c opt --config=cuda --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" tensorflow/tools/pip_package:build_pip_package \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/pip \
 && pip install /tmp/pip/tensorflow-*.whl --no-cache-dir --upgrade \
 && rm -rf /tmp/pip \
 && rm -rf /root/.cache
#   Clean up pip wheel and Bazel cache when done.
#   Download TensorFlow Serving
RUN cd / \
 && git clone --recurse-submodules https://github.com/tensorflow/serving \
 && cd serving \
 && git checkout 1.5.0
#   Configure Tensorflow to use the GPU
WORKDIR /serving
RUN git clone --recursive https://github.com/tensorflow/tensorflow.git \
 && cd tensorflow \
 && git checkout v1.5.0 \
 && tensorflow/tools/ci_build/builds/configured GPU
#   Build TensorFlow Serving and Install it in /usr/local/bin
WORKDIR /serving
RUN bazel build -c opt --config=cuda --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" --crosstool_top=@local_config_cuda//crosstool:toolchain tensorflow_serving/model_servers:tensorflow_model_server \
 && cp bazel-bin/tensorflow_serving/model_servers/tensorflow_model_server /usr/local/bin/ \
 && bazel clean --expunge
WORKDIR /root
#   cleaning up the container
RUN rm -rf /tensorflow \
 && rm -rf /serving \
 && rm -rf /tmp/tensorflow_pkg/tensorflow-1.4.0-cp27-cp27mu-linux_x86_64.whl \
 && rm -rf /bazel
RUN ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
