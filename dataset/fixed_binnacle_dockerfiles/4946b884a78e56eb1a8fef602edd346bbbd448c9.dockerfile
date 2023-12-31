FROM nvidia/cuda:9.0-base-ubuntu16.04
LABEL maintainer="Craig Citro <craigcitro@google.com>"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential cuda-command-line-tools-9-0 cuda-cublas-dev-9-0 cuda-cudart-dev-9-0 cuda-cufft-dev-9-0 cuda-curand-dev-9-0 cuda-cusolver-dev-9-0 cuda-cusparse-dev-9-0 curl git libcudnn7=7.1.4.18-1+cuda9.0 libcudnn7-dev=7.1.4.18-1+cuda9.0 libnccl2=2.2.13-1+cuda9.0 libnccl-dev=2.2.13-1+cuda9.0 libcurl3-dev libfreetype6-dev libhdf5-serial-dev libpng12-dev libzmq3-dev pkg-config python-dev rsync software-properties-common unzip zip zlib1g-dev wget -y \
 && rm -rf /var/lib/apt/lists/* \
 && find /usr/local/cuda-9.0/lib64/ -type f -name 'lib*_static.a' -not -name 'libcudart_static.a' -delete \
 && rm /usr/lib/x86_64-linux-gnu/libcudnn_static_v7.a
#   Link NCCL libray and header where the build script expects them.
RUN mkdir /usr/local/cuda-9.0/lib \
 && ln -s /usr/lib/x86_64-linux-gnu/libnccl.so.2 /usr/local/cuda/lib/libnccl.so.2 \
 && ln -s /usr/include/nccl.h /usr/local/cuda/include/nccl.h
#   TODO(tobyboyd): Remove after license is excluded from BUILD file.
RUN gunzip /usr/share/doc/libnccl2/NCCL-SLA.txt.gz \
 && cp /usr/share/doc/libnccl2/NCCL-SLA.txt /usr/local/cuda/
RUN curl -fSsL -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install Pillow==9.5.0 h5py==3.8.0 ipykernel==6.22.0 jupyter==1.0.0 matplotlib==3.7.1 mock==5.0.2 numpy==1.14.5 scipy==1.10.1 sklearn==0.0.post4 pandas==2.0.0 --no-cache-dir \
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
RUN git clone --branch=r1.10 --depth=1 https://github.com/tensorflow/tensorflow.git .
#   Configure the build for our CUDA configuration.
ENV CI_BUILD_PYTHON="python"
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV TF_NEED_CUDA="1"
ENV TF_CUDA_COMPUTE_CAPABILITIES="3.5,5.2,6.0,6.1,7.0"
ENV TF_CUDA_VERSION="9.0"
ENV TF_CUDNN_VERSION="7"
#   NCCL 2.x
ENV TF_NCCL_VERSION="2"
RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/local/cuda/lib64/stubs/libcuda.so.1 \
 && LD_LIBRARY_PATH=/usr/local/cuda/lib64/stubs:${LD_LIBRARY_PATH} tensorflow/tools/ci_build/builds/configured GPU bazel build -c opt --copt=-mavx --config=cuda --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" tensorflow/tools/pip_package:build_pip_package \
 && rm /usr/local/cuda/lib64/stubs/libcuda.so.1 \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/pip \
 && pip install /tmp/pip/tensorflow-*.whl --no-cache-dir --upgrade \
 && rm -rf /tmp/pip \
 && rm -rf /root/.cache
#   Clean up pip wheel and Bazel cache when done.
WORKDIR /root
#   TensorBoard
EXPOSE 6006/tcp
#   IPython
EXPOSE 8888/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
