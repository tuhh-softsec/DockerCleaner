FROM ubuntu:18.04
#   base layer - python3.7
RUN apt-get update \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 ca-certificates=20211016ubuntu0.18.04.1 locales=2.27-3ubuntu1.6 git=1:2.17.1-1ubuntu0.17 python3.7=3.7.5-2ubuntu1~18.04.2 python3.7-dev=3.7.5-2ubuntu1~18.04.2 make=4.1-9.1ubuntu1 gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 --no-install-suggests -y \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && ln -s ./python3.7 /usr/bin/python3 \
 && apt-get download python3-distutils \
 && dpkg -x python3-distutils* / \
 && rm python3-distutils* \
 && wget -O - https://bootstrap.pypa.io/get-pip.py | python3 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   nvidia driver layer
ENV NVIDIA_DRIVER_VERSION="384.130"
RUN apt-get update \
 && apt-get install --no-install-recommends kmod=24-1ubuntu3.5 -y \
 && mkdir -p /opt/nvidia \
 && cd /opt/nvidia/ \
 && wget http://us.download.nvidia.com/XFree86/Linux-x86_64/${NVIDIA_DRIVER_VERSION}/NVIDIA-Linux-x86_64-${NVIDIA_DRIVER_VERSION}.run -O /opt/nvidia/driver.run \
 && chmod +x /opt/nvidia/driver.run \
 && /opt/nvidia/driver.run -a -s --no-nvidia-modprobe --no-kernel-module --no-unified-memory --no-x-check --no-opengl-files \
 && rm -rf /opt/nvidia \
 && apt-get remove -y kmod \
 && apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   cuda layer
ENV CUDA_VERSION="9.2.88"
ENV CUDA_VERSION_DASH="9-2"
ENV CUDA_VERSION_MAJOR="9.2"
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg=2.2.4-1ubuntu1.6 -y \
 && wget http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/cuda-repo-ubuntu1604_${CUDA_VERSION}-1_amd64.deb \
 && dpkg -i cuda-repo-ubuntu1604_${CUDA_VERSION}-1_amd64.deb \
 && rm cuda-repo-ubuntu1604_${CUDA_VERSION}-1_amd64.deb \
 && apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub \
 && apt-get update \
 && apt-get install --no-install-recommends cuda-cublas-${CUDA_VERSION_DASH} cuda-cudart-${CUDA_VERSION_DASH} cuda-cufft-${CUDA_VERSION_DASH} cuda-curand-${CUDA_VERSION_DASH} cuda-cusolver-${CUDA_VERSION_DASH} cuda-cusparse-${CUDA_VERSION_DASH} -y --no-install-suggests \
 && sed -i 's#"$#:/usr/local/cuda-${CUDA_VERSION_MAJOR}/bin"#' /etc/environment \
 && apt-get remove -y gnupg \
 && apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/cuda-${CUDA_VERSION_MAJOR}/bin"
#   cudnn layer
COPY libcudnn7_7.1.4.18-1+cuda9.2_amd64.deb /
RUN dpkg -i /libcudnn7_7.1.4.18-1+cuda9.2_amd64.deb \
 && rm libcudnn7_7.1.4.18-1+cuda9.2_amd64.deb
#   pip deps
RUN pip3 install --no-cache-dir numpy
#   build and install tensorflow
ENV BAZEL_VERSION="0.15.0"
COPY libcudnn7-dev_7.1.4.18-1+cuda9.2_amd64.deb /
COPY 0001-Port-to-Python-3.7.patch /
COPY 0002-Update-Cython.patch /
RUN apt-get update \
 && dpkg -i /libcudnn7-dev_7.1.4.18-1+cuda9.2_amd64.deb \
 && rm libcudnn7-dev_7.1.4.18-1+cuda9.2_amd64.deb \
 && apt-get install --no-install-recommends unzip=6.0-21ubuntu1.2 cuda-command-line-tools-${CUDA_VERSION_DASH} cuda-cublas-dev-${CUDA_VERSION_DASH} cuda-cudart-dev-${CUDA_VERSION_DASH} cuda-cufft-dev-${CUDA_VERSION_DASH} cuda-curand-dev-${CUDA_VERSION_DASH} cuda-cusolver-dev-${CUDA_VERSION_DASH} cuda-cusparse-dev-${CUDA_VERSION_DASH} -y --no-install-suggests \
 && wget https://github.com/bazelbuild/bazel/releases/download/${BAZEL_VERSION}/bazel-${BAZEL_VERSION}-installer-linux-x86_64.sh \
 && chmod +x ./bazel-*.sh \
 && ./bazel-${BAZEL_VERSION}-installer-linux-x86_64.sh \
 && rm ./bazel-${BAZEL_VERSION}-installer-linux-x86_64.sh \
 && git clone --single-branch --depth 1 --branch r1.9 https://github.com/tensorflow/tensorflow \
 && cd tensorflow \
 && git apply /0001-Port-to-Python-3.7.patch \
 && git apply /0002-Update-Cython.patch \
 && rm /*.patch \
 && echo 'export PYTHON_BIN_PATH="/usr/bin/python3"' > tools/python_bin_path.sh \
 && echo 'import /tensorflow/.tf_configure.bazelrc' > .bazelrc \
 && echo 'build --action_env PYTHON_BIN_PATH="/usr/bin/python3"\nbuild --action_env PYTHON_LIB_PATH="/usr/lib/python3/dist-packages"\nbuild --python_path="/usr/bin/python3"\nbuild --define with_jemalloc=true\nbuild:gcp --define with_gcp_support=false\nbuild:hdfs --define with_hdfs_support=false\nbuild:s3 --define with_s3_support=false\nbuild:kafka --define with_kafka_support=false\nbuild:xla --define with_xla_support=false\nbuild:gdr --define with_gdr_support=false\nbuild:verbs --define with_verbs_support=false\nbuild --action_env TF_NEED_OPENCL_SYCL="0"\nbuild --action_env TF_NEED_CUDA="1"\nbuild --action_env CUDA_TOOLKIT_PATH="/usr/local/cuda-9.2"\nbuild --action_env TF_CUDA_VERSION="9.2"\nbuild --action_env CUDNN_INSTALL_PATH="/usr/lib/x86_64-linux-gnu"\nbuild --action_env TF_CUDNN_VERSION="7"\nbuild --action_env TF_NCCL_VERSION="1"\nbuild --action_env TF_CUDA_COMPUTE_CAPABILITIES="3.7,6.1,7.0"\nbuild --action_env TF_CUDA_CLANG="0"\nbuild --action_env GCC_HOST_COMPILER_PATH="/usr/bin/gcc"\nbuild --config=cuda\ntest --config=cuda\nbuild --define grpc_no_ares=true\nbuild:opt --copt=-march=native\nbuild:opt --host_copt=-march=native\nbuild:opt --define with_default_optimizations=true\nbuild --strip=always' > .tf_configure.bazelrc \
 && cat .tf_configure.bazelrc \
 && ln -s python3 /usr/bin/python \
 && bazel build --config opt '@protobuf_archive//:src/google/protobuf/any.h' \
 && find bazel-tensorflow/external/protobuf_archive/python/google/protobuf/pyext -name '*.cc' -exec sed -i "s/PyUnicode_AsUTF8AndSize(/(char*)PyUnicode_AsUTF8AndSize(/g" {}
#   install FractalAI deps
ENV NPY_NUM_BUILD_JOBS="8"
RUN apt-get update \
 && apt-get install --no-install-recommends cmake=3.10.2-1ubuntu2.18.04.2 pkg-config=0.29.1-0ubuntu2 flex=2.6.4-6 bison=2:3.0.4.dfsg-1build1 curl=7.58.0-2ubuntu3.24 libpng16-16=1.6.34-1ubuntu0.18.04.2 libpng-dev=1.6.34-1ubuntu0.18.04.2 libjpeg-turbo8=1.5.2-0ubuntu5.18.04.6 libjpeg-turbo8-dev=1.5.2-0ubuntu5.18.04.6 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libhdf5-100=1.10.0-patch1+docs-4 libhdf5-dev=1.10.0-patch1+docs-4 libopenblas-base=0.2.20+ds-4 libopenblas-dev=0.2.20+ds-4 gfortran=4:7.4.0-1ubuntu2.3 libfreetype6=2.8.1-2ubuntu2.2 libfreetype6-dev=2.8.1-2ubuntu2.2 -y \
 && pip3 install --no-cache-dir cython \
 && pip3 install --no-cache-dir git+https://github.com/openai/gym git+https://github.com/ray-project/ray#subdirectory=python git+https://github.com/Guillem-db/atari-py networkx jupyter keras h5py Pillow-simd PyOpenGL matplotlib \
 && python3 -c "import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot" \
 && pip3 uninstall -y cython \
 && apt-get remove -y cmake cmake pkg-config flex bison curl libpng-dev libjpeg-turbo8-dev zlib1g-dev libhdf5-dev libopenblas-dev gfortran libfreetype6-dev \
 && apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   FractalAI
COPY . /fractalai
RUN rm /fractalai/*.patch /fractalai/*.deb \
 && pip3 install -e /fractalai \
 && apt-get remove -y gcc g++ make git \
 && apt-get autoremove -y
#  Jupyter notebook
RUN mkdir /root/.jupyter \
 && echo 'c.NotebookApp.token = "mallorca"' > /root/.jupyter/jupyter_notebook_config.py
CMD jupyter notebook --allow-root --port 8080 --ip 0.0.0.0
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
