FROM nvidia/cuda:8.0-devel-centos7
#  MKL
RUN mkdir -p /opt/intel/lib
COPY mkl_libs/libmkl_core.a /opt/intel/lib/libmkl_core.a
COPY mkl_libs/libmkl_gnu_thread.a /opt/intel/lib/libmkl_gnu_thread.a
COPY mkl_libs/libmkl_intel_lp64.a /opt/intel/lib/libmkl_intel_lp64.a
COPY mkl_libs/include /opt/intel/
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
RUN yum install -y wget curl perl util-linux xz bzip2 git patch which perl
RUN yum install -y yum-utils centos-release-scl
RUN yum-config-manager --enable rhel-server-rhscl-7-rpms
RUN yum install -y devtoolset-3-gcc devtoolset-3-gcc-c++ devtoolset-3-gcc-gfortran devtoolset-3-binutils
ENV PATH="/opt/rh/devtoolset-3/root/usr/bin:$PATH"
ENV LD_LIBRARY_PATH="/opt/rh/devtoolset-3/root/usr/lib64:/opt/rh/devtoolset-3/root/usr/lib:$LD_LIBRARY_PATH"
#  EPEL for cmake
RUN wget http://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm \
 && rpm -ivh epel-release-latest-7.noarch.rpm \
 && rm -f epel-release-latest-7.noarch.rpm
#  cmake
RUN yum install -y cmake3 \
 && ln -s /usr/bin/cmake3 /usr/bin/cmake
#  build python
COPY build_scripts /build_scripts
RUN bash build_scripts/build.sh \
 && rm -r build_scripts
ENV SSL_CERT_FILE="/opt/_internal/certs.pem"
RUN wget -q https://developer.nvidia.com/compute/cuda/8.0/Prod2/patches/2/cuda_8.0.61.2_linux-run \
 && chmod +x cuda_8.0.61.2_linux-run \
 && cp /usr/local/cuda/version.txt /tmp/ \
 && ./cuda_8.0.61.2_linux-run --silent --accept-eula --installdir=/tmp \
 && yes | cp -P /tmp/lib64/* /usr/local/cuda/lib64/ \
 && rm -rf /usr/local/cuda/lib64/lib*blas.so.8.0.61 \
 && rm -r cuda_8.0.61.2_linux-run
#  cuDNN license: https://developer.nvidia.com/cudnn/license_agreement
RUN curl -fsSL http://developer.download.nvidia.com/compute/redist/cudnn/v7.1.2/cudnn-8.0-linux-x64-v7.1.tgz -O \
 && tar --no-same-owner -xzf cudnn-8.0-linux-x64-v7.1.tgz -C /usr/local \
 && rm cudnn-8.0-linux-x64-v7.1.tgz \
 && ldconfig
#  NCCL2 license: https://docs.nvidia.com/deeplearning/sdk/nccl-sla/index.html
RUN wget -q https://s3.amazonaws.com/pytorch/nccl_2.2.13-1%2Bcuda8.0_x86_64.txz \
 && ls \
 && ls -alh nccl_2.2.13-1+cuda8.0_x86_64.txz \
 && tar --no-same-owner -xvf nccl_2.2.13-1+cuda8.0_x86_64.txz \
 && mv nccl_2.2.13-1+cuda8.0_x86_64/include/* /usr/local/cuda/include/ \
 && cp -P nccl_2.2.13-1+cuda8.0_x86_64/lib/libnccl* /usr/local/cuda/lib64/ \
 && rm -rf nccl_2.2.13-1+cuda8.0_x86_64* \
 && ldconfig
#  magma
RUN wget http://icl.cs.utk.edu/projectsfiles/magma/downloads/magma-2.3.0.tar.gz \
 && tar -xvf magma-2.3.0.tar.gz \
 && cd magma-2.3.0 \
 && wget https://raw.githubusercontent.com/pytorch/builder/master/conda/old/magma-cuda80-2.3.0/cmakelists.patch \
 && wget https://raw.githubusercontent.com/pytorch/builder/master/conda/old/magma-cuda80-2.3.0/thread_queue.patch \
 && wget https://raw.githubusercontent.com/pytorch/builder/master/conda/old/magma-cuda80-2.3.0/magma_cparict_tools.patch \
 && wget https://raw.githubusercontent.com/pytorch/builder/master/conda/old/magma-cuda80-2.3.0/magma_dparict_tools.patch \
 && wget https://raw.githubusercontent.com/pytorch/builder/master/conda/old/magma-cuda80-2.3.0/magma_sparict_tools.patch \
 && wget https://raw.githubusercontent.com/pytorch/builder/master/conda/old/magma-cuda80-2.3.0/magma_zparict_tools.patch \
 && patch < cmakelists.patch \
 && patch -p0 < thread_queue.patch \
 && patch -p0 < magma_cparict_tools.patch \
 && patch -p0 < magma_dparict_tools.patch \
 && patch -p0 < magma_sparict_tools.patch \
 && patch -p0 < magma_zparict_tools.patch \
 && mkdir build \
 && cd build \
 && cmake .. -DUSE_FORTRAN=OFF -DGPU_TARGET="All" -DCMAKE_INSTALL_PREFIX=$PREFIX \
 && make -j$( getconf _NPROCESSORS_CONF ;) \
 && make install \
 && cd ..
RUN rm -f /usr/local/bin/patchelf
RUN git clone https://github.com/NixOS/patchelf \
 && cd patchelf \
 && sed -i 's/serial/parallel/g' configure.ac \
 && ./bootstrap.sh \
 && ./configure \
 && make \
 && make install \
 && cd .. \
 && rm -rf patchelf
# ####################################################################################
#  CUDA 8.0 prune static libs
# ####################################################################################
ARG NVPRUNE="/usr/local/cuda-8.0/bin/nvprune"
ARG CUDA_LIB_DIR="/usr/local/cuda-8.0/lib64"
ARG GENCODE="-gencode arch=compute_35,code=sm_35 -gencode arch=compute_50,code=sm_50 -gencode arch=compute_60,code=sm_60 -gencode arch=compute_61,code=sm_61"
ARG GENCODE_CUDNN="-gencode arch=compute_35,code=sm_35 -gencode arch=compute_37,code=sm_37 -gencode arch=compute_50,code=sm_50 -gencode arch=compute_60,code=sm_60 -gencode arch=compute_61,code=sm_61"
#  all CUDA libs except CuDNN and CuBLAS (cudnn and cublas need arch 3.7 included)
RUN ls $CUDA_LIB_DIR/ | grep "\.a" | grep -v "culibos" | grep -v "cudart" | grep -v "cudnn" | grep -v "cublas" | xargs -I {} bash -c "echo {} \
 && $NVPRUNE $GENCODE $CUDA_LIB_DIR/{} -o $CUDA_LIB_DIR/{}"
#  prune CuDNN and CuBLAS
RUN $NVPRUNE $GENCODE_CUDNN $CUDA_LIB_DIR/libcudnn_static.a -o $CUDA_LIB_DIR/libcudnn_static.a
RUN $NVPRUNE $GENCODE_CUDNN $CUDA_LIB_DIR/libcublas_static.a -o $CUDA_LIB_DIR/libcublas_static.a
RUN $NVPRUNE $GENCODE_CUDNN $CUDA_LIB_DIR/libcublas_device.a -o $CUDA_LIB_DIR/libcublas_device.a
