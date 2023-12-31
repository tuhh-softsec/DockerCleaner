#  Copyright (C) 2019 Zilliqa
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
FROM zilliqa/scilla:v0.3.0
#  Format guideline: one package per line and keep them alphabetically sorted
RUN apt-get update \
 && apt-get install software-properties-common -y \
 && add-apt-repository ppa:tah83/secp256k1 -y \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf build-essential ca-certificates cmake curl git golang libboost-filesystem-dev libboost-program-options-dev libboost-system-dev libboost-test-dev libcurl4-openssl-dev libevent-dev libjsoncpp-dev libjsonrpccpp-dev libleveldb-dev libmicrohttpd-dev libminiupnpc-dev libprotobuf-dev libsnappy-dev libssl-dev libtool ocl-icd-opencl-dev pkg-config protobuf-compiler python python-pip libsecp256k1-dev -y \
 && rm -rf /var/lib/apt/lists/* \
 && pip install setuptools \
 && pip install request requests clint futures
# ############################## CUDA Installation ##############################
#  taken from https://gitlab.com/nvidia/cuda/blob/ubuntu16.04/10.0/base/Dockerfile
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates apt-transport-https gnupg-curl -y \
 && rm -rf /var/lib/apt/lists/* \
 && NVIDIA_GPGKEY_SUM=d1be581509378368edeec8c1eb2958702feedf3bc3d17011adbf24efacce4ab5 \
 && NVIDIA_GPGKEY_FPR=ae09fe4bbd223a84b2ccfce3f60f4b3d7fa2af80 \
 && apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub \
 && apt-key adv --export --no-emit-version -a $NVIDIA_GPGKEY_FPR | tail -n +5 > cudasign.pub \
 && echo "$NVIDIA_GPGKEY_SUM cudasign.pub" | sha256sum -c --strict - \
 && rm cudasign.pub \
 && echo "deb https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64 /" > /etc/apt/sources.list.d/cuda.list \
 && echo "deb https://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu1604/x86_64 /" > /etc/apt/sources.list.d/nvidia-ml.list
ENV CUDA_VERSION="10.0.130"
ENV CUDA_PKG_VERSION="10-0=$CUDA_VERSION-1"
#  For libraries in the cuda-compat-* package: https://docs.nvidia.com/cuda/eula/index.html#attachment-a
RUN apt-get update \
 && apt-get install --no-install-recommends cuda-compat-10-0=410.48-1 cuda-cudart-$CUDA_PKG_VERSION -y \
 && ln -s cuda-10.0 /usr/local/cuda \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/usr/local/cuda/bin:${PATH}"
#  nvidia-container-runtime
ENV NVIDIA_VISIBLE_DEVICES="all"
ENV NVIDIA_DRIVER_CAPABILITIES="compute,utility"
ENV NVIDIA_REQUIRE_CUDA="\"cuda>=10.0 brand=tesla,driver>=384,driver<385\""
#  taken from https://gitlab.com/nvidia/cuda/blob/ubuntu16.04/10.0/runtime/Dockerfile
ENV NCCL_VERSION="2.3.7"
RUN apt-get update \
 && apt-get install --no-install-recommends cuda-libraries-$CUDA_PKG_VERSION cuda-nvtx-$CUDA_PKG_VERSION libnccl2=$NCCL_VERSION-1+cuda10.0 -y \
 && apt-mark hold libnccl2 \
 && rm -rf /var/lib/apt/lists/*
#  taken from https://gitlab.com/nvidia/cuda/blob/ubuntu16.04/10.0/devel/Dockerfile
RUN apt-get update \
 && apt-get install --no-install-recommends cuda-libraries-dev-$CUDA_PKG_VERSION cuda-nvml-dev-$CUDA_PKG_VERSION cuda-minimal-build-$CUDA_PKG_VERSION cuda-command-line-tools-$CUDA_PKG_VERSION libnccl-dev=$NCCL_VERSION-1+cuda10.0 -y \
 && rm -rf /var/lib/apt/lists/*
ENV LIBRARY_PATH="/usr/local/cuda/lib64/stubs"
# ###################### End of CUDA Installation ###############################
ARG ZILLIQA_VERSION=v4.6.1
ARG REPO=https://github.com/Zilliqa/Zilliqa.git
ARG SOURCE_DIR=/zilliqa
ARG BUILD_DIR=/build
ARG INSTALL_DIR=/usr/local
ARG BUILD_TYPE=RelWithDebInfo
ARG EXTRA_CMAKE_ARGS="-DCUDA_MINE=1"
RUN git clone -b ${ZILLIQA_VERSION} --depth 1 ${REPO} ${SOURCE_DIR} \
 && cmake -H${SOURCE_DIR} -B${BUILD_DIR} -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} ${EXTRA_CMAKE_ARGS} \
 && cmake --build ${BUILD_DIR} -- -j$( nproc ;) \
 && cmake --build ${BUILD_DIR} --target install \
 && rm -rf ${BUILD_DIR}
ENV LD_LIBRARY_PATH="${INSTALL_DIR}/lib"
ENTRYPOINT ["/bin/bash"]
