#   Generated by Neurodocker version 0.4.0
#   Timestamp: 2018-06-22 09:08:27 UTC
#   
#   Thank you for using Neurodocker. If you discover any issues
#   or ways to improve this software, please submit an issue or
#   pull request on our GitHub repository:
#   
#       https://github.com/kaczmarj/neurodocker
FROM neurodebian:stretch-non-free
ARG DEBIAN_FRONTEND="noninteractive"
ENV LANG="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    ND_ENTRYPOINT="/neurodocker/startup.sh"
RUN export ND_ENTRYPOINT="/neurodocker/startup.sh" \
 && apt-get update -qq \
 && apt-get install --no-install-recommends apt-utils bzip2 ca-certificates curl locales unzip -y -q \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
 && dpkg-reconfigure --frontend=noninteractive locales \
 && update-locale LANG="en_US.UTF-8" \
 && chmod 777 /opt \
 && chmod a+s /opt \
 && mkdir -p /neurodocker \
 && if [ ! -f "$ND_ENTRYPOINT" ] ; then echo '#!/usr/bin/env bash' >> "$ND_ENTRYPOINT" \
 && echo 'set -e' >> "$ND_ENTRYPOINT" \
 && echo 'if [ -n "$1" ]; then "$@"; else /usr/bin/env bash; fi' >> "$ND_ENTRYPOINT"; fi \
 && chmod -R 777 /neurodocker \
 && chmod a+s /neurodocker
ENTRYPOINT ["/neurodocker/startup.sh"]
RUN apt-get update -qq \
 && apt-get install --no-install-recommends fsl-core -y -q \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN sed -i '$isource /etc/fsl/fsl.sh' $ND_ENTRYPOINT
RUN echo '{ \n "pkg_manager": "apt", \n "instructions": [ \n [ \n "base", \n "neurodebian:stretch-non-free" \n ], \n [ \n "install", \n [ \n "fsl-core" \n ] \n ], \n [ \n "add_to_entrypoint", \n "source /etc/fsl/fsl.sh" \n ] \n ] \n}' > /neurodocker/neurodocker_specs.json
RUN apt-get update \
 && apt-get install --no-install-recommends git g++ python python-numpy libeigen3-dev zlib1g-dev libqt4-opengl-dev libgl1-mesa-dev libfftw3-dev libtiff5-dev curl -y \
 && apt-get install --no-install-recommends git-core python-setuptools python-dev build-essential -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  # NVIDIA CUDA Installation
#  ##########################
#   LICENSE:
#  #########
#   Copyright (c) 2017, NVIDIA CORPORATION. All rights reserved.
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions
#   are met:
#   * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#   * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#   * Neither the name of NVIDIA CORPORATION nor the names of its
#   contributors may be used to endorse or promote products derived
#   from this software without specific prior written permission.
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
#   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#   PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
#   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
#   OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#   BASE INSTALLATION
#  ##################
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates apt-transport-https -y \
 && rm -rf /var/lib/apt/lists/* \
 && NVIDIA_GPGKEY_SUM=d1be581509378368edeec8c1eb2958702feedf3bc3d17011adbf24efacce4ab5 \
 && NVIDIA_GPGKEY_FPR=ae09fe4bbd223a84b2ccfce3f60f4b3d7fa2af80 \
 && apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/7fa2af80.pub \
 && apt-key adv --export --no-emit-version -a $NVIDIA_GPGKEY_FPR | tail -n +2 > cudasign.pub \
 && echo "$NVIDIA_GPGKEY_SUM cudasign.pub" | sha256sum -c --strict - \
 && rm cudasign.pub \
 && echo "deb https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64 /" > /etc/apt/sources.list.d/cuda.list \
 && echo "deb https://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu1604/x86_64 /" > /etc/apt/sources.list.d/nvidia-ml.list
ENV CUDA_VERSION="9.1.85"
ENV CUDA_PKG_VERSION="9-1=$CUDA_VERSION-1"
RUN apt-get update \
 && apt-get install --no-install-recommends cuda-cudart-$CUDA_PKG_VERSION -y \
 && ln -s cuda-9.1 /usr/local/cuda \
 && rm -rf /var/lib/apt/lists/*
#   nvidia-docker 1.0
LABEL com.nvidia.volumes.needed="nvidia_driver"
LABEL com.nvidia.cuda.version="${CUDA_VERSION}"
RUN echo "/usr/local/nvidia/lib" >> /etc/ld.so.conf.d/nvidia.conf \
 && echo "/usr/local/nvidia/lib64" >> /etc/ld.so.conf.d/nvidia.conf
ENV PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/nvidia/lib:/usr/local/nvidia/lib64"
#   nvidia-container-runtime
ENV NVIDIA_VISIBLE_DEVICES="all"
ENV NVIDIA_DRIVER_CAPABILITIES="compute,utility"
ENV NVIDIA_REQUIRE_CUDA="\"cuda>=9.1\""
#   RUNTIME INSTALLATION
#  #####################
ENV NCCL_VERSION="2.2.12"
RUN apt-get update \
 && apt-get install --no-install-recommends cuda-libraries-$CUDA_PKG_VERSION libnccl2=$NCCL_VERSION-1+cuda9.1 -y \
 && apt-mark hold libnccl2 \
 && rm -rf /var/lib/apt/lists/*
#   CUDNN INSTALLATION
#  ###################
ENV CUDNN_VERSION="7.1.2.21"
LABEL com.nvidia.cudnn.version="${CUDNN_VERSION}"
RUN apt-get update \
 && apt-get install --no-install-recommends libcudnn7=$CUDNN_VERSION-1+cuda9.1 -y \
 && apt-mark hold libcudnn7 \
 && rm -rf /var/lib/apt/lists/*
RUN easy_install pip \
 && pip install wheel==0.40.0 numpy==1.24.2 scipy==1.10.1 nilearn==0.10.0 matplotlib==3.7.1 scikit-image==0.20.0 nibabel==5.1.0 \
 && pip install http://download.pytorch.org/whl/cu91/torch-1.0.0-cp27-cp27mu-linux_x86_64.whl
#   This command does not get cached -> very slow each time when building container -> use prebuild mrtrix_RC3.tar.gz instead
#  RUN mkdir /code && cd /code \
#      && git clone https://github.com/MRtrix3/mrtrix3.git \
#      && cd mrtrix3/ \
#      && git checkout 3.0_RC3 \
#      && ./configure \
#      && ./build \
#      && ./set_path \
RUN mkdir -p ~/.tractseg \
 && mkdir -p /code \
 && curl -SL -o /code/mrtrix3_RC3.tar.gz https://zenodo.org/record/1415322/files/mrtrix3_RC3.tar.gz?download=1
RUN tar -zxvf /code/mrtrix3_RC3.tar.gz -C code \
 && /code/mrtrix3/set_path
#   Uncomment if we want to rebuild the following commands (otherwise using cache)
#  RUN echo "rebuild"
RUN pip install https://github.com/MIC-DKFZ/TractSeg/archive/master.zip --upgrade
RUN download_all_pretrained_weights
#   Does not work -> added mrtrix to path in python
ENV PATH="/code/mrtrix3/bin:$PATH"
#   Using this we can avoid having to call TractSeg each time -> but has problems finding bet then
#  ENTRYPOINT ["TractSeg"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
