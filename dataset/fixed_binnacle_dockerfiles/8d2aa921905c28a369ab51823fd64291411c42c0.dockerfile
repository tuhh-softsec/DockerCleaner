FROM ubuntu:16.04
#   Install base packages that would be needed for any builder or runner
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y \
 && add-apt-repository -y ppa:george-edison55/cmake-3.x \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-9 automake=1:1.15-4ubuntu1 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 fontconfig=2.11.94-0ubuntu1.1 fonts-wqy-microhei=0.2.0-beta-2 gcc-4.8=4.8.5-4ubuntu2 gcc-4.8-multilib=4.8.5-4ubuntu2 g++-4.8=4.8.5-4ubuntu2 g++-4.8-multilib=4.8.5-4ubuntu2 gfortran=4:5.3.1-1ubuntu1 git-core=1:2.7.4-0ubuntu1.10 libatlas-base-dev=3.10.2-9 libblas-dev=3.6.0-2ubuntu2 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libgdal-dev=1.11.3+dfsg-3build2 libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libjpeg-dev=8c-2ubuntu8 libjpeg8-dev=8c-2ubuntu8 liblapack-dev=3.6.0-2ubuntu2 libmagickwand-dev=8:6.8.9.9-7ubuntu5.16 libopenblas-dev=0.2.18-1ubuntu1 libpng-dev libssl-dev=1.0.2g-1ubuntu4.20 libtbb-dev=4.4~20151115-0ubuntu3 libtiff-dev libx11-dev=2:1.6.3-1ubuntu2.2 pandoc=1.16.0.2~dfsg-1 pkg-config=0.29.1-0ubuntu1 unzip=6.0-20ubuntu1.1 texlive=2015.20160320-1ubuntu0.1 wget=1.17.1-1ubuntu1.5 zip=3.0-11 -y \
 && rm -rf /var/lib/apt/lists/*
#   Set options that should be defined everywhere
ENV JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF8"
ENV LANG="C.UTF-8"
RUN adduser --disabled-password --gecos "" --uid 1001 algo
#   --------
#   Install gpu-driver
COPY libraries/gpu-driver/install.sh /opt/algorithmia/setup/gpu-driver/install.sh
RUN /opt/algorithmia/setup/gpu-driver/install.sh \
 && rm -rf /var/lib/apt/lists/*
ENV CUDA_ARCH="3.7"
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:$LD_LIBRARY_PATH"
ENV PATH="/usr/local/cuda/bin:$PATH"
#   --------
#   Install python2
COPY libraries/python2/install.sh /opt/algorithmia/setup/python2/install.sh
RUN /opt/algorithmia/setup/python2/install.sh \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/opt/anaconda2/bin:$PATH"
ENV PYTHON_LIB_PATH="/opt/anaconda2/lib"
ENV PYTHON_VERSION="python2.7"
#   --------
#   Install caffe
COPY libraries/caffe/caffe-ld-so.conf /opt/algorithmia/setup/caffe/caffe-ld-so.conf
COPY libraries/caffe/install-deps.sh /opt/algorithmia/setup/caffe/install-deps.sh
RUN /opt/algorithmia/setup/caffe/install-deps.sh \
 && rm -rf /var/lib/apt/lists/*
COPY libraries/caffe/install-system.sh /opt/algorithmia/setup/caffe/install-system.sh
RUN /opt/algorithmia/setup/caffe/install-system.sh \
 && rm -rf /var/lib/apt/lists/*
COPY libraries/caffe/install.sh /opt/algorithmia/setup/caffe/install.sh
RUN /opt/algorithmia/setup/caffe/install.sh \
 && rm -rf /var/lib/apt/lists/*
ENV PYTHONPATH="/opt/caffe/python:$PYTHONPATH"
#   --------
#   Install pycuda
COPY libraries/pycuda/install.sh /opt/algorithmia/setup/pycuda/install.sh
RUN /opt/algorithmia/setup/pycuda/install.sh \
 && rm -rf /var/lib/apt/lists/*
#   --------
#   Install torch
COPY libraries/torch/install-torch.sh /opt/algorithmia/setup/torch/install-torch.sh
RUN /opt/algorithmia/setup/torch/install-torch.sh \
 && rm -rf /var/lib/apt/lists/*
COPY libraries/torch/install-rocks.sh /opt/algorithmia/setup/torch/install-rocks.sh
RUN /opt/algorithmia/setup/torch/install-rocks.sh \
 && rm -rf /var/lib/apt/lists/*
COPY libraries/torch/install-cuda-rocks.sh /opt/algorithmia/setup/torch/install-cuda-rocks.sh
RUN /opt/algorithmia/setup/torch/install-cuda-rocks.sh \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/opt/torch/install/bin:$PATH"
#   --------
#   Install xgboost
COPY libraries/xgboost/install.sh /opt/algorithmia/setup/xgboost/install.sh
RUN /opt/algorithmia/setup/xgboost/install.sh \
 && rm -rf /var/lib/apt/lists/*
#   --------
#   Install open-mpi
COPY libraries/open-mpi/install.sh /opt/algorithmia/setup/open-mpi/install.sh
RUN /opt/algorithmia/setup/open-mpi/install.sh \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/usr/local/mpi/bin:$PATH"
#   --------
#   Install opencv-python2
COPY libraries/opencv-python2/install.sh /opt/algorithmia/setup/opencv-python2/install.sh
RUN /opt/algorithmia/setup/opencv-python2/install.sh \
 && rm -rf /var/lib/apt/lists/*
#   --------
#   Install ffmpeg
COPY libraries/ffmpeg/install.sh /opt/algorithmia/setup/ffmpeg/install.sh
RUN /opt/algorithmia/setup/ffmpeg/install.sh \
 && rm -rf /var/lib/apt/lists/*
#   --------
#   Add langserver binary and algorithm directory
RUN mkdir /opt/algorithm \
 && chown algo /opt/algorithm
COPY bin/init-langserver /bin/
COPY target/release/langserver /bin/
USER algo
# Please add your HEALTHCHECK here!!!
