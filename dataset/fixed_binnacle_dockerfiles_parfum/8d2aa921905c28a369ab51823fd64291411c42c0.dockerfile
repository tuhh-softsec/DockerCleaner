FROM ubuntu:16.04
#  Install base packages that would be needed for any builder or runner
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
 && apt-get install --no-install-recommends software-properties-common -y \
 && add-apt-repository -y ppa:george-edison55/cmake-3.x \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf automake build-essential cmake curl fontconfig fonts-wqy-microhei gcc-4.8 gcc-4.8-multilib g++-4.8 g++-4.8-multilib gfortran git-core libatlas-base-dev libblas-dev libfreetype6-dev libgdal-dev libjasper-dev libjpeg-dev libjpeg8-dev liblapack-dev libmagickwand-dev libopenblas-dev libpng-dev libssl-dev libtbb-dev libtiff-dev libx11-dev pandoc pkg-config unzip texlive wget zip -y \
 && rm -rf /var/lib/apt/lists/*
#  Set options that should be defined everywhere
ENV JAVA_TOOL_OPTIONS="-Dfile.encoding=UTF8"
ENV LANG="C.UTF-8"
RUN adduser --disabled-password --gecos "" --uid 1001 algo
#  --------
#  Install gpu-driver
COPY libraries/gpu-driver/install.sh /opt/algorithmia/setup/gpu-driver/install.sh
RUN /opt/algorithmia/setup/gpu-driver/install.sh \
 && rm -rf /var/lib/apt/lists/*
ENV CUDA_ARCH="3.7"
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:$LD_LIBRARY_PATH"
ENV PATH="/usr/local/cuda/bin:$PATH"
#  --------
#  Install python2
COPY libraries/python2/install.sh /opt/algorithmia/setup/python2/install.sh
RUN /opt/algorithmia/setup/python2/install.sh \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/opt/anaconda2/bin:$PATH"
ENV PYTHON_LIB_PATH="/opt/anaconda2/lib"
ENV PYTHON_VERSION="python2.7"
#  --------
#  Install caffe
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
#  --------
#  Install pycuda
COPY libraries/pycuda/install.sh /opt/algorithmia/setup/pycuda/install.sh
RUN /opt/algorithmia/setup/pycuda/install.sh \
 && rm -rf /var/lib/apt/lists/*
#  --------
#  Install torch
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
#  --------
#  Install xgboost
COPY libraries/xgboost/install.sh /opt/algorithmia/setup/xgboost/install.sh
RUN /opt/algorithmia/setup/xgboost/install.sh \
 && rm -rf /var/lib/apt/lists/*
#  --------
#  Install open-mpi
COPY libraries/open-mpi/install.sh /opt/algorithmia/setup/open-mpi/install.sh
RUN /opt/algorithmia/setup/open-mpi/install.sh \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/usr/local/mpi/bin:$PATH"
#  --------
#  Install opencv-python2
COPY libraries/opencv-python2/install.sh /opt/algorithmia/setup/opencv-python2/install.sh
RUN /opt/algorithmia/setup/opencv-python2/install.sh \
 && rm -rf /var/lib/apt/lists/*
#  --------
#  Install ffmpeg
COPY libraries/ffmpeg/install.sh /opt/algorithmia/setup/ffmpeg/install.sh
RUN /opt/algorithmia/setup/ffmpeg/install.sh \
 && rm -rf /var/lib/apt/lists/*
#  --------
#  Add langserver binary and algorithm directory
RUN mkdir /opt/algorithm \
 && chown algo /opt/algorithm
COPY bin/init-langserver /bin/
COPY target/release/langserver /bin/
USER algo
