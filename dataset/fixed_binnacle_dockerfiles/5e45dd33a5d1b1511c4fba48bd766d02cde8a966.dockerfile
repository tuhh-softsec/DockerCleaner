FROM nvidia/cuda:9.0-cudnn7-devel-ubuntu16.04
MAINTAINER nejumi <dr_jingles@mac.com>
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common -y ) \
 && add-apt-repository ppa:git-core/ppa \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git build-essential cmake -y ) \
 && git --version
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends curl wget bzip2 ca-certificates libglib2.0-0 libxext6 libsm6 libxrender1 git vim mercurial subversion cmake libboost-dev libboost-system-dev libboost-filesystem-dev gcc g++ -y )
#   Add OpenCL ICD files for LightGBM
RUN mkdir -p /etc/OpenCL/vendors \
 && echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd
#  #############################################################################
#   TINI
#  #############################################################################
#   Install tini
ENV TINI_VERSION="v0.14.0"
RUN which wget &> /dev/null || (apt-get update ;apt-get install --no-install-recommends wget=1.20.3 ) ; wget --no-verbose --output-document /tini https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini
RUN chmod +x /tini
#  #############################################################################
#   anaconda python
#  #############################################################################
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends wget bzip2 ca-certificates -y ) \
 && rm -rf /var/lib/apt/lists/*
RUN wget https://repo.continuum.io/archive/Anaconda3-5.2.0-Linux-x86_64.sh \
 && /bin/bash Anaconda3-5.2.0-Linux-x86_64.sh -b -p /opt/conda \
 && rm Anaconda3-5.2.0-Linux-x86_64.sh
ENV PATH="/opt/conda/bin:$PATH"
RUN pip install pip==23.1 --upgrade
RUN : \
 && rm /opt/conda/lib/libstdc++* \
 && rm /opt/conda/lib/libgomp.* \
 && ln -s /usr/lib/x86_64-linux-gnu/libgomp.so.1 /opt/conda/lib/libgomp.so.1 \
 && ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6 /opt/conda/lib/libstdc++.so.6
#  #############################################################################
#   LightGBM-GPU
#  #############################################################################
RUN cd /usr/local/src \
 && mkdir lightgbm \
 && cd lightgbm \
 && git clone --recursive https://github.com/Microsoft/LightGBM \
 && cd LightGBM \
 && mkdir build \
 && cd build \
 && cmake -DUSE_GPU=1 -DOpenCL_LIBRARY=/usr/local/cuda/lib64/libOpenCL.so -DOpenCL_INCLUDE_DIR=/usr/local/cuda/include/ .. \
 && make OPENCL_HEADERS=/usr/local/cuda-8.0/targets/x86_64-linux/include LIBOPENCL=/usr/local/cuda-8.0/targets/x86_64-linux/lib
ENV PATH="/usr/local/src/lightgbm/LightGBM:${PATH}"
RUN /bin/bash -c "cd /usr/local/src/lightgbm/LightGBM/python-package \
 && python setup.py install --precompile"
#  #############################################################################
#   XGBoost-GPU
#  #############################################################################
RUN cd /usr/local/src \
 && git clone --recursive https://github.com/dmlc/xgboost \
 && cd xgboost \
 && mkdir build \
 && cd build \
 && cmake .. -DPLUGIN_UPDATER_GPU=ON \
 && make -j4 \
 && cd ../python-package \
 && python3 setup.py install
#  #############################################################################
#   keras
#  #############################################################################
RUN cd /usr/local/src \
 && pip install tensorflow-gpu==2.12.0 --no-cache-dir -I -U
RUN pip install keras==2.12.0
#  #############################################################################
#   other libraries
#  #############################################################################
RUN cd /usr/local/src \
 && pip install catboost==1.1.1 kaggle==1.5.13 umap-learn==0.5.3 tqdm==4.65.0 nltk==3.8.1 hdbscan==0.8.29 spacy==3.5.2 category_encoders==2.6.0 gensim==4.3.1 optuna==3.1.1 cupy==12.0.0
RUN python -m spacy download en
RUN cd /usr/local/src \
 && pip install torch==2.0.0 torchvision==0.15.1
RUN cd /usr/local/src \
 && pip install git+https://github.com/hyperopt/hyperopt.git
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
