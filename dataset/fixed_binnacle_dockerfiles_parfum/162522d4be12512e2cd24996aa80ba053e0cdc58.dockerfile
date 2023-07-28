#  ==================================================================
#  module list
#  ------------------------------------------------------------------
#  cuda          9.0    (apt)
#  python        3.6    (apt)
#  jupyter       latest (pip)
#  pytorch       1.1.0  (pip)
#  tensorflow    1.12.2  (pip)
#  keras         2.1.6  (pip)
#  opencv        4.1.0  (git)
#  ==================================================================
FROM nvidia/cuda:9.0-cudnn7-devel-ubuntu16.04
RUN APT_INSTALL="apt-get install -y --no-install-recommends" \
 && PIP_INSTALL="python -m pip --no-cache-dir install --upgrade" \
 && GIT_CLONE="git clone --depth 10" \
 && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/cuda.list /etc/apt/sources.list.d/nvidia-ml.list \
 && apt-get update \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL build-essential libcupti-dev ca-certificates cmake wget git tmux graphviz vim \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL software-properties-common \
 && add-apt-repository ppa:deadsnakes/ppa \
 && apt-get update \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL python3.6 python3.6-dev python3.6-tk \
 && wget -O ~/get-pip.py https://bootstrap.pypa.io/get-pip.py \
 && python3.6 ~/get-pip.py \
 && ln -s /usr/bin/python3.6 /usr/local/bin/python3 \
 && ln -s /usr/bin/python3.6 /usr/local/bin/python \
 && $PIP_INSTALL setuptools \
 && $PIP_INSTALL jupyter \
 && $PIP_INSTALL torch torchvision \
 && $PIP_INSTALL tensorflow-gpu==1.12.2 \
 && $PIP_INSTALL h5py keras==2.1.6 \
 && $PIP_INSTALL opencv-python \
 && ldconfig \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* /tmp/* ~/*
#  ==================================================================
#  install pip packages from requirements text file
#  ------------------------------------------------------------------
COPY requirements_docker.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt
WORKDIR /microDL
ENV PYTHONPATH="/microDL"
EXPOSE 8888/tcp 6006/tcp
