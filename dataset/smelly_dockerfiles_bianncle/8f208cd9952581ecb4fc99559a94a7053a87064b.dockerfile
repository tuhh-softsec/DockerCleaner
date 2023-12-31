#  ==================================================================
#  py36-tf15
#  ------------------------------------------------------------------
#  python        3.6    (apt)
#  tensorflow    1.5    (pip)
#  pytorch       1.0    (pip)
#  ==================================================================
FROM nvidia/cuda:9.0-cudnn7-devel-ubuntu16.04
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH" \
    PATH="/usr/local/cuda-9.0/bin:$PATH"
ENV LC_ALL="C"
ENV LANG="en_US.UTF8"
ENV LESSCHARSET="UTF-8"
ENV PYTHONIOENCODING="UTF-8"
RUN mkdir ~/.pip \
 && echo "[global]" > ~/.pip/pip.conf \
 && echo "index-url=https://pypi.tuna.tsinghua.edu.cn/simple" >> ~/.pip/pip.conf \
 && echo "format = columns" >> ~/.pip/pip.conf
RUN sed -i 's/archive.ubuntu.com/mirrors.ustc.edu.cn/g' /etc/apt/sources.list \
 && APT_INSTALL="apt-get install -y --no-install-recommends" \
 && PIP_INSTALL="python -m pip --no-cache-dir install --upgrade -i https://pypi.tuna.tsinghua.edu.cn/simple " \
 && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/cuda.list /etc/apt/sources.list.d/nvidia-ml.list \
 && apt-get update \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL build-essential ca-certificates cmake wget git vim openssh-client openssh-server python-tk zip unzip \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL software-properties-common \
 && add-apt-repository ppa:jonathonf/python-3.6 \
 && apt-get update \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL python3.6 python3.6-dev \
 && wget -O ~/get-pip.py https://bootstrap.pypa.io/get-pip.py \
 && python3.6 ~/get-pip.py \
 && ln -s /usr/bin/python3.6 /usr/local/bin/python3 \
 && ln -s /usr/bin/python3.6 /usr/local/bin/python \
 && $PIP_INSTALL setuptools pip \
 && $PIP_INSTALL git+https://github.com/JiahuiYu/neuralgym \
 && $PIP_INSTALL numpy scipy pandas scikit-learn matplotlib opencv-python absl-py Cython sphinx sphinx_rtd_theme pyyaml \
 && $PIP_INSTALL pycocotools h5py ipython==5.8.0 scikit-image easydict \
 && $PIP_INSTALL tensorflow-gpu==1.5.0 tensorboard
#  ==================================================================
#  pytorch 1.0
#  ------------------------------------------------------------------
RUN pip install torch==1.0.0 torchvision --no-cache-dir
#  ==================================================================
#  config & cleanup
#  ------------------------------------------------------------------
RUN ldconfig \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* /tmp/* ~/*
EXPOSE 6006/tcp
