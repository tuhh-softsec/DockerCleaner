#   This image contains all the libs and environment necessary for compiling and running Faster R-CNN
FROM 10.11.3.8:5000/nvidia/cuda:8.0-cudnn6-devel-ubuntu16.04
#  FROM nvidia/cuda:8.0-cudnn6-devel-ubuntu16.04
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH" \
    PATH="/usr/local/cuda-8.0/bin:$PATH"
RUN sed -i 's/archive.ubuntu.com/mirrors.ustc.edu.cn/g' /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential cmake git wget libatlas-base-dev libboost-all-dev libgflags-dev libgoogle-glog-dev libhdf5-serial-dev libleveldb-dev liblmdb-dev libopencv-dev libprotobuf-dev libsnappy-dev protobuf-compiler python-dev python-numpy python-pip python-setuptools python-scipy nano libopenblas-dev liblapack-dev python-tk openssh-client openssh-server autossh expect -y \
 && apt-get install --no-install-recommends libboost-all-dev \
 && apt-get install --no-install-recommends libopenblas-dev liblapack-dev libatlas-base-dev libgflags-dev libgoogle-glog-dev liblmdb-dev gfortran
COPY requirements.txt ./
RUN pip install pip==23.1 --upgrade \
 && hash -r
RUN pip install -i https://pypi.tuna.tsinghua.edu.cn/simple -r requirements.txt
RUN mkdir ~/.pip \
 && echo "[global]" > ~/.pip/pip.conf \
 && echo "index-url=https://mirrors.ustc.edu.cn/pypi/web/simple" >> ~/.pip/pip.conf \
 && echo "format = columns" >> ~/.pip/pip.conf
RUN pip install opencv-python==4.7.0.72 easydict==1.10 \
 && apt-get install --no-install-recommends python-tk liblmdb-dev \
 && pip install protobuf==4.22.3 pyyaml==6.0 lmdb==1.4.1 \
 && apt-get install --no-install-recommends build-essential git libprotobuf-dev libleveldb-dev libsnappy-dev libopencv-dev libboost-all-dev libhdf5-serial-dev libgflags-dev libgoogle-glog-dev liblmdb-dev protobuf-compiler protobuf-c-compiler libyaml-dev libffi-dev libssl-dev python-dev python-pip python3-pip python3-tk time vim screen tmux -y \
 && pip install numpy==1.24.2 scipy==1.10.1 sklearn==0.0.post4 matplotlib==3.7.1 scikit-image==0.20.0 opencv-python==4.7.0.72 h5py==3.8.0 leveldb==0.201 lmdb==1.4.1 protobuf==4.22.3 pandas==2.0.0 imageio==2.27.0 cython==0.29.34 packaging==23.1 SimpleITK==2.2.1 pydicom==2.3.1 tqdm==4.65.0 cffi==1.15.1 tensorboardX==2.6 tensorflow-gpu==1.4.0 \
 && pip install torch==0.4.0 torchvision==0.15.1 cython==0.29.34 \
 && pip3 install torch==0.4.1 torchvision datetime scipy matplotlib opencv-python \
 && git clone https://github.com/NVIDIA/nccl.git \
 && cd nccl \
 && make -j src.build
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
