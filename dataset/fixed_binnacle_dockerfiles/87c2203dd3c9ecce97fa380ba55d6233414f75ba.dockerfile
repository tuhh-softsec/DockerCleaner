#   ==================================================================
#   module list
#   ------------------------------------------------------------------
#   python        3.6    (apt)
#   theano        latest (git)
#   lasagne       latest (git)
#   ==================================================================
FROM nvidia/cuda:10.0-cudnn7-devel-ubuntu18.04
ENV LANG="C.UTF-8"
RUN APT_INSTALL="apt-get install -y --no-install-recommends" \
 && PIP_INSTALL="python -m pip --no-cache-dir install --upgrade" \
 && GIT_CLONE="git clone --depth 10" \
 && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/cuda.list /etc/apt/sources.list.d/nvidia-ml.list \
 && : \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL build-essential apt-utils ca-certificates wget git vim \
 && $GIT_CLONE https://github.com/Kitware/CMake ~/cmake \
 && cd ~/cmake \
 && ./bootstrap \
 && make -j"$( nproc ;)" install \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL software-properties-common \
 && add-apt-repository ppa:deadsnakes/ppa \
 && : \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL python3.6 python3.6-dev python3-distutils-extra \
 && wget -O ~/get-pip.py https://bootstrap.pypa.io/get-pip.py \
 && python3.6 ~/get-pip.py \
 && ln -s /usr/bin/python3.6 /usr/local/bin/python3 \
 && ln -s /usr/bin/python3.6 /usr/local/bin/python \
 && $PIP_INSTALL setuptools \
 && $PIP_INSTALL numpy scipy pandas cloudpickle scikit-learn matplotlib Cython \
 && DEBIAN_FRONTEND=noninteractive $APT_INSTALL libblas-dev \
 && wget -qO- https://github.com/Theano/libgpuarray/archive/v0.7.6.tar.gz | tar xz -C ~ \
 && cd ~/libgpuarray* \
 && mkdir -p build \
 && cd build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j"$( nproc ;)" install \
 && cd ~/libgpuarray* \
 && python setup.py build \
 && python setup.py install \
 && printf '[global]\nfloatX = float32\ndevice = cuda0\n\n[dnn]\ninclude_path = /usr/local/cuda/targets/x86_64-linux/include\n' > ~/.theanorc \
 && $PIP_INSTALL https://github.com/Theano/Theano/archive/master.zip \
 && $GIT_CLONE https://github.com/Lasagne/Lasagne ~/lasagne \
 && cd ~/lasagne \
 && $PIP_INSTALL . \
 && ldconfig \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* /tmp/* ~/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
