#   Modified from h\ttps://github.com/saiprashanths/dl-docker
FROM nvidia/cuda:7.5-cudnn5-devel
MAINTAINER Brian Lee Yung Rowe <rowe@zatonovo.com>
ARG THEANO_VERSION=rel-0.8.2
ARG KERAS_VERSION=1.1.0
ARG TORCH_VERSION=latest
#   Install some dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends curl wget git nano vim bc build-essential pkg-config software-properties-common cmake g++ gfortran libopenblas-dev liblapack-dev libffi-dev libfreetype6-dev libhdf5-dev liblcms2-dev libjpeg-dev libopenjpeg2 libpng12-dev libtiff5-dev libssl-dev libwebp-dev libzmq3-dev python-dev zlib1g-dev unzip -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* \
 && update-alternatives --set libblas.so.3 /usr/lib/openblas-base/libblas.so.3
#   Install latest version of pip
RUN curl -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
#   Add SNI support to Python
RUN pip install pyopenssl==23.1.1 ndg-httpsclient==0.5.1 pyasn1==0.4.8 --no-cache-dir
#   Install useful Python packages using apt-get to avoid version incompatibilities with Tensorflow binary
#   especially numpy, scipy, skimage and sklearn (see https://github.com/tensorflow/tensorflow/issues/2034)
RUN apt-get update \
 && apt-get install --no-install-recommends python-numpy python-scipy python-nose python-h5py python-skimage python-matplotlib python-pandas python-sklearn python-sympy -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
#   Install other useful Python packages using pip
RUN pip install ipython==8.12.0 --no-cache-dir --upgrade \
 && pip install Cython==0.29.34 ipykernel==6.22.0 jupyter==1.0.0 path.py==12.5.0 Pillow==9.5.0 pygments==2.15.0 six==1.16.0 sphinx==6.1.3 wheel==0.40.0 zmq==0.0.0 --no-cache-dir \
 && python -m ipykernel.kernelspec
#   Install Theano and set up Theano config (.theanorc) for CUDA and OpenBLAS
RUN pip install git+git://github.com/Theano/Theano.git@${THEANO_VERSION} --no-cache-dir \
 && echo "[global]\ndevice=gpu\nfloatX=float32\noptimizer_including=cudnn\nmode=FAST_RUN \n[lib]\ncnmem=0.95 \n[nvcc]\nfastmath=True \n[blas]\nldflag = -L/usr/lib/openblas-base -lopenblas \n[DebugMode]\ncheck_finite=1" > /root/.theanorc
#   Install Keras
RUN pip install git+git://github.com/fchollet/keras.git@${KERAS_VERSION} --no-cache-dir
#   Install Torch
RUN git clone https://github.com/torch/distro.git /root/torch --recursive \
 && cd /root/torch \
 && bash install-deps \
 && yes no | ./install.sh
#   Export the LUA evironment variables manually
ENV LUA_PATH="/root/.luarocks/share/lua/5.1/?.lua;/root/.luarocks/share/lua/5.1/?/init.lua;/root/torch/install/share/lua/5.1/?.lua;/root/torch/install/share/lua/5.1/?/init.lua;./?.lua;/root/torch/install/share/luajit-2.1.0-beta1/?.lua;/usr/local/share/lua/5.1/?.lua;/usr/local/share/lua/5.1/?/init.lua" \
    LUA_CPATH="/root/.luarocks/lib/lua/5.1/?.so;/root/torch/install/lib/lua/5.1/?.so;./?.so;/usr/local/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/loadall.so" \
    PATH="/root/torch/install/bin:$PATH" \
    LD_LIBRARY_PATH="/root/torch/install/lib:$LD_LIBRARY_PATH" \
    DYLD_LIBRARY_PATH="/root/torch/install/lib:$DYLD_LIBRARY_PATH"
ENV LUA_CPATH="/root/torch/install/lib/?.so;$LUA_CPATH"
#   Install the latest versions of nn, cutorch, cunn, cuDNN bindings and iTorch
RUN luarocks install nn \
 && luarocks install cutorch \
 && luarocks install cunn \
 && luarocks install dp \
 && luarocks install mnist \
 && luarocks install qtlua \
 && cd /root \
 && git clone https://github.com/soumith/cudnn.torch.git \
 && cd cudnn.torch \
 && git checkout R4 \
 && luarocks make \
 && cd /root \
 && git clone https://github.com/facebook/iTorch.git \
 && cd iTorch \
 && luarocks make
RUN wget https://raw.githubusercontent.com/rtsisyk/luafun/master/fun-scm-1.rockspec \
 && luarocks install fun-scm-1.rockspec
#   Set up notebook config
COPY jupyter_notebook_config.py /root/.jupyter/
#   Jupyter has issues with being run directly: https://github.com/ipython/ipython/issues/7062
COPY run_jupyter.sh /root/
#   Use theano backend
COPY keras.json /root/.keras/
#   Expose Ports for IPython (8888)
EXPOSE 8888/tcp
WORKDIR "/code"
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
