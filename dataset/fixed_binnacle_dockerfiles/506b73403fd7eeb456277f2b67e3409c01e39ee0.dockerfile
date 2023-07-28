FROM ubuntu:14.04
MAINTAINER Sai Soundararaj <saip@outlook.com>
ARG THEANO_VERSION=rel-0.8.2
ARG TENSORFLOW_VERSION=0.12.1 
ARG TENSORFLOW_ARCH=cpu
ARG KERAS_VERSION=1.2.0
ARG LASAGNE_VERSION=v0.1
ARG TORCH_VERSION=latest
ARG CAFFE_VERSION=master
#   Install some dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bc=1.06.95-8ubuntu1 build-essential=11.6ubuntu6 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 g++=4:4.8.2-1ubuntu6 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libfreetype6-dev=2.5.2-1ubuntu2.8 libhdf5-dev=1.8.11-5ubuntu7.1 libjpeg-dev=8c-2ubuntu8 liblcms2-dev=2.5-0ubuntu4.2 libopenblas-dev=0.2.8-6ubuntu1 liblapack-dev=3.5.0-2ubuntu1 libopenjpeg2=1.3+dfsg-4.7ubuntu1 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libssl-dev=1.0.1f-1ubuntu2.27 libtiff5-dev=4.0.3-7ubuntu0.11 libwebp-dev=0.4.0-4 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 nano=2.2.6-1ubuntu1 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 software-properties-common=0.92.37.8 unzip=6.0-9ubuntu1.5 vim=2:7.4.052-1ubuntu3.1 wget=1.15-1ubuntu1.14.04.5 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 qt5-default=5.2.1+dfsg-1ubuntu14.3 libvtk6-dev=6.0.0-6 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libjpeg-dev=8c-2ubuntu8 libwebp-dev=0.4.0-4 libpng-dev libtiff5-dev=4.0.3-7ubuntu0.11 libjasper-dev=1.900.1-14ubuntu3.5 libopenexr-dev=1.6.1-7ubuntu1 libgdal-dev=1.10.1+dfsg-5ubuntu1 libdc1394-22-dev=2.2.1-2ubuntu2 libavcodec-dev=6:9.20-0ubuntu0.14.04.1 libavformat-dev=6:9.20-0ubuntu0.14.04.1 libswscale-dev=6:9.20-0ubuntu0.14.04.1 libtheora-dev=1.1.1+dfsg.1-3.2 libvorbis-dev=1.3.2-1.3ubuntu1.2 libxvidcore-dev=2:1.3.2-9ubuntu1 libx264-dev=2:0.142.2389+git956c8d8-2 yasm=1.2.0-1ubuntu1 libopencore-amrnb-dev=0.1.3-2ubuntu1 libopencore-amrwb-dev=0.1.3-2ubuntu1 libv4l-dev=1.0.1-1 libxine2-dev=1.2.4-2ubuntu1 libtbb-dev=4.2~20130725-1.1ubuntu1 libeigen3-dev=3.2.0-8 python-dev=2.7.5-5ubuntu3 python-tk=2.7.5-1ubuntu1 python-numpy=1:1.8.2-0ubuntu0.1 python3-dev=3.4.0-0ubuntu2 python3-tk=3.4.3-1~14.04.2 python3-numpy=1:1.8.2-0ubuntu0.1 ant=1.9.3-2ubuntu0.1 default-jdk=2:1.7-51 doxygen=1.8.6-2 -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* \
 && update-alternatives --set libblas.so.3 /usr/lib/openblas-base/libblas.so.3
#   Install pip
RUN curl -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
#   Add SNI support to Python
RUN pip install pyopenssl==23.1.1 ndg-httpsclient==0.5.1 pyasn1==0.4.8 --no-cache-dir
#   Install useful Python packages using apt-get to avoid version incompatibilities with Tensorflow binary
#   especially numpy, scipy, skimage and sklearn (see https://github.com/tensorflow/tensorflow/issues/2034)
RUN apt-get update \
 && apt-get install --no-install-recommends python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 python-nose=1.3.1-2 python-h5py=2.2.1-1build2 python-skimage=0.9.3-4build1 python-matplotlib=1.3.1-1ubuntu5.1 python-pandas=0.13.1-2ubuntu2 python-sklearn=0.14.1-2 python-sympy=0.7.4.1-1 -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
#   Install other useful Python packages using pip
RUN pip install ipython==8.12.0 --no-cache-dir --upgrade \
 && pip install Cython==0.29.34 ipykernel==6.22.0 jupyter==1.0.0 path.py==12.5.0 Pillow==9.5.0 pygments==2.15.0 six==1.16.0 sphinx==6.1.3 wheel==0.40.0 zmq==0.0.0 --no-cache-dir \
 && python -m ipykernel.kernelspec
#   Install TensorFlow
RUN pip install https://storage.googleapis.com/tensorflow/linux/${TENSORFLOW_ARCH}/tensorflow-${TENSORFLOW_VERSION}-cp27-none-linux_x86_64.whl --no-cache-dir
#   Install dependencies for Caffe
RUN apt-get update \
 && apt-get install --no-install-recommends libboost-all-dev=1.54.0.1ubuntu1 libgflags-dev=2.0-1.1ubuntu1 libgoogle-glog-dev=0.3.3-1 libhdf5-serial-dev=1.8.11-5ubuntu7.1 libleveldb-dev=1.15.0-2 liblmdb-dev=0.9.10-1 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 libprotobuf-dev=2.5.0-9ubuntu1 libsnappy-dev=1.1.0-1ubuntu1 protobuf-compiler=2.5.0-9ubuntu1 -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
#   Install Caffe
RUN git clone -b ${CAFFE_VERSION} --depth 1 https://github.com/BVLC/caffe.git /root/caffe \
 && cd /root/caffe \
 && cat python/requirements.txt | xargs -n1 pip install \
 && mkdir build \
 && cd build \
 && cmake -DCPU_ONLY=1 -DBLAS=Open .. \
 && make -j"$( nproc ;)" all \
 && make install
#   Set up Caffe environment variables
ENV CAFFE_ROOT="/root/caffe"
ENV PYCAFFE_ROOT="$CAFFE_ROOT/python"
ENV PYTHONPATH="$PYCAFFE_ROOT:$PYTHONPATH" \
    PATH="$CAFFE_ROOT/build/tools:$PYCAFFE_ROOT:$PATH"
RUN echo "$CAFFE_ROOT/build/lib" >> /etc/ld.so.conf.d/caffe.conf \
 && ldconfig
#   Install Theano and set up Theano config (.theanorc) OpenBLAS
RUN pip install git+git://github.com/Theano/Theano.git@${THEANO_VERSION} --no-cache-dir \
 && echo "[global]\ndevice=cpu\nfloatX=float32\nmode=FAST_RUN \n[lib]\ncnmem=0.95 \n[nvcc]\nfastmath=True \n[blas]\nldflag = -L/usr/lib/openblas-base -lopenblas \n[DebugMode]\ncheck_finite=1" > /root/.theanorc
#   Install Keras
RUN pip install git+git://github.com/fchollet/keras.git@${KERAS_VERSION} --no-cache-dir
#   Install Lasagne
RUN pip install git+git://github.com/Lasagne/Lasagne.git@${LASAGNE_VERSION} --no-cache-dir
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
#   Install the latest versions of nn, and iTorch
RUN luarocks install nn \
 && luarocks install loadcaffe \
 && cd /root \
 && git clone https://github.com/facebook/iTorch.git \
 && cd iTorch \
 && luarocks make
#   Install OpenCV
RUN git clone --depth 1 https://github.com/opencv/opencv.git /root/opencv \
 && cd /root/opencv \
 && mkdir build \
 && cd build \
 && cmake -DWITH_QT=ON -DWITH_OPENGL=ON -DFORCE_VTK=ON -DWITH_TBB=ON -DWITH_GDAL=ON -DWITH_XINE=ON -DBUILD_EXAMPLES=ON .. \
 && make -j"$( nproc ;)" \
 && make install \
 && ldconfig \
 && echo 'ln /dev/null /dev/raw1394' >> ~/.bashrc
#   Set up notebook config
COPY jupyter_notebook_config.py /root/.jupyter/
#   Jupyter has issues with being run directly: https://github.com/ipython/ipython/issues/7062
COPY run_jupyter.sh /root/
#   Expose Ports for TensorBoard (6006), Ipython (8888)
EXPOSE 6006/tcp 8888/tcp
WORKDIR "/root"
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
