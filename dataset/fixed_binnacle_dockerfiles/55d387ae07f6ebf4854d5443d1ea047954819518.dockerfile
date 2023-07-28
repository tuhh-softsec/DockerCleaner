FROM ubuntu:14.04
MAINTAINER berlius <berlius52@yahoo.com>
#   This project contains my code and that of https://github.com/saiprashanths/dl-docker/
#   Install some dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bc=1.06.95-8ubuntu1 build-essential=11.6ubuntu6 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 g++=4:4.8.2-1ubuntu6 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libfreetype6-dev=2.5.2-1ubuntu2.8 libhdf5-dev=1.8.11-5ubuntu7.1 libjpeg-dev=8c-2ubuntu8 liblcms2-dev=2.5-0ubuntu4.2 libopenblas-dev=0.2.8-6ubuntu1 liblapack-dev=3.5.0-2ubuntu1 libopenjpeg2=1.3+dfsg-4.7ubuntu1 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libssl-dev=1.0.1f-1ubuntu2.27 libtiff5-dev=4.0.3-7ubuntu0.11 libwebp-dev=0.4.0-4 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 nano=2.2.6-1ubuntu1 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 swig=2.0.11-1ubuntu2 software-properties-common=0.92.37.8 unzip=6.0-9ubuntu1.5 vim=2:7.4.052-1ubuntu3.1 wget=1.15-1ubuntu1.14.04.5 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 swig=2.0.11-1ubuntu2 libsdl2-2.0 libsdl2-dev=2.0.2+dfsg1-3ubuntu1.3 openjdk-7-jdk=7u211-2.6.17-0ubuntu0.1 lua5.1=5.1.5-5ubuntu0.1 libxerces-c3.1=3.1.1-5.1+deb8u4build0.14.04.1 liblua5.1-0-dev=5.1.5-5ubuntu0.1 libav-tools=6:9.20-0ubuntu0.14.04.1 python-tk=2.7.5-1ubuntu1 timidity=2.13.2-40.2 jackd1=1:0.121.3+20120418git75e3e20b-2.1ubuntu1 mencoder=2:1.1+dfsg1-0ubuntu3.1 lxterminal=0.1.11-4ubuntu3.1 gnome-terminal=3.6.2-0ubuntu1 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 python-nose=1.3.1-2 python-h5py=2.2.1-1build2 python-skimage=0.9.3-4build1 python-matplotlib=1.3.1-1ubuntu5.1 python-pandas=0.13.1-2ubuntu2 python-sklearn=0.14.1-2 python-sympy=0.7.4.1-1 libboost-all-dev=1.54.0.1ubuntu1 libgflags-dev=2.0-1.1ubuntu1 libgoogle-glog-dev=0.3.3-1 libhdf5-serial-dev=1.8.11-5ubuntu7.1 libleveldb-dev=1.15.0-2 liblmdb-dev=0.9.10-1 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 libprotobuf-dev=2.5.0-9ubuntu1 libsnappy-dev=1.1.0-1ubuntu1 protobuf-compiler=2.5.0-9ubuntu1 -y \
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
 && pip install Cython==0.29.34 ipykernel==6.22.0 jupyter==1.0.0 path.py==12.5.0 Pillow==9.5.0 pygments==2.15.0 six==1.16.0 tflearn==0.5.0 sphinx==6.1.3 wheel==0.40.0 zmq==0.0.0 tqdm==4.65.0 chainer==7.8.1 pygame==2.3.0 scikit-neuralnetwork==0.7 gym[all] --no-cache-dir \
 && python -m ipykernel.kernelspec
#   Install TensorFlow
RUN pip install tensorflow==2.12.0 --no-cache-dir
#   Install dependencies for Caffe
RUN apt-get update \
 && apt-get install --no-install-recommends libboost-all-dev=1.54.0.1ubuntu1 libgflags-dev=2.0-1.1ubuntu1 libgoogle-glog-dev=0.3.3-1 libhdf5-serial-dev=1.8.11-5ubuntu7.1 libleveldb-dev=1.15.0-2 liblmdb-dev=0.9.10-1 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 libprotobuf-dev=2.5.0-9ubuntu1 libsnappy-dev=1.1.0-1ubuntu1 protobuf-compiler=2.5.0-9ubuntu1 -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
#   Install Caffe 
RUN git clone -b master --depth 1 https://github.com/BVLC/caffe.git /root/caffe \
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
RUN pip install git+git://github.com/Theano/Theano.git --no-cache-dir \
 && echo "[global]\ndevice=cpu\nfloatX=float32\nmode=FAST_RUN \n[lib]\ncnmem=0.95 \n[nvcc]\nfastmath=True \n[blas]\nldflag = -L/usr/lib/openblas-base -lopenblas \n[DebugMode]\ncheck_finite=1" > /root/.theanorc
#   Install Keras
RUN pip install git+git://github.com/fchollet/keras.git --no-cache-dir
#   Install Lasagne
RUN pip install git+git://github.com/Lasagne/Lasagne.git --no-cache-dir
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
 && cd /root \
 && git clone https://github.com/facebook/iTorch.git \
 && cd iTorch \
 && luarocks make
RUN luarocks install display
RUN luarocks install hdf5
RUN luarocks install image
RUN luarocks install loadcaffe
RUN luarocks install mnist
RUN update-ca-certificates -f
ENV JAVA_HOME="/usr/lib/jvm/java-7-openjdk-amd64" \
    CLASSPATH="/usr/lib/jvm/java-7-openjdk-amd64/libtools.jar:$CLASSPATH" \
    PATH="/usr/lib/jvm/java-7-openjdk-amd64/bin:$PATH"
#   Set up notebook config
COPY jupyter_notebook_config.py /root/.jupyter/
#   Jupyter has issues with being run directly: https://github.com/ipython/ipython/issues/7062
COPY run_jupyter.sh /root/
#   Expose Ports for TensorBoard (6006), Ipython (8888)
EXPOSE 6006/tcp 8888/tcp 8000/tcp
WORKDIR "/root"
CMD ["jackd", "-R", "-d", "alsa", "-d", "hw:1"]
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
