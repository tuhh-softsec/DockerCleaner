#  FROM ubuntu:16.04
#   FROM jfinmetrix/rhadley_ubuntu
FROM ubuntu:trusty
#  FROM debian:stretch
MAINTAINER Shlomo <shlomo@deep-ml.com>
#  ENV LLVM_CONFIG /usr/local/opt/llvm/bin/llvm-config
#  ENV LLVM_CONFIG=/usr/lib/llvm-3.8/bin/llvm-config
ARG DEBIAN_FRONTEND=noninteractive
ENV DEBIAN_FRONTEND="noninteractive"
#  Install base dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 wget=1.15-1ubuntu1.14.04.5 cmake=2.8.12.2-0ubuntu3 build-essential=11.6ubuntu6 libgoogle-glog-dev=0.3.3-1 libgflags-dev=2.0-1.1ubuntu1 libeigen3-dev=3.2.0-8 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 libcppnetlib-dev=0.11.0-1 libboost-dev=1.54.0.1ubuntu1 libboost-iostreams-dev=1.54.0.1ubuntu1 libcurlpp-dev=0.7.3-5 libcurl4-openssl-dev=7.35.0-1ubuntu2.20 protobuf-compiler=2.5.0-9ubuntu1 libopenblas-dev=0.2.8-6ubuntu1 libhdf5-dev=1.8.11-5ubuntu7.1 libprotobuf-dev=2.5.0-9ubuntu1 libleveldb-dev=1.15.0-2 libsnappy-dev=1.1.0-1ubuntu1 liblmdb-dev=0.9.10-1 libutfcpp-dev=2.3.4-1 wget=1.15-1ubuntu1.14.04.5 liblapack-dev=3.5.0-2ubuntu1 fortran-compiler libedit-dev=3.1-20130712-2 -y )
RUN :
#   Very complicated step, took me hours to make it works. this is required for fastparquet
RUN echo "deb http://llvm.org/apt/trusty/ llvm-toolchain-trusty main \ndeb-src http://llvm.org/apt/trusty/ llvm-toolchain-trusty main \ndeb http://llvm.org/apt/trusty/ llvm-toolchain-trusty-3.7 main \ndeb-src http://llvm.org/apt/trusty/ llvm-toolchain-trusty-3.7 main" >> /etc/apt/sources.list
RUN wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key | apt-key add -
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends clang-3.7 libclang-common-3.7-dev libclang-3.7-dev libclang1-3.7 libllvm-3.7-ocaml-dev libllvm3.7 lldb-3.7 llvm-3.7 llvm-3.7-dev llvm-3.7-runtime clang-modernize-3.7 clang-format-3.7 lldb-3.7-dev -y )
RUN apt-get clean
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN echo "/usr/lib/llvm-3.7/lib/" >> /etc/ld.so.conf \
 && ldconfig
ENV LD_LIBRARY_PATH="/usr/lib/llvm-3.7/lib/"
ENV LLVM_CONFIG="/usr/lib/llvm-3.7/bin/llvm-config"
RUN sudo apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 clang-3.7 lldb-3.7 llvm-3.7 python-clang-3.7 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  Install dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 software-properties-common=0.92.37.8 g++=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 wget=1.15-1ubuntu1.14.04.5 tar=1.27.1-1ubuntu0.1 git=1:1.9.1-1ubuntu0.10 imagemagick=8:6.7.7.10-6ubuntu3.13 curl=7.35.0-1ubuntu2.20 bc=1.06.95-8ubuntu1 htop=1.0.2-3 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 g++=4:4.8.2-1ubuntu6 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libfreetype6-dev=2.5.2-1ubuntu2.8 libhdf5-dev=1.8.11-5ubuntu7.1 libjpeg-dev=8c-2ubuntu8 liblcms2-dev=2.5-0ubuntu4.2 libopenblas-dev=0.2.8-6ubuntu1 liblapack-dev=3.5.0-2ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 libtiff5-dev=4.0.3-7ubuntu0.11 libwebp-dev=0.4.0-4 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 nano=2.2.6-1ubuntu1 unzip=6.0-9ubuntu1.5 vim=2:7.4.052-1ubuntu3.1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 qt5-default=5.2.1+dfsg-1ubuntu14.3 libvtk6-dev=6.0.0-6 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libjpeg-dev=8c-2ubuntu8 libwebp-dev=0.4.0-4 libpng-dev libtiff5-dev=4.0.3-7ubuntu0.11 libjasper-dev=1.900.1-14ubuntu3.5 libopenexr-dev=1.6.1-7ubuntu1 libgdal-dev=1.10.1+dfsg-5ubuntu1 libdc1394-22-dev=2.2.1-2ubuntu2 libavcodec-dev=6:9.20-0ubuntu0.14.04.1 libavformat-dev=6:9.20-0ubuntu0.14.04.1 libswscale-dev=6:9.20-0ubuntu0.14.04.1 libtheora-dev=1.1.1+dfsg.1-3.2 libvorbis-dev=1.3.2-1.3ubuntu1.2 libxvidcore-dev=2:1.3.2-9ubuntu1 libx264-dev=2:0.142.2389+git956c8d8-2 yasm=1.2.0-1ubuntu1 libopencore-amrnb-dev=0.1.3-2ubuntu1 libopencore-amrwb-dev=0.1.3-2ubuntu1 libv4l-dev=1.0.1-1 libxine2-dev=1.2.4-2ubuntu1 libtbb-dev=4.2~20130725-1.1ubuntu1 libeigen3-dev=3.2.0-8 doxygen=1.8.6-2 less=458-2 htop=1.0.2-3 procps=1:3.3.9-1ubuntu2.3 vim-tiny=2:7.4.052-1ubuntu3.1 libboost-dev=1.54.0.1ubuntu1 libgraphviz-dev=2.36.0-0ubuntu3.2 -y ) \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* \
 && update-alternatives --set libblas.so.3 /usr/lib/openblas-base/libblas.so.3
#  ###################################################PYTHON2########################################################
#   install debian packages
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends python=2.7.5-5ubuntu3 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 python-setuptools=3.3-1ubuntu2 python-virtualenv=1.11.4-1ubuntu1 python-wheel=0.24.0-1~ubuntu1.1 python-tk=2.7.5-1ubuntu1 pkg-config=0.26-1ubuntu4 libopenblas-base=0.2.8-6ubuntu1 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 python-h5py=2.2.1-1build2 python-yaml=3.10-4ubuntu0.1 python-pydot=1.0.28-0ubuntu1 python-nose=1.3.1-2 python-h5py=2.2.1-1build2 python-skimage=0.9.3-4build1 python-matplotlib=1.3.1-1ubuntu5.1 python-pandas=0.13.1-2ubuntu2 python-sklearn=0.14.1-2 python-sympy=0.7.4.1-1 ipython=1.2.1-2 python-joblib=0.7.1-1 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Install pip
RUN curl -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
#  RUN python -m pip install --upgrade --force pip
RUN pip install pip==23.1 --upgrade
RUN pip install pyOpenSSL==23.1.1 ndg-httpsclient==0.5.1 pyasn1==0.4.8
#   Install other useful Python packages using pip
RUN pip install ipython==8.12.0 --no-cache-dir --upgrade \
 && pip install Cython==0.29.34 ipykernel==6.22.0 jupyter==1.0.0 path.py==12.5.0 Pillow==9.5.0 pygments==2.15.0 six==1.16.0 sphinx==6.1.3 wheel==0.40.0 zmq==0.0.0 --no-cache-dir \
 && python -m ipykernel.kernelspec
ENV LD_LIBRARY_PATH="/usr/lib/llvm-3.7/lib/"
ENV LLVM_CONFIG="/usr/lib/llvm-3.7/bin/llvm-config"
RUN pip install cython==0.29.34 pytest==7.3.1 pandas==2.0.0 scikit-learn==1.2.2 statsmodels==0.13.5 line-profiler==4.0.3 psutil==5.9.4 spectrum==0.8.1 memory_profiler==0.61.0 pandas==2.0.0 jupyter==1.0.0 joblib==1.2.0 pyparsing==3.0.9 pydot==1.4.2 pydot-ng==2.0.0 graphviz==0.20.1 pandoc==2.3 SQLAlchemy==2.0.9 flask==2.2.3 toolz==0.12.0 cloudpickle==2.2.1 python-snappy==0.6.1 s3fs==2023.4.0 widgetsnbextension==4.0.7 ipywidgets==8.0.6 terminado==0.17.1 cytoolz==0.12.1 bcolz==1.2.1 blosc==1.11.1 partd==1.4.0 backports.lzma==0.0.14 mock==5.0.2 cachey==0.2.1 moto==4.1.7 pandas_datareader==0.10.0 ipython[all] --no-cache-dir
RUN pip install llvmlite==0.39.1 -i https://pypi.anaconda.org/sklam/simple
#  RUN pip install llvmlite
#   Distributed dataframes
RUN pip install numba==0.56.4 --no-cache-dir
RUN pip install git+https://github.com/dask/dask.git --no-cache-dir
RUN pip install git+https://github.com/dask/distributed.git --no-cache-dir
RUN pip install fastparquet==2023.2.0 --no-cache-dir
#  please point LLVM_CONFIG to the path for llvm-config
#  RUN pip --no-cache-dir install  llvmpy
#   Install Theano and set up Theano config (.theanorc) OpenBLAS
RUN pip install theano==1.0.5 --no-cache-dir \
 && echo "[global]\ndevice=cpu\nfloatX=float32\nmode=FAST_RUN \n[lib]\ncnmem=0.95 \n[nvcc]\nfastmath=True \n[blas]\nldflag = -L/usr/lib/openblas-base -lopenblas \n[DebugMode]\ncheck_finite=1" > /root/.theanorc
ARG TENSORFLOW_VERSION=0.11.0
ARG TENSORFLOW_DEVICE=cpu
ARG TENSORFLOW_APPEND
RUN pip install https://storage.googleapis.com/tensorflow/linux/${TENSORFLOW_DEVICE}/tensorflow${TENSORFLOW_APPEND}-${TENSORFLOW_VERSION}-cp27-none-linux_x86_64.whl --no-cache-dir
ARG KERAS_VERSION=1.2.2
ENV KERAS_BACKEND="tensorflow"
RUN pip install git+https://github.com/fchollet/keras.git@${KERAS_VERSION} --no-cache-dir --no-dependencies
#   Install BAYESIAN FRAMEWORKS
RUN pip install pymc3==3.11.5 pystan==3.6.0 edward==1.3.5 watermark==2.3.1 xgboost==1.7.5 bokeh==3.1.0 seaborn==0.12.2 mmh3==3.1.0 --no-cache-dir --upgrade
#   dump package lists
RUN dpkg-query -l > /dpkg-query-l.txt \
 && pip2 freeze > /pip2-freeze.txt
#  ###################################################PYTHON2########################################################
#   configure console
RUN echo 'alias ll="ls --color=auto -lA"' >> /root/.bashrc \
 && echo '"\e[5~": history-search-backward' >> /root/.inputrc \
 && echo '"\e[6~": history-search-forward' >> /root/.inputrc
#   default password: keras
ENV PASSWD="sha1:98b767162d34:8da1bc3c75a0f29145769edc977375a373407824"
#   dump package lists
RUN dpkg-query -l > /dpkg-query-l.txt \
 && pip3 freeze > /pip3-freeze.txt
RUN git clone https://github.com/dask/dask-tutorial.git ./dask-tutorial
RUN git clone https://github.com/dask/dask-examples.git ./dask-examples
#   Set up notebook config
COPY jupyter_notebook_config.py /root/.jupyter/
#   Jupyter has issues with being run directly: https://github.com/ipython/ipython/issues/7062
COPY run_jupyter.sh /root/
RUN jupyter nbextension enable --py --sys-prefix widgetsnbextension
#   Expose Ports for TensorBoard (6006), Ipython (8888) drill
EXPOSE 6006/tcp 3838/tcp 8787/tcp 8888/tcp 8786/tcp 9786/tcp 8788/tcp
WORKDIR "/root"
RUN git clone https://github.com/dask/dask-tutorial.git ./dask-tutorial
RUN git clone https://github.com/dask/dask-examples.git ./dask-examples
RUN git clone https://github.com/vgvassilev/cling.git ./cling
RUN conda install libgcc
RUN export PATH=/root/cling/bin:$PATH
RUN cd /root/cling/tools/Jupyter/kernel
RUN pip install -e .
RUN ./jupyter-kernelspec install --user cling-c++11
RUN pwd
RUN df -k
RUN chmod +x run_jupyter.sh
RUN ls -la
#  #RUN ./run_jupyter.sh
#  CMD daskd-scheduler &
#  CMD ["/bin/bash", "-c", "./run_jupyter.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
