#  FROM ubuntu:16.04
#   FROM jfinmetrix/rhadley_ubuntu
FROM ubuntu:trusty
#  FROM debian:stretch
#  FROM nvidia/cuda:8.0-devel-ubuntu16.04
MAINTAINER Shlomo <shlomo@deep-ml.com>
ENV DEBIAN_FRONTEND="noninteractive"
ENV PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:."
RUN rm -rf /var/lib/apt/lists/*
RUN apt-get clean
#   install debian packages
ENV DEBIAN_FRONTEND="noninteractive"
#  Install dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 cmake=2.8.12.2-0ubuntu3 build-essential=11.6ubuntu6 libgoogle-glog-dev=0.3.3-1 libgflags-dev=2.0-1.1ubuntu1 libeigen3-dev=3.2.0-8 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 libcppnetlib-dev=0.11.0-1 libboost-dev=1.54.0.1ubuntu1 libboost-iostreams-dev=1.54.0.1ubuntu1 libcurl4-openssl-dev=7.35.0-1ubuntu2.20 protobuf-compiler=2.5.0-9ubuntu1 libopenblas-dev=0.2.8-6ubuntu1 libhdf5-dev=1.8.11-5ubuntu7.1 libprotobuf-dev=2.5.0-9ubuntu1 libleveldb-dev=1.15.0-2 libsnappy-dev=1.1.0-1ubuntu1 liblmdb-dev=0.9.10-1 libutfcpp-dev=2.3.4-1 wget=1.15-1ubuntu1.14.04.5 unzip=6.0-9ubuntu1.5 supervisor=3.0b2-1ubuntu0.1 python=2.7.5-5ubuntu3 python-dev=2.7.5-5ubuntu3 python2.7-dev=2.7.6-8ubuntu0.5 python3-dev=3.4.0-0ubuntu2 python-virtualenv=1.11.4-1ubuntu1 python-wheel=0.24.0-1~ubuntu1.1 python-tk=2.7.5-1ubuntu1 pkg-config=0.26-1ubuntu4 libopenblas-base=0.2.8-6ubuntu1 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 python-h5py=2.2.1-1build2 python-yaml=3.10-4ubuntu0.1 python-pydot=1.0.28-0ubuntu1 python-nose=1.3.1-2 python-h5py=2.2.1-1build2 python-skimage=0.9.3-4build1 python-matplotlib=1.3.1-1ubuntu5.1 python-pandas=0.13.1-2ubuntu2 python-sklearn=0.14.1-2 python-sympy=0.7.4.1-1 python-joblib=0.7.1-1 build-essential=11.6ubuntu6 software-properties-common=0.92.37.8 g++=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 wget=1.15-1ubuntu1.14.04.5 tar=1.27.1-1ubuntu0.1 git=1:1.9.1-1ubuntu0.10 imagemagick=8:6.7.7.10-6ubuntu3.13 curl=7.35.0-1ubuntu2.20 bc=1.06.95-8ubuntu1 htop=1.0.2-3 curl=7.35.0-1ubuntu2.20 g++=4:4.8.2-1ubuntu6 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libfreetype6-dev=2.5.2-1ubuntu2.8 libhdf5-dev=1.8.11-5ubuntu7.1 libjpeg-dev=8c-2ubuntu8 liblcms2-dev=2.5-0ubuntu4.2 libopenblas-dev=0.2.8-6ubuntu1 liblapack-dev=3.5.0-2ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 libtiff5-dev=4.0.3-7ubuntu0.11 libwebp-dev=0.4.0-4 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 nano=2.2.6-1ubuntu1 unzip=6.0-9ubuntu1.5 vim=2:7.4.052-1ubuntu3.1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 qt5-default=5.2.1+dfsg-1ubuntu14.3 libvtk6-dev=6.0.0-6 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libjpeg-dev=8c-2ubuntu8 libwebp-dev=0.4.0-4 libpng-dev libtiff5-dev=4.0.3-7ubuntu0.11 libjasper-dev=1.900.1-14ubuntu3.5 libopenexr-dev=1.6.1-7ubuntu1 libgdal-dev=1.10.1+dfsg-5ubuntu1 libdc1394-22-dev=2.2.1-2ubuntu2 libavcodec-dev=6:9.20-0ubuntu0.14.04.1 libavformat-dev=6:9.20-0ubuntu0.14.04.1 libswscale-dev=6:9.20-0ubuntu0.14.04.1 libtheora-dev=1.1.1+dfsg.1-3.2 libvorbis-dev=1.3.2-1.3ubuntu1.2 libxvidcore-dev=2:1.3.2-9ubuntu1 libx264-dev=2:0.142.2389+git956c8d8-2 yasm=1.2.0-1ubuntu1 libopencore-amrnb-dev=0.1.3-2ubuntu1 libopencore-amrwb-dev=0.1.3-2ubuntu1 libv4l-dev=1.0.1-1 libxine2-dev=1.2.4-2ubuntu1 libtbb-dev=4.2~20130725-1.1ubuntu1 libeigen3-dev=3.2.0-8 doxygen=1.8.6-2 less=458-2 htop=1.0.2-3 procps=1:3.3.9-1ubuntu2.3 vim-tiny=2:7.4.052-1ubuntu3.1 libboost-dev=1.54.0.1ubuntu1 libgraphviz-dev=2.36.0-0ubuntu3.2 -y ) \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* \
 && update-alternatives --set libblas.so.3 /usr/lib/openblas-base/libblas.so.3
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 clinfo=0.0.20130513-1 cmake=2.8.12.2-0ubuntu3 git=1:1.9.1-1ubuntu0.10 libboost-all-dev=1.54.0.1ubuntu1 libfftw3-dev=3.3.3-7ubuntu3 libfontconfig1-dev=2.11.0-0ubuntu4.2 libfreeimage-dev=3.15.4-3ubuntu0.1 liblapack-dev=3.5.0-2ubuntu1 liblapacke-dev=3.5.0-2ubuntu1 libopenblas-dev=0.2.8-6ubuntu1 ocl-icd-opencl-dev=2.1.3-4 opencl-headers=1.2-2013.10.23-1 wget=1.15-1ubuntu1.14.04.5 xorg-dev=1:7.7+1ubuntu8.1 -y ) \
 && rm -rf /var/lib/apt/lists/*
WORKDIR "/root"
WORKDIR /root/
RUN wget https://cmake.org/files/v3.8/cmake-3.8.0-rc4.tar.gz
RUN tar -xvf cmake-3.8.0-rc4.tar.gz
WORKDIR /root/cmake-3.8.0-rc4
RUN /root/cmake-3.8.0-rc4/bootstrap
RUN make
RUN make install
WORKDIR /root
#   Build GLFW from source
RUN git clone https://github.com/glfw/glfw.git \
 && cd glfw \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr .. \
 && make -j4 \
 && make install
RUN (apt-get update ;apt-get install --no-install-recommends libopenblas-dev=0.2.8-6ubuntu1 libfftw3-dev=3.3.3-7ubuntu3 liblapacke-dev=3.5.0-2ubuntu1 )
WORKDIR /root
ENV AF_PATH="/opt/arrayfire" \
    AF_DISABLE_GRAPHICS="1"
#  ARG COMPILE_GRAPHICS=ON
ENV AF_PATH="/opt/arrayfire" \
    AF_DISABLE_GRAPHICS="1"
#  ARG COMPILE_GRAPHICS=OFF
RUN git clone --recursive https://github.com/arrayfire/arrayfire.git -b master \
 && cd arrayfire \
 && mkdir build \
 && cd build \
 && cmake .. -DCMAKE_INSTALL_PREFIX=/opt/arrayfire-3 -DCMAKE_BUILD_TYPE=Release -DBUILD_CPU=ON -DBUILD_CUDA=OFF -DBUILD_OPENCL=OFF -DBUILD_UNIFIED=ON -DBUILD_GRAPHICS=OFF -DBUILD_NONFREE=OFF -DBUILD_EXAMPLES=ON -DBUILD_TEST=ON -DBUILD_DOCS=OFF -DINSTALL_FORGE_DEV=ON -DUSE_FREEIMAGE_STATIC=OFF \
 && make -j8 \
 && make install \
 && mkdir -p ${AF_PATH} \
 && ln -s /opt/arrayfire-3/* ${AF_PATH}/ \
 && echo "${AF_PATH}/lib" >> /etc/ld.so.conf.d/arrayfire.conf \
 && echo "/usr/local/cuda/nvvm/lib64" >> /etc/ld.so.conf.d/arrayfire.conf \
 && ldconfig
#  WORKDIR "/root"
#  WORKDIR /root/
#  RUN wget https://cmake.org/files/v3.8/cmake-3.8.0-rc4.tar.gz
#  RUN tar -xvf cmake-3.8.0-rc4.tar.gz
#  WORKDIR /root/cmake-3.8.0-rc4
#  RUN /root/cmake-3.8.0-rc4/bootstrap
#  RUN make
#  RUN make install
ENV PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:."
WORKDIR /root
RUN git clone https://github.com/jpbarrette/curlpp.git
WORKDIR /root/curlpp
RUN cmake .
RUN make install
WORKDIR /root
RUN git clone https://github.com/wjakob/nanogui.git
WORKDIR /root/nanogui
RUN git submodule update --init --recursive
WORKDIR /root/nanogui
RUN cmake .
RUN make
#  WORKDIR /root
#  RUN git clone https://github.com/glfw/glfw.git
#  WORKDIR /root/glfw/
#  RUN cmake .
#  RUN make
#  RUN make install
WORKDIR /root/
RUN git clone https://github.com/ocornut/imgui.git
WORKDIR /root/imgui/examples/opengl2_example/
RUN make
#  RUN make install
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 clinfo=0.0.20130513-1 git=1:1.9.1-1ubuntu0.10 libboost-all-dev=1.54.0.1ubuntu1 libfftw3-dev=3.3.3-7ubuntu3 libfontconfig1-dev=2.11.0-0ubuntu4.2 libfreeimage-dev=3.15.4-3ubuntu0.1 liblapack-dev=3.5.0-2ubuntu1 liblapacke-dev=3.5.0-2ubuntu1 libopenblas-dev=0.2.8-6ubuntu1 ocl-icd-opencl-dev=2.1.3-4 opencl-headers=1.2-2013.10.23-1 wget=1.15-1ubuntu1.14.04.5 xorg-dev=1:7.7+1ubuntu8.1 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   Setting up symlinks for libcuda and OpenCL ICD
RUN ln -s /usr/local/cuda/lib64/stubs/libcuda.so /usr/lib/libcuda.so.1 \
 && ln -s /usr/lib/libcuda.so.1 /usr/lib/libcuda.so \
 && mkdir -p /etc/OpenCL/vendors \
 && echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd \
 && echo "/usr/local/nvidia/lib" >> /etc/ld.so.conf.d/nvidia.conf \
 && echo "/usr/local/nvidia/lib64" >> /etc/ld.so.conf.d/nvidia.conf
ENV PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:${PATH}"
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
RUN apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 clang-3.7 lldb-3.7 llvm-3.7 python-clang-3.7 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN echo "deb http://ppa.launchpad.net/keithw/glfw3/ubuntu trusty main" | tee -a /etc/apt/sources.list.d/fillwave_ext.list
RUN echo "deb-src http://ppa.launchpad.net/keithw/glfw3/ubuntu trusty main" | tee -a /etc/apt/sources.list.d/fillwave_ext.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libglfw3 libglfw3-dev -qqy --force-yes )
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib/llvm-3.7/lib/:/usr/local/lib/"
ENV LIBRARY_PATH="$LIBRARY_PATH:$LD_LIBRARY_PATH:/usr/lib/llvm-3.7/lib/:/usr/local/lib/"
ENV LLVM_CONFIG="/usr/lib/llvm-3.7/bin/llvm-config"
WORKDIR /root
RUN git clone https://github.com/vurtun/nuklear.git
WORKDIR /root/nuklear/demo/glfw_opengl2/
RUN make
#  ###################################################PYTHON2########################################################
#   Install pip
#   pip dependencies
RUN curl --silent https://bootstrap.pypa.io/get-pip.py | python
RUN pip install setuptools==33.1.1 --no-cache-dir
#   Install other useful Python packages using pip
RUN pip install Cython==0.29.34 werkzeug==2.2.3 pillow==9.5.0 psycogreen==1.0.2 flask==2.2.3 celery==5.2.7 redis==4.5.4 Boto==2.49.0 FileChunkIO==1.8 nltk==3.8.1 fuzzywuzzy==0.18.0 rotate-backups==8.1 oauthlib==3.2.2 requests==2.28.2 pyOpenSSL==23.1.1 ndg-httpsclient==0.5.1 pyasn1==0.4.8 path.py==12.5.0 Pillow==9.5.0 pygments==2.15.0 six==1.16.0 sphinx==6.1.3 wheel==0.40.0 zmq==0.0.0 --no-cache-dir
ENV LD_LIBRARY_PATH="/usr/lib/llvm-3.7/lib/"
ENV LLVM_CONFIG="/usr/lib/llvm-3.7/bin/llvm-config"
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 scons=2.3.0-2ubuntu1 pkg-config=0.26-1ubuntu4 libx11-dev=2:1.6.2-1ubuntu2.1 libxcursor-dev=1:1.1.14-1ubuntu0.14.04.2 libxinerama-dev=2:1.1.3-1 libgl1-mesa-dev=10.1.3-0ubuntu0.6 libglu-dev libasound2-dev=1.0.27.2-3ubuntu7 libpulse-dev=1:4.0-0ubuntu11.1 libfreetype6-dev=2.5.2-1ubuntu2.8 libssl-dev=1.0.1f-1ubuntu2.27 libudev-dev=204-5ubuntu20.31 libxrandr-dev=2:1.5.0-1~trusty1 -qyy )
RUN pip install cython==0.29.34 pytest==7.3.1 pandas==2.0.0 scikit-learn==1.2.2 statsmodels==0.13.5 line-profiler==4.0.3 psutil==5.9.4 spectrum==0.8.1 memory_profiler==0.61.0 pandas==2.0.0 joblib==1.2.0 pyparsing==3.0.9 pydot==1.4.2 pydot-ng==2.0.0 graphviz==0.20.1 pandoc==2.3 SQLAlchemy==2.0.9 flask==2.2.3 toolz==0.12.0 cloudpickle==2.2.1 python-snappy==0.6.1 s3fs==2023.4.0 widgetsnbextension==4.0.7 ipywidgets==8.0.6 terminado==0.17.1 cytoolz==0.12.1 bcolz==1.2.1 blosc==1.11.1 partd==1.4.0 backports.lzma==0.0.14 mock==5.0.2 cachey==0.2.1 moto==4.1.7 pandas_datareader==0.10.0 --no-cache-dir
RUN pip install llvmlite==0.39.1 -i https://pypi.anaconda.org/sklam/simple
RUN pip install fastparquet==2023.2.0 --no-cache-dir
#   Install Theano and set up Theano config (.theanorc) OpenBLAS
RUN pip install theano==1.0.5 --no-cache-dir \
 && echo "[global]\ndevice=cpu\nfloatX=float32\nmode=FAST_RUN \n[lib]\ncnmem=0.95 \n[nvcc]\nfastmath=True \n[blas]\nldflag = -L/usr/lib/openblas-base -lopenblas \n[DebugMode]\ncheck_finite=1" > /root/.theanorc
#   Install BAYESIAN FRAMEWORKS
RUN pip install pymc3==3.11.5 pystan==3.6.0 edward==1.3.5 watermark==2.3.1 xgboost==1.7.5 bokeh==3.1.0 seaborn==0.12.2 mmh3==3.1.0 tensorflow==2.12.0 theano==1.0.5 --no-cache-dir --upgrade
ENV KERAS_VERSION="1.2.2"
ENV KERAS_BACKEND="tensorflow"
RUN pip install git+https://github.com/fchollet/keras.git@${KERAS_VERSION} --no-cache-dir --no-dependencies
#  ###################################################PYTHON2########################################################
#   configure console
RUN echo 'alias ll="ls --color=auto -lA"' >> /root/.bashrc \
 && echo '"\e[5~": history-search-backward' >> /root/.inputrc \
 && echo '"\e[6~": history-search-forward' >> /root/.inputrc
#   RUN which python2.7 /usr/bin/python2.7
RUN ls -la /usr/bin/python2.7
RUN ln -s /opt/python2.7/lib/python2.7/config/libpython2.7.a /usr/local/lib/
ENV LDFLAGS="-L/opt/python2.7/lib:usr/lib/openblas-base/"
ENV PATH="/usr/lib/openblas-base/:/usr/lib/openblas-base/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:."
#   Set up notebook config
RUN (apt-get update ;apt-get install --no-install-recommends python2.7=2.7.6-8ubuntu0.5 python-pip=1.5.4-1ubuntu4 python-dev=2.7.5-5ubuntu3 ipython=1.2.1-2 ipython-notebook=1.2.1-2 -qyy )
RUN pip install pip==23.1 --upgrade
RUN pip install ipython==8.12.0 --upgrade
RUN pip install jupyter==1.0.0 --no-cache-dir
RUN python -m ipykernel.kernelspec
RUN python2 -m ipykernel.kernelspec --user
RUN jupyter notebook --allow-root --generate-config -y
COPY jupyter_notebook_config.py /root/.jupyter/
#   Jupyter has issues with being run directly: https://github.com/ipython/ipython/issues/7062
COPY run_jupyter.sh /root/
WORKDIR "/root/"
RUN chmod +x run_jupyter.sh
RUN jupyter nbextension enable --py --sys-prefix widgetsnbextension
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 )
RUN add-apt-repository ppa:george-edison55/cmake-3.x
RUN :
ENV LD_LIBRARY_PATH="/usr/local/lib:/usr/local/lib/root"
RUN (apt-get update ;apt-get install --no-install-recommends libsasl2-dev=2.1.25.dfsg1-17build1 libldap2-dev=2.4.31-1+nmu2ubuntu8.5 libssl-dev=1.0.1f-1ubuntu2.27 libpq-dev=9.3.24-0ubuntu0.14.04 postgresql-9.3=9.3.24-0ubuntu0.14.04 postgresql-client-9.3=9.3.24-0ubuntu0.14.04 postgresql-contrib-9.3=9.3.24-0ubuntu0.14.04 -qqy )
RUN pip install superset==0.30.1 pyhive==0.6.5 mysqlclient==2.1.1 skflow==0.1.0 bayesian-optimization==1.4.2 scikit-neuralnetwork==0.7 simplejson==3.19.1 ujson==5.7.0 bson==0.5.10 pyyaml==6.0 python-ldap==3.4.3 superset==0.30.1 pyhive==0.6.5 psycopg2==2.9.6 SQLAlchemy==2.0.9 arrayfire==3.8.0
RUN (apt-get update ;apt-get install --no-install-recommends postgresql=9.3+154ubuntu1.1 postgresql-contrib=9.3+154ubuntu1.1 -qqy )
#  Run pip install http://h2o-release.s3.amazonaws.com/h2o/rel-turing/10/Python/h2o-3.10.0.10-py2.py3-none-any.whl
#   Expose Ports for TensorBoard (6006), Ipython (8888) drill
EXPOSE 6006/tcp 3838/tcp 8787/tcp 8888/tcp 8786/tcp 9786/tcp 8788/tcp 5432/tcp
RUN (apt-get update ;apt-get install --no-install-recommends mesa-common-dev=10.1.3-0ubuntu0.6 freeglut3-dev=2.8.1-1 libglfw-dev=2.7.2-1 libglm-dev=0.9.5.1-1 libglew1.6-dev xorg-dev=1:7.7+1ubuntu8.1 libglu1-mesa-dev=9.0.0-2 libsdl2-dev=2.0.2+dfsg1-3ubuntu1.3 -qqy )
USER postgres
#   Create a PostgreSQL role named ``docker`` with ``docker`` as the password and
#   then create a database `docker` owned by the ``docker`` role.
#   Note: here we use ``&&\`` to run commands one after the other - the ``\``
#         allows the RUN command to span multiple lines.
RUN /etc/init.d/postgresql start \
 && psql --command "CREATE USER docker WITH SUPERUSER PASSWORD 'docker';" \
 && createdb -O docker docker
#   Adjust PostgreSQL configuration so that remote connections to the
#   database are possible.
RUN echo "host all all 0.0.0.0/0 md5" >> /etc/postgresql/9.3/main/pg_hba.conf
#   And add ``listen_addresses`` to ``/etc/postgresql/9.3/main/postgresql.conf``
RUN echo "listen_addresses='*'" >> /etc/postgresql/9.3/main/postgresql.conf
#   Add VOLUMEs to allow backup of config, logs and databases
VOLUME ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]
#   Set the default command to run when starting the container
CMD ["/usr/lib/postgresql/9.3/bin/postgres", "-D", "/var/lib/postgresql/9.3/main", "-c", "config_file=/etc/postgresql/9.3/main/postgresql.conf"]
USER root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
