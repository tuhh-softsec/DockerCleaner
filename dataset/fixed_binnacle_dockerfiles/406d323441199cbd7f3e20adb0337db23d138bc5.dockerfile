FROM ubuntu:14.04
MAINTAINER NVIDIA CORPORATION <digits@nvidia.com>
LABEL com.nvidia.volumes.needed="nvidia_driver"
ENV NVIDIA_GPGKEY_SUM="bd841d59a27a406e513db7d405550894188a4c1cd96bf8aa4f82f1b39e0b5c1c"
ENV NVIDIA_GPGKEY_FPR="889bee522da690103c4b085ed88c3d385c37d3be"
RUN apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/cuda/repos/GPGKEY \
 && apt-key adv --export --no-emit-version -a $NVIDIA_GPGKEY_FPR | tail -n +2 > cudasign.pub \
 && echo "$NVIDIA_GPGKEY_SUM cudasign.pub" | sha256sum -c --strict - \
 && rm cudasign.pub \
 && echo "deb http://developer.download.nvidia.com/compute/cuda/repos/ubuntu1404/x86_64 /" > /etc/apt/sources.list.d/cuda.list
ENV CUDA_VERSION="7.5"
LABEL com.nvidia.cuda.version="7.5"
ENV CUDA_PKG_VERSION="7-5=7.5-18"
RUN apt-get update \
 && apt-get install --no-install-recommends cuda-nvrtc-$CUDA_PKG_VERSION cuda-cusolver-$CUDA_PKG_VERSION cuda-cublas-$CUDA_PKG_VERSION cuda-cufft-$CUDA_PKG_VERSION cuda-curand-$CUDA_PKG_VERSION cuda-cusparse-$CUDA_PKG_VERSION cuda-npp-$CUDA_PKG_VERSION cuda-cudart-$CUDA_PKG_VERSION -y --force-yes \
 && ln -s cuda-$CUDA_VERSION /usr/local/cuda \
 && rm -rf /var/lib/apt/lists/*
RUN echo "/usr/local/cuda/lib" >> /etc/ld.so.conf.d/cuda.conf \
 && echo "/usr/local/cuda/lib64" >> /etc/ld.so.conf.d/cuda.conf \
 && ldconfig
RUN echo "/usr/local/nvidia/lib" >> /etc/ld.so.conf.d/nvidia.conf \
 && echo "/usr/local/nvidia/lib64" >> /etc/ld.so.conf.d/nvidia.conf
ENV PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/nvidia/lib:/usr/local/nvidia/lib64:${LD_LIBRARY_PATH}"
RUN apt-get update \
 && apt-get install --no-install-recommends cuda-core-$CUDA_PKG_VERSION cuda-misc-headers-$CUDA_PKG_VERSION cuda-command-line-tools-$CUDA_PKG_VERSION cuda-license-$CUDA_PKG_VERSION cuda-nvrtc-dev-$CUDA_PKG_VERSION cuda-cusolver-dev-$CUDA_PKG_VERSION cuda-cublas-dev-$CUDA_PKG_VERSION cuda-cufft-dev-$CUDA_PKG_VERSION cuda-curand-dev-$CUDA_PKG_VERSION cuda-cusparse-dev-$CUDA_PKG_VERSION cuda-npp-dev-$CUDA_PKG_VERSION cuda-cudart-dev-$CUDA_PKG_VERSION cuda-driver-dev-$CUDA_PKG_VERSION -y --force-yes \
 && cd /tmp \
 && apt-get download gpu-deployment-kit \
 && rm -rf /var/lib/apt/lists/*
RUN mkdir /tmp/gpu-deployment-kit \
 && cd /tmp/gpu-deployment-kit \
 && dpkg -x /tmp/gpu-deployment-kit_*.deb . \
 && mv usr/include/nvidia/gdk/* /usr/local/cuda/include \
 && mv usr/src/gdk/nvml/lib/* /usr/local/cuda/lib64/stubs \
 && rm -rf /tmp/gpu-deployment-kit*
ENV LIBRARY_PATH="/usr/local/cuda/lib64/stubs:${LIBRARY_PATH}"
RUN echo "deb http://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu1404/x86_64 /" > /etc/apt/sources.list.d/nvidia-ml.list
ENV CUDNN_VERSION="5"
LABEL com.nvidia.cudnn.version="5"
#   workaround: libcudnn5-dev should have an exact version dependency on the runtime library libcudnn5
RUN apt-get update \
 && apt-get install --no-install-recommends libcudnn5=5.0.5-1+cuda7.5 libcudnn5-dev=5.0.5-1+cuda7.5 -y --force-yes \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=11.6ubuntu6 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 graphicsmagick=1.3.18-1ubuntu3.1 libgraphicsmagick1-dev=1.3.18-1ubuntu3.1 libatlas-dev=3.10.1-4 libavcodec-dev=6:9.20-0ubuntu0.14.04.1 libavformat-dev=6:9.20-0ubuntu0.14.04.1 libboost-all-dev=1.54.0.1ubuntu1 libgtk2.0-dev=2.24.23-0ubuntu1.4 libjpeg-dev=8c-2ubuntu8 liblapack-dev=3.5.0-2ubuntu1 libswscale-dev=6:9.20-0ubuntu0.14.04.1 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 python-numpy=1:1.8.2-0ubuntu0.1 python-protobuf=2.5.0-9ubuntu1 software-properties-common=0.92.37.8 zip=3.0-8 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN curl -s https://raw.githubusercontent.com/torch/ezinstall/master/install-deps | bash -e
RUN git clone https://github.com/torch/distro.git ~/torch --recursive
RUN cd ~/torch \
 && ./install.sh \
 && cd install/bin \
 && ./luarocks install nn \
 && ./luarocks install dpnn \
 && ./luarocks install image \
 && ./luarocks install optim \
 && ./luarocks install csvigo \
 && ./luarocks install torchx \
 && ./luarocks install tds
RUN cd ~ \
 && mkdir -p ocv-tmp \
 && cd ocv-tmp \
 && curl -L https://github.com/Itseez/opencv/archive/2.4.11.zip -o ocv.zip \
 && unzip ocv.zip \
 && cd opencv-2.4.11 \
 && mkdir release \
 && cd release \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local -D BUILD_PYTHON_SUPPORT=ON -D CUDA_GENERATION=Kepler .. \
 && make -j8 \
 && make install \
 && rm -rf ~/ocv-tmp
RUN cd ~ \
 && mkdir -p dlib-tmp \
 && cd dlib-tmp \
 && curl -L https://github.com/davisking/dlib/archive/v19.0.tar.gz -o dlib.tar.bz2 \
 && tar xf dlib.tar.bz2 \
 && cd dlib-19.0/python_examples \
 && mkdir build \
 && cd build \
 && cmake ../../tools/python \
 && cmake --build . --config Release \
 && cp dlib.so /usr/local/lib/python2.7/dist-packages \
 && rm -rf ~/dlib-tmp
RUN cd ~/torch/install/bin \
 && ./luarocks install torch \
 && ./luarocks install cutorch
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 graphicsmagick=1.3.18-1ubuntu3.1 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 python-numpy=1:1.8.2-0ubuntu0.1 python-nose=1.3.1-2 python-scipy=0.13.3-1build1 python-pandas=0.13.1-2ubuntu2 python-protobuf=2.5.0-9ubuntu1 wget=1.15-1ubuntu1.14.04.5 zip=3.0-8 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  ADD . /root/openface
RUN git clone https://github.com/cmusatyalab/openface.git ~/openface --recursive
RUN cd ~/openface \
 && ./models/get-models.sh \
 && pip2 install -r requirements.txt \
 && python2 setup.py install \
 && pip2 install -r demos/web/requirements.txt \
 && pip2 install -r training/requirements.txt
EXPOSE 8000/tcp 9000/tcp
#  CMD /bin/bash -l -c '/root/openface/demos/web/start-servers.sh'
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
