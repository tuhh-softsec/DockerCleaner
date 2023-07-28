#   Tag: nvidia/cuda:10.0-cudnn7-devel-ubuntu16.04
#   Created: 2018-10-22T21:14:30.605789926Z
#   Label: com.nvidia.cuda.version: 10.0.
#   Label: com.nvidia.cudnn.version: 7.3.1.20
#   Label: com.nvidia.nccl.version: 2.3.5
#
#   To build, run from the parent with the command line:
#   	docker build -t <image name> -f CNTK-GPU-Image/Dockerfile .
#   Ubuntu 16.04.5
FROM nvidia/cuda@sha256:362e4e25aa46a18dfa834360140e91b61cdb0a3a2796c8e09dadb268b9de3f6b
RUN apt-get update \
 && apt-get install --no-install-recommends autotools-dev=20220109.1 build-essential=12.9ubuntu3 git=1:2.39.2-1ubuntu1 gfortran-multilib=4:12.2.0-3ubuntu1 libavcodec-dev=7:5.1.2-3ubuntu1 libavformat-dev=7:5.1.2-3ubuntu1 libjasper-dev libjpeg-dev=8c-2ubuntu11 libpng-dev=1.6.39-2 liblapacke-dev=3.11.0-2 libswscale-dev=7:5.1.2-3ubuntu1 libtiff-dev=4.5.0-4ubuntu1 pkg-config=1.8.1-1ubuntu2 wget=1.21.3-1ubuntu1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libssl-dev=3.0.8-1ubuntu1 openssl=3.0.8-1ubuntu1 ca-certificates=20230311 curl=7.88.1-7ubuntu1 libcurl4-openssl-dev=7.88.1-7ubuntu1 unzip=6.0-27ubuntu1 python-dev automake=1:1.16.5-1.3 libtool-bin=2.4.7-5 autoconf=2.71-3 subversion=1.14.2-4build2 libapr1=1.7.2-2 libaprutil1=1.6.3-1ubuntu1 libltdl-dev=2.4.7-5 libltdl7=2.4.7-5 libserf-1-1=1.3.9-11 libsigsegv2=2.14-1ubuntu1 libsvn1=1.14.2-4build2 m4=1.4.19-3 openjdk-8-jdk=8u362-ga-0ubuntu2 libpcre3-dev=2:8.39-15 libgdiplus=6.1+dfsg-1build1 apt-transport-https=2.6.0 -y \
 && rm -rf /var/lib/apt/lists/*
ARG CMAKE_DOWNLOAD_VERSION=3.11
ARG CMAKE_BUILD_VERSION=4
RUN DEBIAN_FRONTEND=noninteractive \
 && wget --no-verbose https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/cmake/$CMAKE_DOWNLOAD_VERSION/cmake-$CMAKE_DOWNLOAD_VERSION.$CMAKE_BUILD_VERSION.tar.gz \
 && tar -xzvf cmake-$CMAKE_DOWNLOAD_VERSION.$CMAKE_BUILD_VERSION.tar.gz \
 && cd cmake-$CMAKE_DOWNLOAD_VERSION.$CMAKE_BUILD_VERSION \
 && ./bootstrap --system-curl -- -DCMAKE_USE_OPENSSL=ON \
 && make -j $( nproc ;) install \
 && cd .. \
 && rm -rf cmake-$CMAKE_DOWNLOAD_VERSION.$CMAKE_BUILD_VERSION
ARG OPENMPI_VERSION=1.10.7
RUN wget -q -O - https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/openmpi/$OPENMPI_VERSION/openmpi-$OPENMPI_VERSION.tar.gz | tar -xzf - \
 && cd openmpi-${OPENMPI_VERSION} \
 && apt-get update -y \
 && apt-get install --no-install-recommends -y -f \
 && apt-get install --no-install-recommends libsysfs2=2.1.1-4 libsysfs-dev=2.1.1-4 -y \
 && ./configure --with-verbs --with-cuda=/usr/local/cuda --prefix=/usr/local/mpi \
 && make -j $( nproc ;) install \
 && cd .. \
 && rm -rf openmpi-${OPENMPI_VERSION}
ENV PATH="/usr/local/mpi/bin:$PATH"
ENV LD_LIBRARY_PATH="/usr/local/mpi/lib:$LD_LIBRARY_PATH"
ARG LIBZIP_VERSION=1.1.2
RUN wget -q -O - https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/libzip/$LIBZIP_VERSION/libzip-$LIBZIP_VERSION.tar.gz | tar -xzf - \
 && cd libzip-${LIBZIP_VERSION} \
 && ./configure \
 && make -j $( nproc ;) install \
 && cd .. \
 && rm -rf libzip-${LIBZIP_VERSION}
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ARG CUB_VERSION=1.8.0
RUN wget https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/cub/$CUB_VERSION/cub-$CUB_VERSION.zip \
 && unzip -d /usr/local cub-${CUB_VERSION}.zip \
 && rm cub-${CUB_VERSION}.zip
ARG OPENCV_VERSION=3.1.0
RUN wget -q -O - https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/opencv/$OPENCV_VERSION/opencv-$OPENCV_VERSION.tar.gz | tar -xzf - \
 && cd opencv-${OPENCV_VERSION} \
 && cmake -DWITH_CUDA=OFF -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=/usr/local/opencv-${OPENCV_VERSION} . \
 && make -j $( nproc ;) install \
 && cd .. \
 && rm -rf opencv-${OPENCV_VERSION}
ARG OPENBLAS_VERSION=0.2.18
RUN wget -q -O - https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/openblas/$OPENBLAS_VERSION/OpenBLAS-$OPENBLAS_VERSION.tar.gz | tar -xzf - \
 && cd OpenBLAS-${OPENBLAS_VERSION} \
 && make -j 2 MAKE_NB_JOBS=0 USE_OPENMP=1 | tee make.log \
 && grep -qF 'OpenBLAS build complete. (BLAS CBLAS LAPACK LAPACKE)' make.log \
 && grep -qF 'Use OpenMP in the multithreading.' make.log \
 && make PREFIX=/usr/local/openblas install \
 && cd .. \
 && rm -rf OpenBLAS-${OPENBLAS_VERSION}
ENV LD_LIBRARY_PATH="/usr/local/openblas/lib:$LD_LIBRARY_PATH"
ARG BOOST_VERSION=1.60.0
RUN BOOST_UNDERSCORE_VERSION=$( echo $BOOST_VERSION | tr . _ ;) \
 && wget -q -O - https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/boost/$BOOST_VERSION/boost_$BOOST_UNDERSCORE_VERSION.tar.gz | tar -xzf - \
 && cd boost_${BOOST_UNDERSCORE_VERSION} \
 && ./bootstrap.sh --prefix=/usr/local/boost-${BOOST_VERSION} \
 && ./b2 -d0 -j $( nproc ;) install \
 && cd .. \
 && rm -rf boost_${BOOST_UNDERSCORE_VERSION}
#   Install Protobuf
ARG PROTOBUF_VERSION=3.1.0
RUN PROTOBUF_STRING=protobuf-$PROTOBUF_VERSION \
 && wget -O - --no-verbose https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/protobuf/$PROTOBUF_VERSION/protobuf-$PROTOBUF_VERSION.tar.gz | tar -xzf - \
 && cd $PROTOBUF_STRING \
 && ./autogen.sh \
 && ./configure CFLAGS=-fPIC CXXFLAGS=-fPIC --disable-shared --prefix=/usr/local/$PROTOBUF_STRING \
 && make -j $( nproc ;) install \
 && cd .. \
 && rm -rf $PROTOBUF_STRING
#   Install MKLDNN and MKLML
ARG MKLDNN_VERSION=0.14
ARG MKLDNN_LONG_VERSION=mklml_lnx_2018.0.3.20180406
RUN mkdir /usr/local/mklml \
 && wget --no-verbose -O - https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/mkl-dnn/$MKLDNN_VERSION/$MKLDNN_LONG_VERSION.tgz | tar -xzf - -C /usr/local/mklml \
 && MKLDNN_STRING=mkl-dnn-${MKLDNN_VERSION} \
 && wget --no-verbose -O - https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/mkl-dnn/$MKLDNN_VERSION/mkl-dnn-$MKLDNN_VERSION.tar.gz | tar -xzf - \
 && cd ${MKLDNN_STRING} \
 && ln -s /usr/local external \
 && mkdir -p build \
 && cd build \
 && cmake .. -DCMAKE_INSTALL_PREFIX=/ \
 && make \
 && make install DESTDIR=/usr/local \
 && make install DESTDIR=/usr/local/mklml/${MKLDNN_LONG_VERSION} \
 && cd ../.. \
 && rm -rf ${MKLDNN_STRING}
#   Install Kaldi
ARG KALDI_VERSION=c024e8aa
ARG KALDI_PATH=/usr/local/kaldi-$KALDI_VERSION
RUN mv /bin/sh /bin/sh.orig \
 && ln -s -f /bin/bash /bin/sh \
 && mkdir $KALDI_PATH \
 && wget --no-verbose -O - https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/kaldi/$KALDI_VERSION/kaldi-$KALDI_VERSION.tar.gz | tar -xzf - --strip-components=1 -C $KALDI_PATH \
 && cd $KALDI_PATH \
 && cd tools \
 && perl -pi -e 's/^# (OPENFST_VERSION = 1.4.1)$/\1/' Makefile \
 && ./extras/check_dependencies.sh \
 && make -j $( nproc ;) all \
 && cd ../src \
 && perl -pi -e 's/-gencode arch=compute_20,code=sm_20//' cudamatrix/Makefile \
 && ./configure --openblas-root=/usr/local/openblas --shared \
 && make -j $( nproc ;) depend \
 && make -j $( nproc ;) all \
 && find $KALDI_PATH -name '*.o' -print0 | xargs -0 rm \
 && for dir in $KALDI_PATH/src/*bin; do make -C $dir clean ; done \
 && mv -f /bin/sh.orig /bin/sh
#  # PYTHON
#   Commit that will be used for Python environment creation (and later, compilation)
ARG COMMIT=master
#   Swig
ARG SWIG_VERSION=3.0.10
ARG CACHEBUST=1
RUN wget -q https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/swig/$SWIG_VERSION/swig-$SWIG_VERSION.tar.gz -O - | tar xvfz - \
 && cd swig-$SWIG_VERSION \
 && ./configure --prefix=/usr/local/swig-$SWIG_VERSION --without-perl5 --without-alllang \
 && make -j $( nproc ;) \
 && make install \
 && cd .. \
 && rm -rf swig-$SWIG_VERSION
COPY ./Patches /tmp/patches
RUN /tmp/patches/patch_swig.sh /usr/local/share/swig/3.0.10 \
 && rm -rfd /tmp/patches
#   .NET Core SDK
RUN wget -q https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/packages-microsoft-prod/deb/packages-microsoft-prod.deb \
 && dpkg -i packages-microsoft-prod.deb \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 -y -f \
 && apt-get update -y \
 && apt-get install --no-install-recommends dotnet-sdk-2.1 -y -f \
 && rm ./packages-microsoft-prod.deb
#   Anaconda
ARG ANACONDA_VERSION=4.2.0
RUN wget -q https://cntkbuildstorage.blob.core.windows.net/cntk-ci-dependencies/anaconda3/$ANACONDA_VERSION/Anaconda3-$ANACONDA_VERSION-Linux-x86_64.sh \
 && bash Anaconda3-$ANACONDA_VERSION-Linux-x86_64.sh -b \
 && rm Anaconda3-$ANACONDA_VERSION-Linux-x86_64.sh
RUN CONDA_ENV_PATH=/tmp/conda-linux-cntk-py35-environment.yml ; wget -q https://raw.githubusercontent.com/Microsoft/CNTK/$COMMIT/Scripts/install/linux/conda-linux-cntk-py35-environment.yml -O "$CONDA_ENV_PATH" \
 && /root/anaconda3/bin/conda env create -p /root/anaconda3/envs/cntk-py35 --file "$CONDA_ENV_PATH" \
 && rm -f "$CONDA_ENV_PATH"
ENV PATH="/root/anaconda3/envs/cntk-py35/bin:$PATH"
WORKDIR /cntk
#   Allow CNTK's configure to pick up GDK and CuDNN where it expects it.
#   (Note: $CUDNN_VERSION is defined by NVidia's base image)
#   TODO hack, CNTK configure should be improved.
RUN _CUDNN_VERSION=$( echo $CUDNN_VERSION | cut -d. -f1-2 ;) \
 && mkdir -p /usr/local/cudnn-$_CUDNN_VERSION/cuda/include \
 && ln -s /usr/include/cudnn.h /usr/local/cudnn-$_CUDNN_VERSION/cuda/include/cudnn.h \
 && mkdir -p /usr/local/cudnn-$_CUDNN_VERSION/cuda/lib64 \
 && ln -s /etc/alternatives/libcudnn_so /usr/local/cudnn-$_CUDNN_VERSION/cuda/lib64/libcudnn.so \
 && ln -s /usr/local/cudnn-$_CUDNN_VERSION \
 && mkdir -p /usr/src/gdk/nvml/lib \
 && cp -av /usr/local/cuda/lib64/stubs/libnvidia-ml* /usr/src/gdk/nvml/lib \
 && cp -av /usr/local/cuda/lib64/stubs/libnvidia-ml.so /usr/src/gdk/nvml/lib/libnvidia-ml.so.1 \
 && mkdir -p /usr/include/nvidia/gdk \
 && cp -av /usr/local/cuda/include/nvml.h /usr/include/nvidia/gdk/nvml.h
#   Build CNTK
RUN git clone --depth=1 --recursive -b $COMMIT https://github.com/Microsoft/CNTK.git cntksrc \
 && cd cntksrc \
 && MKLML_VERSION_DETAIL=${MKLDNN_LONG_VERSION} \
 && CONFIGURE_OPTS=" --with-kaldi=${KALDI_PATH} --with-py35-path=/root/anaconda3/envs/cntk-py35" \
 && mkdir -p build/gpu/release \
 && cd build/gpu/release \
 && ../../../configure $CONFIGURE_OPTS --with-openblas=/usr/local/openblas \
 && make -j"$( nproc ;)" all \
 && cd ../../.. \
 && mkdir -p build-mkl/gpu/release \
 && cd build-mkl/gpu/release \
 && ../../../configure $CONFIGURE_OPTS --with-mkl=/usr/local/mklml/$MKLML_VERSION_DETAIL \
 && make -j"$( nproc ;)" all
RUN cd cntksrc/Examples/Image/DataSets/CIFAR-10 \
 && python install_cifar10.py \
 && cd ../../../..
RUN cd cntksrc/Examples/Image/DataSets/MNIST \
 && python install_mnist.py \
 && cd ../../../..
ENV PATH="/cntk/cntksrc/build/gpu/release/bin:$PATH" \
    PYTHONPATH="/cntk/cntksrc/bindings/python" \
    LD_LIBRARY_PATH="/cntk/cntksrc/bindings/python/cntk/libs:$LD_LIBRARY_PATH"
#   Install CNTK as the default backend for Keras
ENV KERAS_BACKEND="cntk"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
