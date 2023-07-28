#   CNTK Dockerfile
#     CPU only
#     No 1-bit SGD
#
#   To build, run from the parent with the command line:
#   	docker build -t <image name> -f CNTK-CPUOnly-Image/Dockerfile .
FROM ubuntu:16.04
RUN apt-get update \
 && apt-get install --no-install-recommends autotools-dev=20150820.1 build-essential=12.1ubuntu2 git=1:2.7.4-0ubuntu1.10 g++-multilib=4:5.3.1-1ubuntu1 gcc-multilib=4:5.3.1-1ubuntu1 gfortran-multilib=4:5.3.1-1ubuntu1 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libjpeg-dev=8c-2ubuntu8 libpng-dev liblapacke-dev=3.6.0-2ubuntu2 libswscale-dev=7:2.8.17-0ubuntu0.1 libtiff-dev pkg-config=0.29.1-0ubuntu1 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libssl-dev=1.0.2g-1ubuntu4.20 openssl=1.0.2g-1ubuntu4.20 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 unzip=6.0-20ubuntu1.1 python-dev=2.7.12-1~16.04 automake=1:1.15-4ubuntu1 libtool-bin=2.4.6-0.1 autoconf=2.69-9 subversion=1.9.3-2ubuntu1.3 libapr1=1.5.2-3 libaprutil1=1.5.4-1build1 libltdl-dev=2.4.6-0.1 libltdl7=2.4.6-0.1 libserf-1-1=1.3.8-1 libsigsegv2=2.10-4 libsvn1=1.9.3-2ubuntu1.3 m4=1.4.17-5 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 libpcre3-dev=2:8.38-3.1 libgdiplus=4.2-1ubuntu1 apt-transport-https=1.2.35 -y \
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
 && apt-get install --no-install-recommends libsysfs2=2.1.0+repack-4 libsysfs-dev=2.1.0+repack-4 -y \
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
#   Install Boost
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
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 -y -f \
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
#   Build CNTK
RUN git clone --depth=1 --recursive -b $COMMIT https://github.com/Microsoft/CNTK.git cntksrc \
 && cd cntksrc \
 && MKLML_VERSION_DETAIL=${MKLDNN_LONG_VERSION} \
 && CONFIGURE_OPTS=" --with-kaldi=${KALDI_PATH} --with-py35-path=/root/anaconda3/envs/cntk-py35" \
 && mkdir -p build/cpu/release \
 && cd build/cpu/release \
 && ../../../configure $CONFIGURE_OPTS --with-openblas=/usr/local/openblas \
 && make -j"$( nproc ;)" all \
 && cd ../../.. \
 && mkdir -p build-mkl/cpu/release \
 && cd build-mkl/cpu/release \
 && ../../../configure $CONFIGURE_OPTS --with-mkl=/usr/local/mklml/${MKLML_VERSION_DETAIL} \
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
