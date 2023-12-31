#   Ref: https://github.com/Microsoft/CNTK/blob/master/Tools/docker/CNTK-CPUOnly-Image/Dockerfile
#   List of dependencies: https://docs.microsoft.com/en-us/cognitive-toolkit/test-configurations
FROM lablup/kernel-base-python-minimal:3.6-debian
#   FROM lablup/kernel-python:3.6-debian
RUN install_packages autotools-dev build-essential cmake git-core g++-4.8-multilib gcc-4.8-multilib gfortran-multilib libavcodec-dev libavformat-dev libjasper-dev libjpeg-dev libpng-dev liblapacke-dev libswscale-dev libtiff-dev pkg-config wget zlib1g-dev ca-certificates curl zip unzip automake libtool autoconf subversion libapr1 libaprutil1 libltdl-dev libltdl7 libserf-1-1 libsigsegv2 libsvn1 m4 openjdk-7-jdk libpcre3-dev
RUN OPENMPI_VERSION=1.10.3 \
 && wget -q -O - https://www.open-mpi.org/software/ompi/v1.10/downloads/openmpi-${OPENMPI_VERSION}.tar.gz | tar -xzf - \
 && cd openmpi-${OPENMPI_VERSION} \
 && ./configure --prefix=/usr/local/mpi \
 && make -j"$( nproc ;)" install \
 && rm -rf /openmpi-${OPENMPI_VERSION}
ENV PATH="/usr/local/mpi/bin:$PATH"
ENV LD_LIBRARY_PATH="/usr/local/mpi/lib:$LD_LIBRARY_PATH"
RUN LIBZIP_VERSION=1.1.2 \
 && wget -q -O - http://nih.at/libzip/libzip-${LIBZIP_VERSION}.tar.gz | tar -xzf - \
 && cd libzip-${LIBZIP_VERSION} \
 && ./configure \
 && make -j"$( nproc ;)" install \
 && rm -rf /libzip-${LIBZIP_VERSION}
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
RUN OPENCV_VERSION=3.1.0 \
 && wget -q -O - https://github.com/Itseez/opencv/archive/${OPENCV_VERSION}.tar.gz | tar -xzf - \
 && cd opencv-${OPENCV_VERSION} \
 && cmake -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=/usr/local/opencv-${OPENCV_VERSION} . \
 && make -j"$( nproc ;)" install \
 && rm -rf /opencv-${OPENCV_VERSION}
ENV BUILD_TARGET="HASWELL"
RUN OPENBLAS_VERSION=0.2.18 \
 && wget -q -O - https://github.com/xianyi/OpenBLAS/archive/v${OPENBLAS_VERSION}.tar.gz | tar -xzf - \
 && cd OpenBLAS-${OPENBLAS_VERSION} \
 && make -j"$( nproc ;)" TARGET=${BUILD_TARGET} USE_OPENMP=1 | tee make.log \
 && grep -qF 'OpenBLAS build complete. (BLAS CBLAS LAPACK LAPACKE)' make.log \
 && grep -qF 'Use OpenMP in the multithreading.' make.log \
 && make PREFIX=/usr/local/openblas install \
 && rm -rf /OpenBLAS-${OPENBLAS_VERSION}
ENV LD_LIBRARY_PATH="/usr/local/openblas/lib:$LD_LIBRARY_PATH"
#   Install Boost
RUN BOOST_VERSION=1_60_0 \
 && BOOST_DOTTED_VERSION=$( echo $BOOST_VERSION | tr _ . ;) \
 && wget -q -O - https://sourceforge.net/projects/boost/files/boost/${BOOST_DOTTED_VERSION}/boost_${BOOST_VERSION}.tar.gz/download | tar -xzf - \
 && cd boost_${BOOST_VERSION} \
 && ./bootstrap.sh --prefix=/usr/local/boost-${BOOST_DOTTED_VERSION} --with-libraries=filesystem,system,test \
 && ./b2 -d0 -j"$( nproc ;)" install \
 && rm -rf /boost_${BOOST_VERSION}
#   Install Protobuf
RUN PROTOBUF_VERSION=3.1.0 PROTOBUF_STRING=protobuf-$PROTOBUF_VERSION \
 && wget -O - --no-verbose https://github.com/google/protobuf/archive/v${PROTOBUF_VERSION}.tar.gz | tar -xzf - \
 && cd $PROTOBUF_STRING \
 && ./autogen.sh \
 && ./configure CFLAGS=-fPIC CXXFLAGS=-fPIC --disable-shared --prefix=/usr/local/$PROTOBUF_STRING \
 && make -j $( nproc ;) install \
 && cd .. \
 && rm -rf $PROTOBUF_STRING
#   Install MKLML
RUN install_packages python-dev libtool
RUN MKLML_VERSION=v0.11/mklml_lnx_2018.0.1.20171007 \
 && mkdir /usr/local/mklml \
 && wget --no-verbose -O - https://github.com/01org/mkl-dnn/releases/download/$MKLML_VERSION.tgz | tar -xzf - -C /usr/local/mklml
#   Install Kaldi
#   ENV KALDI_VERSION=c024e8aa
#   ENV KALDI_PATH /usr/local/kaldi-$KALDI_VERSION
#   RUN install_packages libtool libatlas3-base
#   RUN mv /bin/sh /bin/sh.orig && \
#      ln -s -f /bin/bash /bin/sh && \
#      mkdir $KALDI_PATH && \
#      wget --no-verbose -O - https://github.com/kaldi-asr/kaldi/archive/$KALDI_VERSION.tar.gz | tar -xzf - --strip-components=1 -C $KALDI_PATH && \
#      cd $KALDI_PATH && \
#      cd tools && \
#      perl -pi -e 's/^# (OPENFST_VERSION = 1.4.1)$/\1/' Makefile && \
#      ./extras/check_dependencies.sh && \
#      make -j $(nproc) all && \
#      cd ../src && \
#      ./configure --openblas-root=/usr/local/openblas --shared && \
#      make -j $(nproc) depend && \
#      make -j $(nproc) all && \
#   # Remove some unneeded stuff in $KALDI_PATH to reduce size
#      find $KALDI_PATH -name '*.o' -print0 | xargs -0 rm && \
#      for dir in $KALDI_PATH/src/*bin; do make -C $dir clean; done && \
#      mv -f /bin/sh.orig /bin/sh
#  # PYTHON
#   Commit that will be used for Python environment creation (and later, compilation)
ARG COMMIT=master
#   Swig
RUN cd /root \
 && wget -q http://prdownloads.sourceforge.net/swig/swig-3.0.10.tar.gz -O - | tar xvfz - \
 && cd swig-3.0.10 \
 && ./configure --without-alllang \
 && make -j $( nproc ;) \
 && make install
#   Anaconda
RUN wget -q https://repo.continuum.io/archive/Anaconda3-4.2.0-Linux-x86_64.sh \
 && bash Anaconda3-4.2.0-Linux-x86_64.sh -b \
 && rm Anaconda3-4.2.0-Linux-x86_64.sh
RUN wget -q https://raw.githubusercontent.com/Microsoft/CNTK/$COMMIT/Scripts/install/linux/conda-linux-cntk-py35-environment.yml -O /tmp/conda-linux-cntk-py35-environment.yml \
 && /root/anaconda3/bin/conda env create -p /root/anaconda3/envs/cntk-py35/ --file /tmp/conda-linux-cntk-py35-environment.yml
ENV PATH="/root/anaconda3/envs/cntk-py35/bin:$PATH"
RUN OPENMPI_VERSION=1.10.7 \
 && wget -q -O - https://www.open-mpi.org/software/ompi/v1.10/downloads/openmpi-${OPENMPI_VERSION}.tar.gz | tar -xzf - \
 && cd openmpi-${OPENMPI_VERSION} \
 && ./configure --prefix=/usr/local/mpi \
 && make -j"$( nproc ;)" install \
 && rm -rf /openmpi-${OPENMPI_VERSION}
#   Build CNTK
WORKDIR /cntk
#   RUN ln -sf /usr/bin/g++-4.8 /usr/bin/g++
#   RUN ln -sf /usr/bin/gcc-4.8 /usr/bin/gcc
RUN g++ --version
RUN gcc --version
RUN git clone --depth=1 -b $COMMIT https://github.com/Microsoft/CNTK.git . \
 && CONFIGURE_OPTS=" --with-py35-path=/root/anaconda3/envs/cntk-py35" \
 && git submodule update --init Source/Multiverso
RUN mkdir -p build/cpu/release \
 && cd build/cpu/release \
 && ../../../configure $CONFIGURE_OPTS --with-openblas=/usr/local/openblas \
 && make -j"$( nproc ;)" all \
 && cd ../../.. \
 && mkdir -p build-mkl/cpu/release \
 && cd build-mkl/cpu/release \
 && ../../../configure $CONFIGURE_OPTS --with-mkl=/usr/local/CNTKCustomMKL \
 && make -j"$( nproc ;)" all
RUN cd Examples/Image/DataSets/CIFAR-10 \
 && python install_cifar10.py \
 && cd ../../../..
RUN cd Examples/Image/DataSets/MNIST \
 && python install_mnist.py \
 && cd ../../../..
ENV PATH="/cntk/build/cpu/release/bin:$PATH" \
    PYTHONPATH="/cntk/bindings/python" \
    LD_LIBRARY_PATH="/cntk/bindings/python/cntk/libs:$LD_LIBRARY_PATH"
#   Install kernel-runner scripts package (installed in base-python-minimal:3.6)
RUN pip install "backend.ai-kernel-runner[python]~=1.0.4" --no-cache-dir
COPY policy.yml /home/backend.ai/policy.yml
LABEL ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input"
CMD ["/home/backend.ai/jail", "-policy", "/home/backend.ai/policy.yml", "/usr/local/bin/python", "-m", "ai.backend.kernel", "cntk"]
#   vim: ft=dockerfile
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
