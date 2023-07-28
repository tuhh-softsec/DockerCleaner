#   Dockerfile for CNTK-CPU-Infiniband-IntelMPI for use with Batch Shipyard on Azure Batch
FROM ubuntu:16.04
MAINTAINER Fred Park <https://github.com/Azure/batch-shipyard>
#   install base system
COPY ssh_config /root/.ssh/config
RUN apt-get update \
 && apt-get install --no-install-recommends autotools-dev=20150820.1 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 gfortran-multilib=4:5.3.1-1ubuntu1 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libjpeg-dev=8c-2ubuntu8 libpng-dev liblapacke-dev=3.6.0-2ubuntu2 libswscale-dev=7:2.8.17-0ubuntu0.1 libtiff-dev pkg-config=0.29.1-0ubuntu1 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 unzip=6.0-20ubuntu1.1 python-dev=2.7.12-1~16.04 automake=1:1.15-4ubuntu1 libtool=2.4.6-0.1 autoconf=2.69-9 subversion=1.9.3-2ubuntu1.3 libapr1=1.5.2-3 libaprutil1=1.5.4-1build1 libltdl-dev=2.4.6-0.1 libltdl7=2.4.6-0.1 libserf-1-1=1.3.8-1 libsigsegv2=2.10-4 libsvn1=1.9.3-2ubuntu1.3 m4=1.4.17-5 openjdk-9-jdk-headless=9~b114-0ubuntu1 libpcre3-dev=2:8.38-3.1 libpcre++-dev=0.9.5-6.1 -y \
 && apt-get install --no-install-recommends cpio=2.11+dfsg-5ubuntu1.1 libmlx4-1=1.0.6-1ubuntu3 libmlx5-1=1.0.2-1ubuntu2 librdmacm1=1.0.21-1 libibverbs1=1.1.8-1.1ubuntu2 libmthca1=1.0.6-1ubuntu1 libdapl2=2.1.5-1 dapl2-utils=2.1.5-1 openssh-server=1:7.2p2-4ubuntu2.10 openssh-client=1:7.2p2-4ubuntu2.10 -y \
 && rm -rf /var/lib/apt/lists/* \
 && mkdir /var/run/sshd \
 && ssh-keygen -A \
 && sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config \
 && sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd \
 && ssh-keygen -f /root/.ssh/id_rsa -t rsa -N '' \
 && chmod 600 /root/.ssh/config \
 && chmod 700 /root/.ssh \
 && cp /root/.ssh/id_rsa.pub /root/.ssh/authorized_keys
#   build and install libzip, boost, openblas, opencv, protobuf
RUN LIBZIP_VERSION=1.1.3 \
 && wget -q -O - http://nih.at/libzip/libzip-${LIBZIP_VERSION}.tar.gz | tar -xzf - \
 && cd libzip-${LIBZIP_VERSION} \
 && ./configure --prefix=/usr/local \
 && make -j"$( nproc ;)" install \
 && ldconfig /usr/local/lib \
 && cd .. \
 && rm -rf /libzip-${LIBZIP_VERSION} \
 && BOOST_VERSION=1_60_0 \
 && BOOST_DOTTED_VERSION=$( echo $BOOST_VERSION | tr _ . ;) \
 && wget -q -O - https://sourceforge.net/projects/boost/files/boost/${BOOST_DOTTED_VERSION}/boost_${BOOST_VERSION}.tar.gz/download | tar -xzf - \
 && cd boost_${BOOST_VERSION} \
 && ./bootstrap.sh --prefix=/usr/local --with-libraries=filesystem,system,test \
 && ./b2 -d0 -j"$( nproc ;)" install \
 && ldconfig /usr/local/lib \
 && cd .. \
 && rm -rf /boost_${BOOST_VERSION} \
 && OPENBLAS_VERSION=0.2.19 \
 && wget -q -O - https://github.com/xianyi/OpenBLAS/archive/v${OPENBLAS_VERSION}.tar.gz | tar -xzf - \
 && cd OpenBLAS-${OPENBLAS_VERSION} \
 && make -j"$( nproc ;)" USE_OPENMP=1 | tee make.log \
 && grep -qF 'OpenBLAS build complete. (BLAS CBLAS LAPACK LAPACKE)' make.log \
 && grep -qF 'Use OpenMP in the multithreading.' make.log \
 && make PREFIX=/usr/local/openblas install \
 && ldconfig /usr/local/openblas \
 && cd .. \
 && rm -rf /OpenBLAS-${OPENBLAS_VERSION} \
 && OPENCV_VERSION=3.1.0 \
 && wget -q -O - https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.tar.gz | tar -xzf - \
 && cd opencv-${OPENCV_VERSION} \
 && cmake -DWITH_CUDA=OFF -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=/usr/local/opencv-${OPENCV_VERSION} . \
 && make -j"$( nproc ;)" install \
 && ldconfig /usr/local/lib \
 && cd .. \
 && rm -rf /opencv-${OPENCV_VERSION} \
 && PROTOBUF_VERSION=3.1.0 PROTOBUF_STRING=protobuf-$PROTOBUF_VERSION \
 && wget -O - --no-verbose https://github.com/google/protobuf/archive/v${PROTOBUF_VERSION}.tar.gz | tar -xzf - \
 && cd $PROTOBUF_STRING \
 && ./autogen.sh \
 && ./configure CFLAGS=-fPIC CXXFLAGS=-fPIC --disable-shared --prefix=/usr/local/$PROTOBUF_STRING \
 && make -j $( nproc ;) install \
 && cd .. \
 && rm -rf $PROTOBUF_STRING
#   set env vars
ENV KALDI_VERSION="c024e8aa"
ENV PATH="/root/anaconda3/envs/cntk-py36/bin:/usr/local/bin:/cntk/build-mkl/cpu/release/bin:${PATH}" \
    KALDI_PATH="/usr/local/kaldi-$KALDI_VERSION" \
    BLAS="/usr/local/openblas/lib/libopenblas.so" \
    LAPACK="/usr/local/openblas/lib/libopenblas.so" \
    MKL_PATH="/usr/local/CNTKCustomMKL" \
    PYTHONPATH="/cntk/bindings/python:$PYTHONPATH" \
    LD_LIBRARY_PATH="/usr/local/openblas/lib:/cntk/bindings/python/cntk/libs:$LD_LIBRARY_PATH"
#   install cntk custom mkl, kaldi, swig and anaconda
RUN CNTK_CUSTOM_MKL_VERSION=3 \
 && mkdir ${MKL_PATH} \
 && wget --no-verbose -O - https://www.cntk.ai/mkl/CNTKCustomMKL-Linux-$CNTK_CUSTOM_MKL_VERSION.tgz | tar -xzf - -C ${MKL_PATH} \
 && mkdir $KALDI_PATH \
 && wget --no-verbose -O - https://github.com/kaldi-asr/kaldi/archive/$KALDI_VERSION.tar.gz | tar -xzf - --strip-components=1 -C $KALDI_PATH \
 && cd $KALDI_PATH/tools \
 && perl -pi -e 's/^# (OPENFST_VERSION = 1.4.1)$/\1/' Makefile \
 && make -j $( nproc ;) sph2pipe atlas sclite openfst \
 && cd ../src \
 && ./configure --openblas-root=/usr/local/openblas --shared \
 && make -j $( nproc ;) depend \
 && make -j $( nproc ;) all \
 && find $KALDI_PATH -name '*.o' -print0 | xargs -0 rm \
 && for dir in $KALDI_PATH/src/*bin; do make -C $dir clean ; done \
 && SWIG_VERSION=3.0.10 \
 && cd /root \
 && wget -q http://prdownloads.sourceforge.net/swig/swig-${SWIG_VERSION}.tar.gz -O - | tar xvfz - \
 && cd swig-${SWIG_VERSION} \
 && ./configure --without-alllang \
 && make -j$( nproc ;) \
 && make install \
 && cd .. \
 && rm -rf swig-${SWIG_VERSION} \
 && wget -q https://repo.continuum.io/archive/Anaconda3-4.4.0-Linux-x86_64.sh \
 && bash Anaconda3-4.4.0-Linux-x86_64.sh -b \
 && rm -f Anaconda3-4.4.0-Linux-x86_64.sh \
 && ldconfig /usr/local/lib
#   set cntk dir
WORKDIR /cntk
#   add intel mpi library and build cntk
ENV MANPATH="/usr/share/man:/usr/local/man" \
    COMPILERVARS_ARCHITECTURE="intel64" \
    COMPILERVARS_PLATFORM="linux" \
    INTEL_MPI_PATH="/opt/intel/compilers_and_libraries/linux/mpi"
COPY l_mpi_2017.2.174.tgz /tmp
COPY USE_SERVER.lic /tmp/l_mpi_2017.2.174/
RUN sed -i -e 's/^ACCEPT_EULA=decline/ACCEPT_EULA=accept/g' /tmp/l_mpi_2017.2.174/silent.cfg \
 && sed -i -e 's|^#ACTIVATION_LICENSE_FILE=|ACTIVATION_LICENSE_FILE=/tmp/l_mpi_2017.2.174/USE_SERVER.lic|g' /tmp/l_mpi_2017.2.174/silent.cfg \
 && sed -i -e 's/^ACTIVATION_TYPE=exist_lic/ACTIVATION_TYPE=license_server/g' /tmp/l_mpi_2017.2.174/silent.cfg \
 && cd /tmp/l_mpi_2017.2.174 \
 && ./install.sh -s silent.cfg \
 && cd .. \
 && rm -rf l_mpi_2017.2.174 \
 && ln -s ${INTEL_MPI_PATH}/${COMPILERVARS_ARCHITECTURE}/bin/mpicxx ${INTEL_MPI_PATH}/${COMPILERVARS_ARCHITECTURE}/bin/mpic++ \
 && CNTK_VERSION=v2.1 \
 && cd /cntk \
 && git clone --depth=1 --recursive -b ${CNTK_VERSION} --single-branch https://github.com/Microsoft/CNTK.git . \
 && /root/anaconda3/bin/conda env create -p /root/anaconda3/envs/cntk-py36/ --file /cntk/Scripts/install/linux/conda-linux-cntk-py36-environment.yml \
 && . /opt/intel/bin/compilervars.sh \
 && . /opt/intel/compilers_and_libraries/linux/mpi/bin64/mpivars.sh \
 && CONFIGURE_OPTS=" --1bitsgd=yes --with-mpi=${INTEL_MPI_PATH}/${COMPILERVARS_ARCHITECTURE} --with-kaldi=${KALDI_PATH} --with-py36-path=/root/anaconda3/envs/cntk-py36" \
 && mkdir -p build-mkl/cpu/release \
 && cd build-mkl/cpu/release \
 && ../../../configure $CONFIGURE_OPTS --with-mkl=${MKL_PATH} \
 && make -j"$( nproc ;)" all \
 && rm -rf /cntk/build-mkl/cpu/release/.build \
 && rm -rf /cntk/.git \
 && /root/anaconda3/bin/conda clean --all --yes \
 && echo "source /root/anaconda3/bin/activate /root/anaconda3/envs/cntk-py36" > /cntk/activate-cntk \
 && echo "source /cntk/activate-cntk" >> /root/.bashrc \
 && echo LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:'$LD_LIBRARY_PATH' >> /root/.bashrc \
 && rm -rf /opt/intel
#   set sshd command
EXPOSE 23/tcp
CMD ["/usr/sbin/sshd", "-D", "-p", "23"]
#   copy in intel mpirun helper script on Batch
COPY run_cntk.sh /cntk/
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
