FROM ubuntu:14.04
MAINTAINER caffe-dev <caffe-dev@googlegroups.com>
#   A docker container with CUDA and caffe2 installed.
#   Note: this should install everything but cudnn, which requires you to have a
#   manual registration and download from the NVidia website. After creating this
#   docker image, the Caffe2 repository is located at /opt/caffe2. You can install
#   cudnn manually and re-compile caffe2.
#  ###############################################################################
#   Step 1: set up cuda on the ubuntu box.
#  ###############################################################################
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=11.6ubuntu6 wget=1.15-1ubuntu1.14.04.5 -q -y
RUN cd /tmp \
 && wget http://developer.download.nvidia.com/compute/cuda/7_0/Prod/local_installers/cuda_7.0.28_linux.run \
 && chmod +x cuda_*_linux.run \
 && ./cuda_*_linux.run -extract=`pwd ` \
 && ./NVIDIA-Linux-x86_64-*.run -s --no-kernel-module \
 && ./cuda-linux64-rel-*.run -noprompt \
 && rm -rf *
#   Ensure the CUDA libs and binaries are in the correct environment variables
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda/lib64"
ENV PATH="$PATH:/usr/local/cuda/bin"
#   Run nvcc to make sure things are set correctly.
RUN nvcc --version
#  ###############################################################################
#   Step 2: set up caffe2 pre-requisites
#  ###############################################################################
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 libeigen3-dev=3.2.0-8 libgoogle-glog-dev=0.3.3-1 libleveldb-dev=1.15.0-2 liblmdb-dev=0.9.10-1 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 libprotobuf-dev=2.5.0-9ubuntu1 libsnappy-dev=1.1.0-1ubuntu1 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 libbz2-dev=1.0.6-5 protobuf-compiler=2.5.0-9ubuntu1 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 -q -y
RUN cd /tmp \
 && git clone https://github.com/facebook/rocksdb.git \
 && cd /tmp/rocksdb \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/rocksdb
#   Caffe2 works best with openmpi 1.8.5 or above (which has cuda support).
#   If you do not need openmpi, skip this step.
RUN cd /tmp \
 && wget http://www.open-mpi.org/software/ompi/v1.10/downloads/openmpi-1.10.0.tar.gz \
 && tar xzvf openmpi-1.10.0.tar.gz \
 && cd /tmp/openmpi-1.10.0 \
 && ./configure --with-cuda --with-threads \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/openmpi-1.10.0 \
 && rm /tmp/openmpi-1.10.0.tar.gz
#   Caffe2 requires zeromq 4.0 or above, manually install.
#   If you do not need zeromq, skip this step.
RUN apt-get install --no-install-recommends autoconf=2.69-6 libtool=2.4.2-1.7ubuntu1 -q -y
RUN mkdir /tmp/zeromq-build \
 && cd /tmp/zeromq-build \
 && wget https://github.com/zeromq/zeromq4-1/archive/v4.1.3.tar.gz \
 && tar xzvf v4.1.3.tar.gz --strip 1 \
 && ./autogen.sh \
 && ./configure --without-libsodium \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/zeromq-build
#   pip self upgrade
RUN pip install pip==23.1 --upgrade
#   Python dependencies
RUN pip install matplotlib==3.7.1 numpy==1.24.2 protobuf==4.22.3
#  ###############################################################################
#   Step 3: install optional dependencies ("good to have" features)
#  ###############################################################################
RUN apt-get install --no-install-recommends gfortran=4:4.8.2-1ubuntu6 graphviz=2.36.0-0ubuntu3.2 libatlas-base-dev=3.10.1-4 vim=2:7.4.052-1ubuntu3.1 -q -y
RUN pip install flask==2.2.3 ipython==8.12.0 notebook==6.5.4 pydot==1.4.2 python-nvd3==0.15.0 scipy==1.10.1 tornado==6.2
#   This is intentional. scikit-image has to be after scipy.
RUN pip install scikit-image==0.20.0
#  ###############################################################################
#   Step 4: set up caffe2
#  ###############################################################################
#   Get the repository, and build.
RUN cd /opt \
 && git clone https://github.com/Yangqing/caffe2.git \
 && cd /opt/caffe2 \
 && make
#   Now, we know that some of the caffe tests will fail. How do we deal with
#   those?
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
