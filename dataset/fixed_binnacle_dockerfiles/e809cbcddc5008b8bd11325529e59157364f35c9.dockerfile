FROM ubuntu:18.04
ARG BART_URL=https://github.com/mrirecon/bart
ARG BART_BRANCH=master
RUN apt-get update --quiet \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 apt-utils=1.6.14 wget=1.19.4-1ubuntu2.2 build-essential=12.4ubuntu1 cython=0.26.1-0.4 emacs=47.0 python-dev=2.7.15~rc1-1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-dev=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 libhdf5-serial-dev=1.10.0-patch1+docs-4 cmake=3.10.2-1ubuntu2.18.04.2 git-core libboost-all-dev=1.65.1.0ubuntu1 libfftw3-dev=3.3.7-1 h5utils=1.13-2 jq=1.5+dfsg-2 hdf5-tools=1.10.0-patch1+docs-4 liblapack-dev=3.7.1-4ubuntu1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libfreetype6-dev=2.8.1-2ubuntu2.2 pkg-config=0.29.1-0ubuntu2 libxslt-dev libarmadillo-dev=1:8.400.0+dfsg-2 libace-dev=6.4.5+dfsg-1build2 gcc-multilib=4:7.4.0-1ubuntu2.3 libgtest-dev=1.8.0-6 python3-dev=3.6.7-1~18.04 liblapack-dev=3.7.1-4ubuntu1 liblapacke-dev=3.7.1-4ubuntu1 libplplot-dev=5.13.0+dfsg-6ubuntu2 libdcmtk-dev=3.6.2-3build3 supervisor=3.3.1-1.1 cmake-curses-gui=3.10.2-1ubuntu2.18.04.2 neofetch=3.4.0-1 supervisor=3.3.1-1.1 net-tools=1.60+git20161116.90da8a0-1ubuntu1 cpio=2.12+dfsg-6ubuntu0.18.04.4 gpg-agent=2.2.4-1ubuntu1.6 --no-install-suggests --yes )
#   install cuda 9
RUN (apt-get update ;apt-get install --no-install-recommends linux-image-extra-virtual=4.15.0.208.191 --no-install-suggests --yes )
RUN (apt-get update ;apt-get install --no-install-recommends linux-source=4.15.0.208.191 --no-install-suggests --yes )
#   RUN apt-get source linux-image-$(uname -r)
RUN (apt-get update ;apt-get install --no-install-recommends linux-headers-$( uname -r ;) --no-install-suggests --yes )
#   RUN add-apt-repository ppa:graphics-drivers/ppa
#   RUN apt-get update --quiet && apt-get upgrade -y
#   RUN DEBIAN_FRONTEND=noninteractive apt install --no-install-recommends --no-install-suggests --yes nvidia-390
RUN mkdir /opt/code
RUN cd /opt/code
RUN wget https://developer.nvidia.com/compute/cuda/9.0/Prod/local_installers/cuda-repo-ubuntu1704-9-0-local_9.0.176-1_amd64-deb
RUN dpkg -i cuda-repo-ubuntu1704-9-0-local_9.0.176-1_amd64-deb
RUN apt-key add /var/cuda-repo-9-0-local/7fa2af80.pub
RUN :
RUN DEBIAN_FRONTEND=noninteractive apt-get install cuda --no-install-recommends --no-install-suggests --yes
RUN wget https://developer.nvidia.com/compute/cuda/9.0/Prod/patches/1/cuda-repo-ubuntu1704-9-0-local-cublas-performance-update_1.0-1_amd64-deb
RUN dpkg -i cuda-repo-ubuntu1704-9-0-local-cublas-performance-update_1.0-1_amd64-deb
RUN :
RUN DEBIAN_FRONTEND=noninteractive apt-get upgrade --yes cuda
RUN wget https://developer.nvidia.com/compute/cuda/9.0/Prod/patches/2/cuda-repo-ubuntu1704-9-0-local-cublas-performance-update-2_1.0-1_amd64-deb
RUN dpkg -i cuda-repo-ubuntu1704-9-0-local-cublas-performance-update-2_1.0-1_amd64-deb
RUN :
RUN DEBIAN_FRONTEND=noninteractive apt-get upgrade --yes cuda
RUN wget https://developer.nvidia.com/compute/cuda/9.0/Prod/patches/3/cuda-repo-ubuntu1704-9-0-local-cublas-performance-update-3_1.0-1_amd64-deb
RUN dpkg -i cuda-repo-ubuntu1704-9-0-local-cublas-performance-update-3_1.0-1_amd64-deb
RUN :
RUN DEBIAN_FRONTEND=noninteractive apt-get upgrade --yes cuda
COPY /docker/cudnn-9.0-linux-x64-v7.tgz /opt/code/
RUN cd /opt/code \
 && tar -xvf ./cudnn-9.0-linux-x64-v7.tgz \
 && cp cuda/include/cudnn.h /usr/local/cuda/include \
 && cp cuda/lib64/libcudnn* /usr/local/cuda/lib64 \
 && chmod a+r /usr/local/cuda/include/cudnn.h /usr/local/cuda/lib64/libcudnn*
RUN pip3 install --upgrade pip
RUN pip3 install -U pip setuptools
RUN (apt-get update ;apt-get install --no-install-recommends python3-psutil=5.4.2-1ubuntu0.1 python3-pyxb=1.2.6+dfsg-1 python3-lxml=4.2.1-1ubuntu0.6 --no-install-suggests --yes )
RUN (apt-get update ;apt-get install --no-install-recommends python3-pil=5.1.0-1ubuntu0.8 --no-install-suggests --yes )
RUN (apt-get update ;apt-get install --no-install-recommends python3-scipy=0.19.1-2ubuntu1 --no-install-suggests --yes )
RUN (apt-get update ;apt-get install --no-install-recommends python3-configargparse=0.11.0-1 --no-install-suggests --yes )
RUN pip3 install numpy==1.15.4 Cython tk-tools matplotlib scikit-image opencv_python pydicom scikit-learn
RUN pip3 uninstall h5py
RUN (apt-get update ;apt-get install --no-install-recommends python3-h5py=2.7.1-2 -y )
RUN pip3 install --upgrade tensorflow tensorflow-gpu
RUN pip3 install torch
RUN pip3 install torchvision
RUN pip3 install tensorboardx visdom
#   since cmake has problems to find python3 and boost-python3
#   RUN ln -s /usr/lib/x86_64-linux-gnu/libboost_python-py36.so /usr/lib/x86_64-linux-gnu/libboost_python3.so
#   fix the  qhull reentrant problem
#   RUN pip uninstall -y scipy
#  OpenBLAS with OpenMP
RUN cd /opt \
 && mkdir debsource \
 && cd debsource \
 && apt-get --no-install-recommends --no-install-suggests --yes build-dep libopenblas-base \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.4ubuntu1 fakeroot=1.22-2ubuntu1 devscripts=2.17.12ubuntu1.1 --no-install-suggests --yes ) \
 && apt-get source libopenblas-base \
 && cd openblas-0.2.20+ds/ \
 && sed -i "s/NO_WARMUP=1/NO_WARMUP=1 OPENMP=1/g" debian/rules \
 && debchange -i "Compiling with OpenMP support" \
 && debuild -us -uc -i -I \
 && debi \
 && cd /opt \
 && rm -rf debsource
#  Install Openblas
RUN rm /usr/lib/x86_64-linux-gnu/libblas.so
RUN rm /usr/lib/x86_64-linux-gnu/libblas.so.3
RUN ln -s /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3 /usr/lib/x86_64-linux-gnu/libblas.so
RUN ln -s /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3 /usr/lib/x86_64-linux-gnu/libblas.so.3
RUN rm /usr/lib/x86_64-linux-gnu/liblapack.so
RUN rm /usr/lib/x86_64-linux-gnu/liblapack.so.3
RUN ln -s /usr/lib/x86_64-linux-gnu/openblas/liblapack.so.3 /usr/lib/x86_64-linux-gnu/liblapack.so
RUN ln -s /usr/lib/x86_64-linux-gnu/openblas/liblapack.so.3 /usr/lib/x86_64-linux-gnu/liblapack.so.3
#  ZFP
RUN cd /opt \
 && git clone https://github.com/hansenms/ZFP.git \
 && cd ZFP \
 && mkdir lib \
 && make \
 && make shared \
 && make -j $( nproc ;) install
#   BART
RUN cd /opt/code \
 && git clone ${BART_URL} --branch ${BART_BRANCH} --single-branch \
 && cd bart \
 && mkdir build \
 && cd build \
 && cmake .. -DBART_FPIC=ON -DBART_ENABLE_MEM_CFL=ON -DBART_REDEFINE_PRINTF_FOR_TRACE=ON -DBART_LOG_BACKEND=ON -DBART_LOG_GADGETRON_BACKEND=ON \
 && make -j $( nproc ;) \
 && make install
#   ceres
RUN (apt-get update ;apt-get install --no-install-recommends libgoogle-glog-dev=0.3.5-1 libeigen3-dev=3.3.4-4 libsuitesparse-dev=1:5.1.2-2 --yes )
RUN cd /opt \
 && wget http://ceres-solver.org/ceres-solver-1.14.0.tar.gz \
 && tar zxf ceres-solver-1.14.0.tar.gz \
 && mkdir ceres-bin \
 && cd ceres-bin \
 && cmake ../ceres-solver-1.14.0 \
 && make -j20 \
 && make install
#  Set more environment variables in preparation for Gadgetron installation
ENV GADGETRON_HOME="/usr/local" \
    ISMRMRD_HOME="/usr/local"
ENV PATH="$PATH:/usr/local/cuda-9.0/bin;$GADGETRON_HOME/bin:$ISMRMRD_HOME/bin" \
    LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/cuda-9.0/lib64:$ISMRMRD_HOME/lib:$GADGETRON_HOME/lib"
ENV LIBRARY_PATH="/usr/local/cuda/lib64/stubs:${LIBRARY_PATH}"
#   Clean up packages.
#  RUN  apt-get clean && \
#     rm -rf /var/lib/apt/lists/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
