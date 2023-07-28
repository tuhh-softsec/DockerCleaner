FROM nvidia/cuda:7.5-cudnn5-devel
#   Get dependencies
#   Update gcc for dense inference wrapper (CRF)
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common -y
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update \
 && apt-get install --no-install-recommends bc cmake curl gcc-4.9 g++-4.9 gcc-4.9-multilib g++-4.9-multilib gfortran git libprotobuf-dev libleveldb-dev libsnappy-dev libopencv-dev libboost-all-dev libhdf5-serial-dev liblmdb-dev libjpeg62 libfreeimage-dev libatlas-base-dev libgflags-dev libgoogle-glog-dev pkgconf protobuf-compiler python-dev python-pip python-opencv python-numpy unzip wget vim htop sshfs cifs-utils -y
#   Use gcc 4.9
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/gcc-4.9 30 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++-4.9 30 \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.9 30 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 30
#   Allow it to find CUDA libs
RUN echo "/usr/local/cuda/lib64" > /etc/ld.so.conf.d/cuda.conf \
 && ldconfig
#   Clone Faustos 3D CAFFE
RUN cd /opt \
 && git clone https://github.com/faustomilletari/3D-Caffe.git caffe \
 && cd /opt/caffe
#  RUN cd /opt && git clone https://github.com/BVLC/caffe.git caffe && cd /opt/caffe
#  Clone Faustos VNET Project
RUN cd /opt/ \
 && git clone https://github.com/patrickchrist/VNet.git vnet
#  Add deployment ssh key
COPY repo_key /
RUN chmod 600 /repo_key
RUN echo "IdentityFile /repo_key" >> /etc/ssh/ssh_config
RUN echo "StrictHostKeyChecking no" >> /etc/ssh/ssh_config
#   Clone and Install 3D DenseCRF
RUN cd /opt \
 && git clone -b master git@bitbucket.org:ibbm/denseinferencewrapper.git denseinferencewrapper
#   Build and install 3D DesneCRF
RUN cd /opt/denseinferencewrapper \
 && make \
 && pip install .
#   Clone Saratan project
RUN cd /opt/ \
 && git clone git@bitbucket.org:patrickchrist/saratan.git
ENV PYTHONPATH="/opt/caffe/python:/opt/saratan/:/opt/saratan/data/layers:/opt/vnet"
#   Install caffe python dependencies
RUN cd /opt/caffe/python \
 && for req in $( cat requirements.txt ;); do sudo pip install $req ; done
#   Instal pip packages used by various scripts, and useful for interactive dev
RUN sudo pip install pydicom lmdb jupyter plyvel peewee nibabel tqdm pypng natsort medpy psutil
RUN pip install www.simpleitk.org==null 30==null SimpleITK==2.2.1 --allow-insecure -f http://www.simpleitk.org/SimpleITK/resources/software.html --timeout
#   Install NLopt
RUN cd /opt/ \
 && wget http://ab-initio.mit.edu/nlopt/nlopt-2.4.2.tar.gz \
 && tar -xvf nlopt-2.4.2.tar.gz \
 && rm nlopt-2.4.2.tar.gz
RUN cd /opt/nlopt-2.4.2 \
 && ./configure --enable-shared \
 && make -j$( nproc ;) \
 && make install
RUN echo "/opt/nlopt-2.4.2/.libs/" > /etc/ld.so.conf.d/nlopt.conf \
 && ldconfig
#   Build Caffe
RUN cd /opt/caffe \
 && cp Makefile.config.example Makefile.config \
 && echo "CXX := /usr/bin/g++-4.9" >> Makefile.config \
 && sed -i 's/CXX :=/CXX ?=/' Makefile \
 && sed -i 's/# WITH_PYTHON_LAYER/WITH_PYTHON_LAYER/' Makefile.config \
 && sed -i 's/# USE_CUDNN/USE_CUDNN/' Makefile.config \
 && make all -j$( nproc ;) \
 && make pycaffe -j$( nproc ;)
#   Install caffe
ENV PATH="$PATH:/opt/caffe/.build_release/tools"
#  Upgrade Numpy
RUN sudo pip install numpy --upgrade
EXPOSE 8888/tcp 8889/tcp 8890/tcp 8891/tcp 8892/tcp 8893/tcp 8894/tcp 8895/tcp 8896/tcp 8897/tcp 8898/tcp 8899/tcp 8900/tcp
#   For some reason, at this point git doesn't work giving the following error:
#   git-remote-https: symbol lookup error: /usr/lib/x86_64-linux-gnu/libasn1.so.8: undefined symbol: _et_list
#   so I run "sudo ldconfig" and it fixes it
RUN sudo ldconfig
RUN cd /opt/saratan \
 && git pull
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
