FROM nvidia/cuda:7.0-cudnn3-devel
#   Get dependencies
#   Update gcc for dense inference wrapper (CRF)
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common -y
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-get update \
 && apt-get install --no-install-recommends bc cmake curl gcc-4.9 g++-4.9 gcc-4.9-multilib g++-4.9-multilib gfortran git libprotobuf-dev libleveldb-dev libsnappy-dev libopencv-dev libboost-all-dev libhdf5-serial-dev liblmdb-dev libjpeg62 libfreeimage-dev libatlas-base-dev libgflags-dev libgoogle-glog-dev pkgconf protobuf-compiler python-dev python-pip python-opencv python-numpy unzip wget vim htop sshfs tmux graphviz cifs-utils -y
#   Use gcc 4.9
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/gcc-4.9 30 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++-4.9 30 \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.9 30 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 30
#   Allow it to find CUDA libs
RUN echo "/usr/local/cuda/lib64" > /etc/ld.so.conf.d/cuda.conf \
 && ldconfig
#   Clone my own branch with the CLASS WEIGHTING feature + Weigh_prediction_class option
#  RUN cd /opt && git clone -b master https://github.com/mohamed-ezz/caffe.git caffe && cd /opt/caffe
#   Clone my own branch with the CLASS WEIGHTING feature + Weigh_prediction_class option
RUN cd /opt \
 && git clone -b jonlong https://github.com/mohamed-ezz/caffe.git caffe \
 && cd /opt/caffe
#   Clone and Install 3D DenseCRF
RUN cd /opt \
 && git clone -b master https://github.com/mbickel/DenseInferenceWrapper.git denseinferencewrapper
#   Build and install 3D DesneCRF
RUN cd /opt/denseinferencewrapper \
 && make \
 && pip install .
#   Clone Saratan project
RUN cd /opt/ \
 && git clone https://github.com/mohamed-ezz/saratan.git
ENV PYTHONPATH="/opt/caffe/python:/opt/saratan/:/opt/saratan/data/layers"
#   Install caffe python dependencies
RUN cd /opt/caffe/python \
 && for req in $( cat requirements.txt ;); do sudo pip install $req ; done
#   Instal pip packages used by various scripts, and useful for interactive dev
RUN sudo pip install pydicom lmdb jupyter plyvel peewee nibabel tqdm pypng natsort medpy psutil pydot
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
EXPOSE 8888-8900
#   For some reason, at this point git doesn't work giving the following error:
#   git-remote-https: symbol lookup error: /usr/lib/x86_64-linux-gnu/libasn1.so.8: undefined symbol: _et_list
#   so I run "sudo ldconfig" and it fixes it
RUN sudo ldconfig
RUN cd /opt/saratan \
 && git pull
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
