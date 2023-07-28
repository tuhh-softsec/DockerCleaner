FROM ubuntu:14.04
ENV PYTHONPATH="/opt/caffe/python"
#   Add caffe binaries to path
ENV PATH="$PATH:/opt/caffe/.build_release/tools"
#   Get dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bc=1.06.95-8ubuntu1 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 gcc-4.6=4.6.4-6ubuntu2 g++-4.6=4.6.4-6ubuntu2 gcc-4.6-multilib=4.6.4-6ubuntu2 g++-4.6-multilib=4.6.4-6ubuntu2 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 libprotobuf-dev=2.5.0-9ubuntu1 libleveldb-dev=1.15.0-2 libsnappy-dev=1.1.0-1ubuntu1 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 libboost-all-dev=1.54.0.1ubuntu1 libhdf5-serial-dev=1.8.11-5ubuntu7.1 liblmdb-dev=0.9.10-1 libjpeg62=6b1-4ubuntu1 libfreeimage-dev=3.15.4-3ubuntu0.1 libatlas-base-dev=3.10.1-4 pkgconf=0.9.4-1 protobuf-compiler=2.5.0-9ubuntu1 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 unzip=6.0-9ubuntu1.5 wget=1.15-1ubuntu1.14.04.5 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 python-pandas=0.13.1-2ubuntu2 python-sympy=0.7.4.1-1 python-nose=1.3.1-2 -y
#   Use gcc 4.6
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/gcc-4.6 30 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++-4.6 30 \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 30 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.6 30
#   Clone the Caffe repo 
RUN cd /opt \
 && git clone https://github.com/BVLC/caffe.git
#   Glog 
RUN cd /opt \
 && wget https://google-glog.googlecode.com/files/glog-0.3.3.tar.gz \
 && tar zxvf glog-0.3.3.tar.gz \
 && cd /opt/glog-0.3.3 \
 && ./configure \
 && make \
 && make install
#   Workaround for error loading libglog: 
#     error while loading shared libraries: libglog.so.0: cannot open shared object file
#   The system already has /usr/local/lib listed in /etc/ld.so.conf.d/libc.conf, so
#   running `ldconfig` fixes the problem (which is simpler than using $LD_LIBRARY_PATH)
#   TODO: looks like this needs to be run _every_ time a new docker instance is run,
#         so maybe LD_LIBRARY_PATh is a better approach (or add call to ldconfig in ~/.bashrc)
RUN ldconfig
#   Gflags
RUN cd /opt \
 && wget https://github.com/schuhschuh/gflags/archive/master.zip \
 && unzip master.zip \
 && cd /opt/gflags-master \
 && mkdir build \
 && cd /opt/gflags-master/build \
 && export CXXFLAGS="-fPIC" \
 && cmake .. \
 && make VERBOSE=1 \
 && make \
 && make install
#   Build Caffe core
RUN cd /opt/caffe \
 && cp Makefile.config.example Makefile.config \
 && echo "CPU_ONLY := 1" >> Makefile.config \
 && echo "CXX := /usr/bin/g++-4.6" >> Makefile.config \
 && sed -i 's/CXX :=/CXX ?=/' Makefile \
 && make all
#   Add ld-so.conf so it can find libcaffe.so
#  ADD caffe-ld-so.conf /etc/ld.so.conf.d/
#   Run ldconfig again (not sure if needed)
RUN ldconfig
#   Install python deps
RUN cd /opt/caffe \
 && (pip install -r python/requirements.txt )
#   Numpy include path hack - github.com/BVLC/caffe/wiki/Setting-up-Caffe-on-Ubuntu-14.04
#  RUN NUMPY_EGG=`ls /usr/local/lib/python2.7/dist-packages | grep -i numpy` && \
#    ln -s /usr/local/lib/python2.7/dist-packages/$NUMPY_EGG/numpy/core/include/numpy /usr/include/python2.7/numpy
#   Build Caffe python bindings
RUN cd /opt/caffe \
 && make pycaffe
#   Make + run tests
RUN cd /opt/caffe \
 && make test \
 && make runtest
#  Download GoogLeNet
RUN /opt/caffe/scripts/download_model_binary.py /opt/caffe/models/bvlc_googlenet
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
