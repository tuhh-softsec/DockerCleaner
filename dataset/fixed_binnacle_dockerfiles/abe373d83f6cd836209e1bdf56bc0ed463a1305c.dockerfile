#   Don't edit this file directly, since it was generated from a template,
#   and you're changes will be *clobbered*.  Edit the template instead.
FROM tleyden5iwx/ubuntu-cuda
ENV PYTHONPATH="/opt/caffe/python"
#   Add caffe binaries to path
ENV PATH="$PATH:/opt/caffe/.build_release/tools"
#   Get dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bc=1.07.1-3build1 cmake=3.25.1-1 curl=7.88.1-7ubuntu1 gcc-4.6 g++-4.6 gcc-4.6-multilib g++-4.6-multilib gfortran=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 libprotobuf-dev=3.21.12-1ubuntu7 libleveldb-dev=1.23-4 libsnappy-dev=1.1.9-3 libopencv-dev=4.6.0+dfsg-11 libboost-all-dev=1.74.0.3ubuntu7 libhdf5-serial-dev liblmdb-dev=0.9.24-1build2 libjpeg62=1:6b2-3.1 libfreeimage-dev=3.18.0+ds2-9 libatlas-base-dev=3.10.3-13ubuntu1 pkgconf=1.8.1-1ubuntu2 protobuf-compiler=3.21.12-1ubuntu7 python-dev python-pip unzip=6.0-27ubuntu1 wget=1.21.3-1ubuntu1 -y
#   Use gcc 4.6
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/gcc-4.6 30 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++-4.6 30 \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 30 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.6 30
#   Allow it to find CUDA libs
RUN echo "/usr/local/cuda/lib64" > /etc/ld.so.conf.d/cuda.conf \
 && ldconfig
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
 && echo "CXX := /usr/bin/g++-4.6" >> Makefile.config \
 && sed -i 's/CXX :=/CXX ?=/' Makefile \
 && make all
#   Add ld-so.conf so it can find libcaffe.so
COPY caffe-ld-so.conf /etc/ld.so.conf.d/
#   Run ldconfig again (not sure if needed)
RUN ldconfig
#   Install python deps
RUN cd /opt/caffe \
 && cat python/requirements.txt | xargs -L 1 sudo pip install
#   Numpy include path hack - github.com/BVLC/caffe/wiki/Ubuntu-14.04-VirtualBox-VM
RUN ln -s /usr/include/python2.7/ /usr/local/include/python2.7 \
 && ln -s /usr/local/lib/python2.7/dist-packages/numpy/core/include/numpy/ /usr/local/include/python2.7/numpy
#   Build Caffe python bindings
RUN cd /opt/caffe \
 && make pycaffe
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
