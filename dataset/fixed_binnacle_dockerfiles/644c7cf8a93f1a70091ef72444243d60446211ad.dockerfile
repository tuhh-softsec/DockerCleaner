FROM rsneddon/cudapythoncpu
#  FROM kaixhin/cuda-caffe
MAINTAINER R Sneddon <581894@bah.com>
#   install fast-rcnn's deps
#   Get dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bc=1.07.1-3build1 cmake=3.25.1-1 curl=7.88.1-7ubuntu1 gcc-4.6 g++-4.6 gcc-4.6-multilib g++-4.6-multilib gfortran=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 libprotobuf-dev=3.21.12-1ubuntu7 libleveldb-dev=1.23-4 libsnappy-dev=1.1.9-3 libopencv-dev=4.6.0+dfsg-11 libboost-all-dev=1.74.0.3ubuntu7 libhdf5-serial-dev liblmdb-dev=0.9.24-1build2 libjpeg62=1:6b2-3.1 libfreeimage-dev=3.18.0+ds2-9 libatlas-base-dev=3.10.3-13ubuntu1 pkgconf=1.8.1-1ubuntu2 protobuf-compiler=3.21.12-1ubuntu7 python-dev python-pip unzip=6.0-27ubuntu1 -y \
 && apt-get clean
#   Use gcc 4.6
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/gcc-4.6 30 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++-4.6 30 \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 30 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.6 30
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 python-numpy cython python-pip python-skimage python-protobuf python-opencv python-pandas python-yaml python-sklearn octave=7.3.0-1build1 python-ipdb -y --force-yes
RUN pip install easydict==1.10 --upgrade
#   octave is good enough for the PASCAL VOC stuff
RUN ln -s /usr/bin/octave /usr/bin/matlab
#   build fast-rcnn
RUN cd /opt \
 && git clone --recursive https://github.com/rbgirshick/fast-rcnn.git
RUN export CPLUS_INCLUDE_PATH=/opt/anaconda2/include \
 && export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/opt/anaconda2/include/python2.7 \
 && export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/opt/anaconda2/lib/python2.7/site-packages/numpy/core/include
COPY Makefile.config /opt/fast-rcnn/caffe-fast-rcnn/Makefile.config
#   Install Glog and Gflags 
RUN cd /opt \
 && wget --quiet --no-check-certificate http://pkgs.fedoraproject.org/lookaside/pkgs/glog/glog-0.3.3.tar.gz/a6fd2c22f8996846e34c763422717c18/glog-0.3.3.tar.gz \
 && tar zxvf glog-0.3.3.tar.gz \
 && cd glog-0.3.3 \
 && ./configure \
 && make -j$( nproc ;) \
 && make install -j$( nproc ;) \
 && cd .. \
 && rm -rf glog-0.3.3.tar.gz \
 && ldconfig \
 && cd /opt
RUN apt-get install --no-install-recommends unzip=6.0-27ubuntu1 -y
RUN wget --no-check-certificate --quiet https://github.com/schuhschuh/gflags/archive/master.zip \
 && unzip master.zip \
 && cd gflags-master \
 && mkdir build \
 && cd build \
 && export CXXFLAGS="-fPIC" \
 && cmake .. \
 && make VERBOSE=1 \
 && make -j$( nproc ;) \
 && make install -j$( nproc ;) \
 && cd ../.. \
 && rm master.zip
#   Install python dependencies
WORKDIR /opt
RUN /opt/anaconda2/bin/conda install --yes conda==3.10.1 \
 && conda install --yes cython \
 && conda install --yes opencv \
 && conda install --yes --channel https://conda.binstar.org/auto easydict
#   To remove erro when loading libreadline from anaconda
RUN rm /opt/anaconda2/lib/libreadline* \
 && ldconfig
WORKDIR /tmp
RUN wget --no-check-certificate https://sourceforge.net/projects/libpng/files/libpng15/older-releases/1.5.15/libpng-1.5.15.tar.gz
RUN tar -xvf libpng-1.5.15.tar.gz \
 && rm libpng-1.5.15.tar.gz
WORKDIR /tmp/libpng-1.5.15
RUN ./configure --prefix=/opt/libpng-1.5.15 \
 && make check -j$( nproc ;) \
 && make install -j$( nproc ;) \
 && make check -j$( nproc ;)
RUN cd /opt/fast-rcnn/lib \
 && make -j4
RUN cd /opt/fast-rcnn/caffe-fast-rcnn \
 && make -j4 \
 && make -j4 pycaffe
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
