#
#   Yahoo! Open NSFW Web Service
#   Not Suitable for Work (NSFW) classification using deep neural network Caffe models
#   Adapted from https://github.com/loretoparisi/nsfwaas
#   @see https://github.com/loretoparisi/nsfwaas
#   @see https://github.com/yahoo/open_nsfw
#
#   Copyright (c) 2018 Loreto Parisi - https://github.com/loretoparisi/docker
#
FROM ubuntu:16.04
#   To build for GPU
#   FROM nvidia/cuda:8.0-cudnn6-devel-ubuntu16.04
MAINTAINER Loreto Parisi loretoparisi@gmail.com
#   Based on CAFFE's CPU container
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 libatlas-base-dev=3.10.2-9 libboost-all-dev=1.58.0.1ubuntu1 libgflags-dev=2.1.2-3 libgoogle-glog-dev=0.3.4-0.1 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libleveldb-dev=1.18-5 liblmdb-dev=0.9.17-3 libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 libprotobuf-dev=2.6.1-1.3 libsnappy-dev=1.1.3-2 protobuf-compiler=2.6.1-1.3 python-dev=2.7.12-1~16.04 python-setuptools=20.7.0-1 python-numpy=1:1.11.0-1ubuntu1 python-pip=8.1.1-2ubuntu0.6 python-scipy=0.17.0-1 cython=0.23.4-0ubuntu5 python-skimage=0.10.1-2build1 python-matplotlib=1.5.1-1ubuntu1 ipython=2.4.1-1 python-h5py=2.6.0-1 python-leveldb=0~svn68-2build4 python-networkx=1.11-1ubuntu1 python-nose=1.3.7-1 python-pandas=0.17.1-3ubuntu2 python-dateutil=2.4.2-1 python-protobuf=2.6.1-1.3 python-gflags=1.5.1-2 python-yaml=3.11-3build1 python-pil=3.1.2-0ubuntu1.6 python-six=1.10.0-3 python-flask=0.10.1-2ubuntu0.1 python-tornado=4.2.1-1ubuntu3.1 apache2=2.4.18-2ubuntu3.17 libapache2-mod-wsgi=4.3.0-1.1ubuntu1 supervisor=3.2.0-2ubuntu0.2 -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Build Caffe
ENV CAFFE_ROOT="/opt/caffe"
WORKDIR $CAFFE_ROOT
#   FIXME: clone a specific git tag and use ARG instead of ENV once DockerHub supports this.
ENV CLONE_TAG="master"
RUN pip install pip==23.1 --upgrade
RUN git clone -b ${CLONE_TAG} --depth 1 https://github.com/BVLC/caffe.git . \
 && for req in $( cat python/requirements.txt ;) pydot; do pip install $req ; done \
 && mkdir build \
 && cd build \
 && cmake -DCPU_ONLY=1 .. \
 && make -j"$( nproc ;)"
ENV PYCAFFE_ROOT="$CAFFE_ROOT/python"
ENV PYTHONPATH="$PYCAFFE_ROOT:$PYTHONPATH"
ENV PATH="$CAFFE_ROOT/build/tools:$PYCAFFE_ROOT:$PATH"
RUN echo "$CAFFE_ROOT/build/lib" >> /etc/ld.so.conf.d/caffe.conf \
 && ldconfig
#   Web Server config
ARG PORT
ARG DIST
ENV PORT="$PORT"
ENV DIST="$DIST"
WORKDIR /workspace
#   Copy the model and deploy the app server
#  RUN git clone https://github.com/yahoo/open_nsfw.git
COPY open_nsfw /workspace/open_nsfw
COPY nsfwnet.py /workspace/
COPY nsfwaas.py /workspace/
#   Set up Apache
RUN a2enmod wsgi
RUN a2dissite 000*
#   copy virtual host and replace port
COPY nsfwaas.conf /workspace/nsfwaas.conf
RUN sed "s/$PORT/$PORT/g" /workspace/nsfwaas.conf > /etc/apache2/sites-available/nsfwaas.conf
RUN sed -i "s/80/$PORT/g" /etc/apache2/ports.conf
COPY config.py /workspace/
RUN a2ensite nsfwaas
#   Configure supervisord
RUN mkdir -p /var/lock/apache2 /var/run/apache2 /var/log/supervisor
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   Expose the webserver
EXPOSE $PORT
#   Start supervisord
CMD ["/usr/bin/supervisord"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
