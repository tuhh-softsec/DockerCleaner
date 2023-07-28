FROM ubuntu:16.04
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 libatlas-base-dev=3.10.2-9 libboost-all-dev=1.58.0.1ubuntu1 libgflags-dev=2.1.2-3 libgoogle-glog-dev=0.3.4-0.1 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libleveldb-dev=1.18-5 liblmdb-dev=0.9.17-3 libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 libprotobuf-dev=2.6.1-1.3 libsnappy-dev=1.1.3-2 locales=2.23-0ubuntu11.3 netcat=1.10-41 protobuf-compiler=2.6.1-1.3 python3-dev=3.5.1-3 python3-numpy=1:1.11.0-1ubuntu1 python3-pip=8.1.1-2ubuntu0.6 python3-pydot=1.0.28-2 python3-scipy=0.17.0-1 python3-setuptools=20.7.0-1 sudo=1.8.16-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN pip3 install pip==9.0.3 \
 && pip3 install -U cython h5py ipython jupyter leveldb lmdb matplotlib networkx nose numpy pandas Pillow protobuf pydot python-dateutil python-gflags pyyaml scikit-image scipy six \
 && rm -rf ~/.cache/pip
ENV CAFFE_ROOT="/opt/caffe"
WORKDIR $CAFFE_ROOT
ENV CLONE_TAG="1.0"
RUN git clone -b ${CLONE_TAG} --depth 1 https://github.com/BVLC/caffe.git . \
 && mkdir build \
 && cd build \
 && cmake -DCPU_ONLY=1 -Dpython_version=3 .. \
 && make -j$( nproc ;)
ENV PYCAFFE_ROOT="$CAFFE_ROOT/python"
ENV PYTHONPATH="$PYCAFFE_ROOT:$PYTHONPATH"
ENV PATH="$CAFFE_ROOT/build/tools:$PYCAFFE_ROOT:$PATH"
RUN echo '$CAFFE_ROOT/build/lib' >> /etc/ld.so.conf.d/caffe.conf \
 && ldconfig
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen
#   Install Tini
RUN wget --quiet https://github.com/krallin/tini/releases/download/v0.10.0/tini \
 && echo "1361527f39190a7338a0b434bd8c88ff7233ce7b9a4876f3315c22fce7eca1b0 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini
#   Configure environment
ENV SHELL="/bin/bash" \
    NB_USER="aurora" \
    NB_UID="1000" \
    NB_GID="2000" \
    LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8"
#   Create aurora user with UID=1000 and in the 'aurora' group
RUN groupadd -g $NB_GID $NB_USER \
 && useradd -m -s $SHELL -N -u $NB_UID -g $NB_GID $NB_USER \
 && echo '%'$NB_USER 'ALL=(ALL:ALL) NOPASSWD:ALL' >> /etc/sudoers
EXPOSE 8888/tcp
WORKDIR /workspace
#   Add local files as late as possible to avoid cache busting
COPY start.sh /usr/local/bin/
COPY start-notebook.sh /usr/local/bin/
COPY jupyter_notebook_config.py /etc/jupyter/
#   Install Aurora job submit tool
ARG CACHE_DATE
ARG SUBMIT_TOOL_NAME=aurora
RUN wget https://raw.githubusercontent.com/linkernetworks/aurora/master/install.sh -O - | bash \
 && if [ "$SUBMIT_TOOL_NAME" != "aurora" ] ; then mv /usr/local/bin/aurora /usr/local/bin/$SUBMIT_TOOL_NAME ; fi
#   Configure container startup
ENTRYPOINT ["tini", "--"]
CMD ["start-notebook.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
