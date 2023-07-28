FROM ubuntu:14.04
#   [ tensorflow ]
#   https://github.com/tensorflow/tensorflow/blob/master/tensorflow/tools/docker/Dockerfile
RUN apt-get update ; apt-get install --no-install-recommends build-essential=11.6ubuntu6 curl=7.35.0-1ubuntu2.20 libfreetype6-dev=2.5.2-1ubuntu2.8 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 pkg-config=0.26-1ubuntu4 python=2.7.5-5ubuntu3 python-dev=2.7.5-5ubuntu3 rsync=3.1.0-2ubuntu0.4 software-properties-common=0.92.37.8 unzip=6.0-9ubuntu1.5 -y ; apt-get clean autoclean ; apt-get autoremove -y ; rm -rf /var/lib/apt/lists/*
RUN curl -O https://bootstrap.pypa.io/get-pip.py ; python get-pip.py ; rm get-pip.py
RUN pip install ipykernel==6.22.0 jupyter==1.0.0 matplotlib==3.7.1 numpy==1.24.2 scipy==1.10.1 sklearn==0.0.post4 pandas==2.0.0 Pillow==9.5.0 tensorflow==2.12.0 --no-cache-dir ; python -m ipykernel.kernelspec
#   TensorBoard
EXPOSE 6006/tcp
#   IPython
EXPOSE 8888/tcp
#   [ pandas ]
RUN pip install pandas==2.0.0 --no-cache-dir
#   [ keras + theano ]
#   https://github.com/fchollet/keras/blob/master/docker/Dockerfile
RUN apt-get update ; apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 git=1:1.9.1-1ubuntu0.10 libhdf5-dev=1.8.11-5ubuntu7.1 g++=4:4.8.2-1ubuntu6 graphviz=2.36.0-0ubuntu3.2 -y ; apt-get clean autoclean ; apt-get autoremove -y ; rm -rf /var/lib/apt/lists/*
RUN pip install h5py==3.8.0 git+git://github.com/fchollet/keras.git --no-cache-dir
#   [ gensim ]
RUN pip install gensim==4.3.1 --no-cache-dir
#   [ torch ]
#   https://github.com/Kaixhin/dockerfiles/blob/master/torch/Dockerfile
#   Install git, apt-add-repository and dependencies for iTorch
RUN apt-get update ; apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 software-properties-common=0.92.37.8 ipython3=1.2.1-2 libssl-dev=1.0.1f-1ubuntu2.27 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 python-zmq=14.0.1-1build2 python-pip=1.5.4-1ubuntu4 -y ; apt-get clean autoclean ; apt-get autoremove -y ; rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/torch/distro.git /root/torch --recursive ; cd /root/torch ; bash install-deps ; ./install.sh -b
ENV LUA_PATH="/root/.luarocks/share/lua/5.1/?.lua;/root/.luarocks/share/lua/5.1/?/init.lua;/root/torch/install/share/lua/5.1/?.lua;/root/torch/install/share/lua/5.1/?/init.lua;./?.lua;/root/torch/install/share/luajit-2.1.0-beta1/?.lua;/usr/local/share/lua/5.1/?.lua;/usr/local/share/lua/5.1/?/init.lua"
ENV LUA_CPATH="/root/.luarocks/lib/lua/5.1/?.so;/root/torch/install/lib/lua/5.1/?.so;./?.so;/usr/local/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/loadall.so"
ENV PATH="/root/torch/install/bin:$PATH"
ENV LD_LIBRARY_PATH="/root/torch/install/lib:$LD_LIBRARY_PATH"
ENV DYLD_LIBRARY_PATH="/root/torch/install/lib:$DYLD_LIBRARY_PATH"
ENV LUA_CPATH="/root/torch/install/lib/?.so;$LUA_CPATH"
RUN git clone https://github.com/facebook/iTorch.git ; cd iTorch ; luarocks make ; cd .. ; rm -rf iTorch
#   [ pyOSC ]
#   this version is more up to date than pip
RUN pip install git+git://github.com/ptone/pyosc.git --no-cache-dir
#   [ dlib ]
RUN apt-get update ; apt-get install --no-install-recommends libopenblas-dev=0.2.8-6ubuntu1 libboost-python-dev=1.54.0.1ubuntu1 liblapack-dev=3.5.0-2ubuntu1 -y ; git clone https://github.com/davisking/dlib.git /root/dlib ; cd /root/dlib ; mkdir build ; cd build ; cmake .. ; cmake --build . ; cd /root/dlib ; python setup.py install ; cd /root ; rm -rf dlib ; apt-get clean autoclean ; apt-get autoremove -y ; rm -rf /var/lib/apt/lists/*
#   [ Multicore-TSNE ]
RUN git clone https://github.com/DmitryUlyanov/Multicore-TSNE.git ; cd Multicore-TSNE ; pip install --no-cache-dir -r requirements.txt ; python setup.py install ; cd .. ; rm -rf Multicore-TSNE
RUN pip install scikit-image==0.20.0 --no-cache-dir
#   [ lapjv ]
RUN pip install cython==0.29.34 git+git://github.com/gatagat/lapjv.git --no-cache-dir
#   [ torch-rnn ]
RUN git clone https://github.com/deepmind/torch-hdf5 ; cd torch-hdf5 ; luarocks make hdf5-0-0.rockspec ; cd .. ; rm -rf torch-hdf5
#   [ neuraltalk2 ]
RUN apt-get update ; apt-get install --no-install-recommends libprotobuf-dev=2.5.0-9ubuntu1 protobuf-compiler=2.5.0-9ubuntu1 -y ; luarocks install loadcaffe
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
