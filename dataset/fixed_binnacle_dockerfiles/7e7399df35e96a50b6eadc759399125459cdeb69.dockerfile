FROM ubuntu:14.04
MAINTAINER lyysdy@foxmail.com
USER root
#   install dev tools
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 tar=1.27.1-1ubuntu0.1 g++-4.8=4.8.4-2ubuntu1~14.04.4 gcc=4:4.8.2-1ubuntu6 libtool=2.4.2-1.7ubuntu1 pkg-config=0.26-1ubuntu4 autoconf=2.69-6 openssh-server=1:6.6p1-2ubuntu2.13 openssh-client=1:6.6p1-2ubuntu2.13 rsync=3.1.0-2ubuntu0.4 build-essential=11.6ubuntu6 automake=1:1.14.1-2ubuntu1 vim=2:7.4.052-1ubuntu3.1 gdb=7.7.1-0ubuntu5~14.04.3 git=1:1.9.1-1ubuntu0.10 libopenmpi-dev=1.6.5-8 openmpi-bin=1.6.5-8 cmake=2.8.12.2-0ubuntu3 gfortran=4:4.8.2-1ubuntu6 python-nose=1.3.1-2 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 python-dev=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 libopenblas-dev=0.2.8-6ubuntu1 software-properties-common=0.92.37.8 libssl-dev=1.0.1f-1ubuntu2.27 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 python-zmq=14.0.1-1build2 -qqy
#   java
RUN mkdir -p /usr/local/java/default \
 && curl -Ls 'http://download.oracle.com/otn-pub/java/jdk/8u65-b17/jdk-8u65-linux-x64.tar.gz' -H 'Cookie: oraclelicense=accept-securebackup-cookie' | tar --strip-components=1 -xz -C /usr/local/java/default/
ENV JAVA_HOME="/usr/local/java/default/ "
ENV PATH="$PATH:$JAVA_HOME/bin"
#   hadoop
RUN wget -cq -t 0 http://www.eu.apache.org/dist/hadoop/common/hadoop-2.6.0/hadoop-2.6.0.tar.gz
RUN tar -xz -C /usr/local/ -f hadoop-2.6.0.tar.gz \
 && rm hadoop-2.6.0.tar.gz \
 && cd /usr/local \
 && ln -s ./hadoop-2.6.0 hadoop \
 && cp -r /usr/local/hadoop/include/* /usr/local/include
ENV HADOOP_PREFIX="/usr/local/hadoop"
RUN sed -i '/^export JAVA_HOME/ s:.*:export JAVA_HOME=/usr/local/java/default\nexport HADOOP_PREFIX=/usr/local/hadoop\nexport HADOOP_HOME=/usr/local/hadoop\n:' $HADOOP_PREFIX/etc/hadoop/hadoop-env.sh
RUN sed -i '/^export HADOOP_CONF_DIR/ s:.*:export HADOOP_CONF_DIR=/usr/local/hadoop/etc/hadoop/:' $HADOOP_PREFIX/etc/hadoop/hadoop-env.sh
#   fixing the libhadoop.so like a boss
RUN rm /usr/local/hadoop/lib/native/* \
 && curl -Ls http://dl.bintray.com/sequenceiq/sequenceiq-bin/hadoop-native-64-2.6.0.tar | tar -x -C /usr/local/hadoop/lib/native/
#   install Theano-dev
RUN mkdir -p /theano \
 && cd /theano \
 && git clone git://github.com/Theano/Theano.git \
 && cd /theano/Theano \
 && python setup.py develop
#   Install Jupyter Notebook for iTorch
RUN pip install notebook==6.5.4 ipywidgets==8.0.6
#   Run Torch7 installation scripts
RUN git clone https://github.com/torch/distro.git /root/torch --recursive \
 && cd /root/torch \
 && bash install-deps \
 && ./install.sh
#   Export environment variables manually
ENV LUA_PATH="/root/.luarocks/share/lua/5.1/?.lua;/root/.luarocks/share/lua/5.1/?/init.lua;/root/torch/install/share/lua/5.1/?.lua;/root/torch/install/share/lua/5.1/?/init.lua;./?.lua;/root/torch/install/share/luajit-2.1.0-beta1/?.lua;/usr/local/share/lua/5.1/?.lua;/usr/local/share/lua/5.1/?/init.lua"
ENV LUA_CPATH="/root/.luarocks/lib/lua/5.1/?.so;/root/torch/install/lib/lua/5.1/?.so;./?.so;/usr/local/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/loadall.so"
ENV PATH="/root/torch/install/bin:$PATH"
ENV LD_LIBRARY_PATH="/root/torch/install/lib:$LD_LIBRARY_PATH"
ENV DYLD_LIBRARY_PATH="/root/torch/install/lib:$DYLD_LIBRARY_PATH"
ENV LUA_CPATH="/root/torch/install/lib/?.so;$LUA_CPATH"
WORKDIR /dmtk
RUN cd /dmtk \
 && git clone https://github.com/Microsoft/multiverso.git \
 && cd multiverso \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make \
 && make install
#   python tests
RUN cd /dmtk/multiverso/binding/python \
 && python setup.py install \
 && nosetests
#   lua tests
RUN cd /dmtk/multiverso/binding/lua \
 && make install \
 && make test
#   run cpp tests
RUN cd /dmtk/multiverso/build \
 && mpirun -np 4 ./Test/multiverso.test kv \
 && mpirun -np 4 ./Test/multiverso.test array \
 && mpirun -np 4 ./Test/multiverso.test net \
 && mpirun -np 4 ./Test/multiverso.test ip \
 && mpirun -np 4 ./Test/multiverso.test checkpoint \
 && mpirun -np 4 ./Test/multiverso.test restore \
 && mpirun -np 4 ./Test/multiverso.test allreduce
#   - mpirun -np 4 ./Test/multiverso.test matrix  # TODO the matrix test won't stop
#   - mpirun -np 4 ./Test/multiverso.test TestSparsePerf # TODO TestSparsePerf takes too much time
#   - mpirun -np 4 ./Test/multiverso.test TestDensePerf # TODO TestDensePerf takes too much time
#   clean unnessary packages
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
