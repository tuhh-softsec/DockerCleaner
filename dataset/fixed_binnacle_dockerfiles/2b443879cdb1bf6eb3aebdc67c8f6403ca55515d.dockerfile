#
#   Docker file that builds RelEx and visual question answering pipeline.
#
#   To start:
#      docker run -it -p 8888:8888 vqa
#
#   To demo:
#      open in browser localhost:8888
#      use password: password 
#
#   That will open jupyter notebook with vqa demo
#
FROM ubuntu:18.04
#   use bash
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
#   Avoid triggering apt-get dialogs (which may lead to errors). See:
#   https://stackoverflow.com/questions/25019183/docker-java7-install-fail
ENV DEBIAN_FRONTEND="noninteractive"
ENV JAVA_HOME="/usr/lib/jvm/java-1.11.0-openjdk-amd64"
RUN : ; apt-get -y upgrade ; apt-get -y autoclean
#   Java
RUN (apt-get update ;apt-get install --no-install-recommends maven=3.6.0-1~18.04.1 screen=4.6.2-1ubuntu1.1 telnet=0.17-41 netcat-openbsd=1.187-1ubuntu0.1 byobu=5.125-0ubuntu1 wget=1.19.4-1ubuntu2.2 vim=2:8.0.1453-1ubuntu1.11 git=1:2.17.1-1ubuntu0.17 unzip=6.0-21ubuntu1.2 sudo=1.8.21p2-3ubuntu1.5 apt-utils=1.6.14 -y )
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-11-jdk=11.0.18+10-0ubuntu1~18.04.1 -y )
#   GCC and basic build tools
RUN (apt-get update ;apt-get install --no-install-recommends gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 make=4.1-9.1ubuntu1 swig=3.0.12-1 ant=1.10.5-3~18.04 -y )
#   Wordnet
RUN (apt-get update ;apt-get install --no-install-recommends wordnet=1:3.0-35 wordnet-dev=1:3.0-35 wordnet-sense-index=1:3.0-35 -y )
#   There are UTF8 chars in the Java sources, and the RelEx build will
#   break if build in a C environment.
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 -y ) \
 && locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
RUN mkdir /usr/local/share/java
WORKDIR /home/Downloads/
#   build tools
RUN (apt-get update ;apt-get install --no-install-recommends libboost-python-dev=1.65.1.0ubuntu1 libblas-dev=3.7.1-4ubuntu1 libboost-thread-dev=1.65.1.0ubuntu1 libboost-filesystem-dev=1.65.1.0ubuntu1 libboost-system-dev=1.65.1.0ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends cmake=3.10.2-1ubuntu2.18.04.2 libboost-program-options-dev=1.65.1.0ubuntu1 libboost-regex-dev=1.65.1.0ubuntu1 libiberty-dev=20170913-1ubuntu0.1 guile-2.2-dev=2.2.3+1-3ubuntu0.1 protobuf-compiler=3.0.0-9.1ubuntu1.1 uuid-dev=2.31.1-0.4ubuntu3.7 -y )
#   Conda
#   install conda 
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 bzip2=1.0.6-8.1ubuntu0.2 ca-certificates=20211016ubuntu0.18.04.1 libglib2.0-0=2.56.4-0ubuntu0.18.04.9 libxext6=2:1.3.3-1 libsm6=2:1.2.2-1 libxrender1=1:0.9.10-1 -y )
#   Create and switch user. The user is privileged, with no password
#   required.  That is, you can use sudo.
RUN adduser --disabled-password --gecos "ReLex USER" relex
RUN adduser relex sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER relex
RUN sudo chown -R relex:relex .
RUN sudo chown -R relex:relex /home/relex
#   Punch out ports
#  # plain-text-server.sh port
EXPOSE 3333/tcp
#  # opencog-server.sh port
EXPOSE 4444/tcp
#  # link-grammar-server.sh port
EXPOSE 9000/tcp
WORKDIR /home/relex
#   Link Parser -- changes often
#   Download the current released version of link-grammar.
#   The wget gets the latest version w/ wildcard
RUN wget -r --no-parent -nH --cut-dirs=2 https://www.abisource.com/downloads/link-grammar/current/
RUN tar -zxf current/link-grammar-5*.tar.gz
#   get linkgrammar version
RUN bash -l -c 'echo `ls|grep link|sed 's/link-grammar-//g'` >> LINKGRAMMAR_VERSION'
USER root
RUN cd link-grammar-5.*/ ; ./configure ; make -j6 ; sudo make install ; ldconfig
USER relex
RUN cd link-grammar-5.*/ ; mvn install:install-file -Dfile=./bindings/java/linkgrammar-`cat ../LINKGRAMMAR_VERSION `.jar -DgroupId=org.opencog -DartifactId=linkgrammar -Dversion=`cat ../LINKGRAMMAR_VERSION ` -Dpackaging=jar
USER root
RUN rm -rf * link-grammar*
USER relex
#   Relex -- changes often
RUN wget https://github.com/opencog/relex/archive/master.tar.gz -O master.tar.gz
RUN tar -xvf master.tar.gz ; cd relex-master ; mvn install
RUN echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/" >> /home/relex/.profile
RUN echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/relex/miniconda3/envs/pmvqa3/lib/" >> /home/relex/.profile
ENV LANG="C.UTF-8" \
    LC_ALL="C.UTF-8"
ENV PATH="/opt/conda/bin:$PATH"
RUN wget --quiet https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/anaconda.sh \
 && /bin/bash ~/anaconda.sh -b \
 && rm ~/anaconda.sh
RUN /home/relex/miniconda3/bin/conda create -y --name pmvqa3 python=3.5 \
 && source /home/relex/miniconda3/bin/activate pmvqa3 \
 && echo 'opencv ==3.1.0' > $( dirname $( which python ;) ;)/../conda-meta/pinned \
 && conda install -y opencv=3.1.0 atlas bokeh ca-certificates certifi cffi click cloudpickle cudatoolkit cudnn cycler cython cytoolz dask dask-core dbus decorator distributed expat fontconfig freetype gflags glib glog gst-plugins-base gstreamer h5py hdf5 heapdict icu imageio intel-openmp jbig jinja2 jpeg leveldb libedit libffi libgcc libgcc-ng libgfortran-ng libiconv libpng libprotobuf libstdcxx-ng libtiff libxcb libxml2 lmdb locket markupsafe matplotlib mkl msgpack-python nccl ncurses networkx ninja numpy opencv openssl packaging pandas partd pcre pillow pip protobuf psutil pycparser pyparsing pyqt python python-dateutil pytorch pytz pywavelets pyyaml qt readline scikit-image scipy setuptools sip six snappy sortedcontainers sqlite tblib tk toolz tornado wheel xz yaml zict zlib jupyter \
 && conda install -y -c conda-forge jpype1 \
 && pip install easydict==1.10 ipywebrtc==0.6.0
#   vqa piplie(java)
RUN wget https://github.com/singnet/semantic-vision/archive/master.zip
RUN unzip master.zip
RUN rm master.zip
RUN mkdir ~/projects
RUN mv semantic-vision-master/ ~/projects/semantic-vision-1
ENV JAVA_HOME="/usr/lib/jvm/java-1.11.0-openjdk-amd64"
RUN echo "export JAVA_HOME=/usr/lib/jvm/java-1.11.0-openjdk-amd64" >> /home/relex/.profile
RUN cd ~/projects/semantic-vision-1/experiments/opencog/question2atomese \
 && mvn package
#   faster-r-cnn
RUN source /home/relex/miniconda3/bin/activate pmvqa3 \
 && cd ~/projects/semantic-vision-1/experiments/opencog/pattern_matcher_vqa/feature \
 && git clone https://github.com/peteanderson80/bottom-up-attention.git \
 && cd bottom-up-attention \
 && patch -p 1 < ../bottom-up-attention.3.patch \
 && export LD_LIBRARY_PATH=$( dirname $( which python ;) ;)/../lib:$LD_LIBRARY_PATH \
 && cd caffe \
 && cp ../../Makefile.config.3 Makefile.config \
 && make ; make pycaffe ; cd ../lib \
 && export PYTHONPATH=$( pwd ;):$( pwd ;)/../caffe/python:$PYTHONPATH ; make
RUN echo "export PYTHONPATH=$PYTHONPATH:~/projects/semantic-vision-1/experiments/opencog/pattern_matcher_vqa/feature/bottom-up-attention/lib:~/projects/semantic-vision-1/experiments/opencog/pattern_matcher_vqa/feature/bottom-up-attention/lib/../caffe/python" >> /home/relex/.profile
USER relex 
#   cogutil
RUN wget https://github.com/opencog/cogutil/archive/master.zip
RUN unzip master.zip \
 && cd cogutil-master/ \
 && mkdir build \
 && cd build \
 && cmake ..
RUN cd cogutil-master/build \
 && sudo make install
RUN sudo rm -rf cogutil-master master.zip
#   atomspace
RUN wget https://github.com/opencog/atomspace/archive/master.zip
RUN unzip master.zip
RUN source /home/relex/miniconda3/bin/activate pmvqa3 \
 && cd atomspace-master \
 && mkdir build \
 && cd build/ \
 && cmake .. \
 && make
RUN cd atomspace-master/build \
 && sudo make install
RUN sudo rm -rf atomspace-master/build master.zip
RUN mv atomspace-master /home/relex/projects/atomspace
#   opencog
RUN wget https://github.com/opencog/opencog/archive/master.zip
RUN unzip master.zip
RUN source /home/relex/miniconda3/bin/activate pmvqa3 \
 && cd opencog-master \
 && mkdir build \
 && cd build/ \
 && cmake .. \
 && make
RUN cd opencog-master/build \
 && sudo make install
RUN sudo rm -rf opencog-master/build master.zip
RUN mv opencog-master /home/relex/projects/opencog
#   download some images
RUN mkdir -p /home/relex/projects/semantic-vision-1/experiments/opencog/pattern_matcher_vqa/images \
 && cd /home/relex/projects/semantic-vision-1/experiments/opencog/pattern_matcher_vqa/images/ \
 && wget https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/Plains_Zebra_Equus_quagga.jpg/440px-Plains_Zebra_Equus_quagga.jpg
RUN echo "export GUILE_AUTO_COMPILE=0" >> /home/relex/.profile
RUN echo "(use-modules (ice-9 readline)) (activate-readline)(add-to-load-path \"/usr/local/share/opencog/scm\")(add-to-load-path \"/home/relex/projects/opencog/examples/pln/conjunction/\")(add-to-load-path \"/home/relex/projects/atomspace/examples/rule-engine/rules/\")(add-to-load-path \"/home/relex/projects/opencog/opencog/pln/rules/\")(add-to-load-path \".\")(use-modules (opencog))(use-modules (opencog query))(use-modules (opencog exec))" >> /home/relex/.guile
RUN echo "export PYTHONPATH=$PYTHONPATH:/usr/local/lib/python3.5/dist-packages/" >> /home/relex/.profile
RUN echo "export PYTHONPATH=$PYTHONPATH:/usr/local/lib/python3/dist-packages/" >> /home/relex/.profile
RUN source /home/relex/miniconda3/bin/activate pmvqa3 \
 && jupyter notebook --generate-config
RUN echo "c.NotebookApp.password = 'sha1:b6e570f197d6:b920933b262cf450f5c11dc21d878c53972cb2fa'" >> /home/relex/.jupyter/jupyter_notebook_config.py
RUN echo "c.NotebookApp.password_required = False" >> /home/relex/.jupyter/jupyter_notebook_config.py
RUN echo "c.NotebookApp.port = 8888" >> /home/relex/.jupyter/jupyter_notebook_config.py
RUN chmod +x /home/relex/projects/semantic-vision-1/experiments/opencog/pattern_matcher_vqa/vqa
CMD /home/relex/projects/semantic-vision-1/experiments/opencog/pattern_matcher_vqa/vqa
# Please add your HEALTHCHECK here!!!
