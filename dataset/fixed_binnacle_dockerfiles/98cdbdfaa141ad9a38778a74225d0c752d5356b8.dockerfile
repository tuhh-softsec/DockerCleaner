#   build from ubuntu 14.04, matching host
FROM ubuntu:14.04
MAINTAINER Kyle F <kylef@lab41.org>
#   configure headless install
ENV DEBIAN_FRONTEND="noninteractive"
#   install base
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends bc=1.06.95-8ubuntu1 build-essential=11.6ubuntu6 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 dnsutils=1:9.9.5.dfsg-3ubuntu0.19 g++=4:4.8.2-1ubuntu6 g++-4.6=4.6.4-6ubuntu2 g++-4.6-multilib=4.6.4-6ubuntu2 gcc-4.6=4.6.4-6ubuntu2 gcc-4.6-multilib=4.6.4-6ubuntu2 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 htop=1.0.2-3 inetutils-ping=2:1.9.2-1 less=458-2 libatlas-base-dev=3.10.1-4 libatlas-dev=3.10.1-4 libboost-all-dev=1.54.0.1ubuntu1 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libfreeimage-dev=3.15.4-3ubuntu0.1 libfreetype6=2.5.2-1ubuntu2.8 libfreetype6-dev=2.5.2-1ubuntu2.8 libhdf5-serial-dev=1.8.11-5ubuntu7.1 libjpeg-dev=8c-2ubuntu8 libjpeg62=6b1-4ubuntu1 liblapack-dev=3.5.0-2ubuntu1 libleveldb-dev=1.15.0-2 liblmdb-dev=0.9.10-1 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libprotobuf-dev=2.5.0-9ubuntu1 libsnappy-dev=1.1.0-1ubuntu1 libyaml-dev=0.1.4-3ubuntu3.1 net-tools=1.60-25ubuntu2.1 netcat=1.10-40 nmap=6.40-0.2ubuntu1 pkgconf=0.9.4-1 protobuf-compiler=2.5.0-9ubuntu1 python-dev=2.7.5-5ubuntu3 python-lxml=3.3.3-1ubuntu0.2 python-magic=1:5.14-2ubuntu3.4 python-matplotlib=1.3.1-1ubuntu5.1 python-numpy=1:1.8.2-0ubuntu0.1 python-pip=1.5.4-1ubuntu4 python-scipy=0.13.3-1build1 socat=1.7.2.3-1 software-properties-common=0.92.37.8 sudo=1.8.9p5-1ubuntu1.4 telnet=0.17-36build2 tree=1.6.0-1 unzip=6.0-9ubuntu1.5 vim=2:7.4.052-1ubuntu3.1 wget=1.15-1ubuntu1.14.04.5 --assume-yes )
#   Use gcc 4.6
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/gcc-4.6 30 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++-4.6 30 \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.6 30 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.6 30
#   install python modules
RUN pip install ipython==8.12.0 jinja2==3.1.2 tornado==6.2 jsonschema==4.17.3 terminado==0.17.1 simplejson==3.19.1
#   setup ipython
ENV IPYTHONDIR="/ipython"
RUN mkdir /ipython \
 && ipython profile create nbserver
#   install/configure CUDA 7.5 (for driver version 352.39)
#   Change to the /tmp directory
RUN cd /tmp \
 && wget -nv http://developer.download.nvidia.com/compute/cuda/7.5/Prod/local_installers/cuda_7.5.18_linux.run \
 && chmod +x cuda_*_linux.run \
 && ./cuda_*_linux.run -extract=`pwd ` \
 && ./NVIDIA-Linux-x86_64-*.run -s --no-kernel-module \
 && ./cuda-linux64-rel-*.run -noprompt \
 && rm -rf *
ENV PATH="/usr/local/cuda/bin:$PATH"
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:$LD_LIBRARY_PATH"
ENV LIBRARY_PATH="/usr/local/lib:/usr/local/cuda/lib:$LD_LIBRARY_PATH"
RUN echo "/usr/local/cuda/lib64" > /etc/ld.so.conf.d/cuda.conf \
 && ldconfig
#   download/configure/install neon 1.0.0
RUN pip install virtualenv==20.21.0
RUN cd /opt/ \
 && wget -nv https://github.com/NervanaSystems/neon/archive/v1.0.0.tar.gz
RUN cd /opt \
 && tar -xzvf v1.0.0.tar.gz \
 && mv neon-1.0.0 neon
RUN cd /opt/neon \
 && sed -i 's/(CRST, KRST)/(CRST ,)/g' neon/backends/layer_gpu.py \
 && make sysinstall
#   make some updates
RUN pip install six==1.16.0 -U
#   add gensim
RUN pip install gensim==4.3.1
RUN pip install nltk==3.8.1
#   add glove-python
#  RUN cd /tmp && \
#      git clone https://github.com/maciejkula/glove-python.git && \
#      cd glove-python && \
#      python setup.py install && \
#      rm -rf *
#   install custom glove code
RUN cd /tmp \
 && git clone https://github.com/Lab41/sunny-side-up.git \
 && cd sunny-side-up/src/glove \
 && python setup.py install \
 && rm -rf *
#   six version conflict (1.5.2 vs 1.9+)
RUN echo "\ndeb http://archive.ubuntu.com/ubuntu/ vivid main" >> /etc/apt/sources.list \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python-six=1.5.2-1ubuntu1.1 )
RUN sed -i '$d' /etc/apt/sources.list \
 && :
#   default to shell in root dir
WORKDIR /root
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
