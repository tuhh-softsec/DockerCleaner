FROM ubuntu:16.04
MAINTAINER David Lung "lungdm@gmail.com"
ARG INTEL_SDK_VERSION=2017_7.0.0.2511_x64
COPY ./silent-intel-sdk.cfg /tmp/silent-intel-sdk.cfg
ARG USR=ow
ENV USER="$USR"
RUN : \
 && apt-get upgrade -y \
 && apt-get dist-upgrade -y
RUN mkdir -p /etc/sudoers.d \
 && export uid=1000 gid=1000 \
 && mkdir -p /home/$USER \
 && echo "$USER:x:${uid}:${gid}:$USER,,,:/home/$USER:/bin/bash" >> /etc/passwd \
 && echo "$USER:x:${uid}:" >> /etc/group \
 && echo "$USER ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/$USER \
 && chmod 0440 /etc/sudoers.d/$USER \
 && chown ${uid}:${gid} -R /home/$USER
ENV DEBIAN_FRONTEND="noninteractive # TODO: change"
#  RUN useradd -ms /bin/bash $USER
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=1.2.35 wget=1.17.1-1ubuntu1.5 nano=2.5.3-2ubuntu2 htop=2.0.1-1ubuntu1 build-essential=12.1ubuntu2 make=4.1-6 git=1:2.7.4-0ubuntu1.10 automake=1:1.15-4ubuntu1 autoconf=2.69-9 g++=4:5.3.1-1ubuntu1 rpm=4.12.0.1+dfsg1-3build3 libtool=2.4.6-0.1 libncurses5-dev=6.0+20160213-1ubuntu1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 bison=2:3.0.4.dfsg-1 flex=2.6.0-11 lsb-core=9.20160110ubuntu0.2 sudo=1.8.16-0ubuntu1.10 xorg=1:7.7+13ubuntu3.1 openbox=3.6.1-1ubuntu2.1 x11-xserver-utils=7.7+7 libxext-dev=2:1.3.3-1 libncurses-dev python-dev=2.7.12-1~16.04 mercurial=3.7.3-1ubuntu1.2 freeglut3-dev=2.8.1-2 libglu1-mesa-dev=9.0.0-2.1 libglew-dev=1.13.0-2 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python-lxml=3.5.0-1ubuntu0.4 python-numpy=1:1.11.0-1ubuntu1 python-scipy=0.17.0-1 python-tk=2.7.12-1~16.04 kmod=22-1ubuntu5.2 dkms=2.2.0.3-2ubuntu11.8 linux-source=4.4.0.210.216 linux-headers-generic=4.4.0.210.216 maven=3.3.9-3 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 python-setuptools=20.7.0-1 python-yaml=3.11-3build1 libnuma1=2.0.11-1ubuntu1.1 openmpi-bin=1.10.2-8ubuntu1 libopenmpi-dev=1.10.2-8ubuntu1 libgl1-mesa-glx=18.0.5-0ubuntu0~16.04.1 libgl1-mesa-dri=18.0.5-0ubuntu0~16.04.1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libpng12-dev=1.2.54-1ubuntu1.1 libxft-dev=2.3.2-1 python-matplotlib=1.5.1-1ubuntu1 xubuntu-desktop=2.206 ffmpeg=7:2.8.17-0ubuntu0.1 xvfb=2:1.18.4-0ubuntu0.12 tmux=2.1-3build1 -y )
#  RUN  sudo pip install --upgrade matplotlib 
#  RUN sudo apt-get install nvidia-opencl-dev
RUN sudo usermod -a -G video $USER
USER $USER
ENV HOME="/home/$USER"
WORKDIR $HOME
RUN mkdir neuron \
 && cd neuron \
 && git clone https://github.com/nrnhines/iv.git \
 && git clone https://github.com/nrnhines/nrn.git \
 && cd iv \
 && git checkout 76c123b \
 && ./build.sh \
 && ./configure --prefix=`pwd ` \
 && make \
 && sudo make install \
 && cd ../nrn \
 && git checkout e0950a1 \
 && ./build.sh \
 && ./configure --prefix=`pwd ` --with-iv=$HOME/neuron/iv --with-nrnpython=/usr/bin/python --with-paranrn \
 && make \
 && sudo make install \
 && cd src/nrnpython \
 && sudo python setup.py install
RUN mkdir intel-opencl-tmp \
 && cd intel-opencl-tmp \
 && mkdir intel-opencl \
 && wget http://registrationcenter-download.intel.com/akdlm/irc_nas/11396/SRB5.0_linux64.zip \
 && unzip SRB5.0_linux64.zip \
 && tar -C intel-opencl -Jxf intel-opencl-r5.0-63503.x86_64.tar.xz \
 && tar -C intel-opencl -Jxf intel-opencl-devel-r5.0-63503.x86_64.tar.xz \
 && tar -C intel-opencl -Jxf intel-opencl-cpu-r5.0-63503.x86_64.tar.xz \
 && sudo cp -R intel-opencl/* / \
 && sudo ldconfig \
 && cd .. \
 && sudo rm -r intel-opencl-tmp
RUN wget http://registrationcenter-download.intel.com/akdlm/irc_nas/vcp/11705/intel_sdk_for_opencl_$INTEL_SDK_VERSION.tgz \
 && tar xvf intel_sdk_for_opencl_$INTEL_SDK_VERSION.tgz \
 && cd intel_sdk_for_opencl_$INTEL_SDK_VERSION \
 && sudo ./install.sh --silent /tmp/silent-intel-sdk.cfg \
 && cd $HOME \
 && rm intel_sdk_for_opencl_$INTEL_SDK_VERSION.tgz \
 && sudo rm /tmp/silent-intel-sdk.cfg
RUN git clone https://github.com/NeuroML/pyNeuroML.git \
 && cd pyNeuroML \
 && git checkout ow-0.8a \
 && sudo python setup.py install
RUN git clone https://github.com/openworm/PyOpenWorm.git \
 && cd PyOpenWorm \
 && git checkout 7ff1266 \
 && sudo python setup.py install
RUN git clone https://github.com/openworm/CElegansNeuroML.git \
 && cd CElegansNeuroML \
 && git checkout c8b1364
RUN git clone https://github.com/openworm/sibernetic.git \
 && cd sibernetic \
 && git checkout 3eb9914 \
 && make clean \
 && make all
ENV JNML_HOME="$HOME/jNeuroML"
ENV PATH="$PATH:$JNML_HOME"
ENV IV="$HOME/neuron/iv"
ENV N="$HOME/neuron/nrn"
ENV CPU="x86_64"
ENV PATH="$PATH:$IV/$CPU/bin:$N/$CPU/bin"
ENV NEURON_HOME="$N/$CPU"
ENV C302_HOME="$HOME/CElegansNeuroML/CElegans/pythonScripts/c302"
ENV SIBERNETIC_HOME="$HOME/sibernetic"
ENV PYTHONPATH="$PYTHONPATH:$C302_HOME:$SIBERNETIC_HOME"
#   Not working with --chown=$USER:$USER
COPY ./master_openworm.py $HOME/master_openworm.py
RUN sudo chown $USER:$USER $HOME/master_openworm.py
# Please add your HEALTHCHECK here!!!
