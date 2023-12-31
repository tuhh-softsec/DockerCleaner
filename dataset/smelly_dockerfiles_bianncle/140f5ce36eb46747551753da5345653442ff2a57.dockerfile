FROM nvidia/cuda:9.0-cudnn7-devel-ubuntu16.04
#  Docker base environment
#  Includes most dependences for deep learning, computer vision
ENV DEBIAN_FRONTEND="noninteractive"
MAINTAINER Adam Harvey
#  Ubuntu 16.04 + Cuda 9.0 + CUDNN 7.0 + Python2.7 + Python3.5
#  Using Nvidia driver 367.51 and nvidia-docker
#  Install Nvidia driver 367.51: 
#    add-apt-repository ppa:graphics-drivers/ppa
#    apt-get install nvidia-367
#  [ environment paths ]
RUN echo export CUDA_HOME=/usr/local/cuda/ >> /etc/bash.bashrc
RUN echo export PATH=/root/bin/:${CUDA_HOME}/bin:${PATH} >> /etc/bash.bashrc
RUN echo export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib" >> /etc/bash.bashrc
#  --------------------------------------------------------
#  [ Update and upgrade Ubuntu ]
RUN apt-get update
RUN apt-get install --no-install-recommends software-properties-common -y \
 && add-apt-repository "deb http://archive.ubuntu.com/ubuntu $( lsb_release -sc ;) multiverse"
RUN apt-get update \
 && apt-get upgrade -y
#  --------------------------------------------------------
#  [ System dependencies ]
RUN apt-get install ant bc build-essential checkinstall cmake curl default-jdk doxygen git gfortran gir1.2-gst-plugins-base-0.10 gir1.2-gstreamer-0.10 imagemagick iproute2 mediainfo nano nginx pkg-config protobuf-compiler python-cffi python-dev python-magic python-h5py python-numpy python-pip python-pythonmagick python-tk python-qt4 python-yaml python-xtermcolor qt5-default rsync supervisor screen sphinx-common texlive-latex-extra tesseract-ocr x264 v4l-utils vim unzip vlc wget xauth yasm youtube-dl zip zlib1g-dev -y
RUN apt-get update
RUN apt-get install libatlas-base-dev libavcodec-dev libavformat-dev libcurl3-dev libdc1394-22-dev libeigen3-dev libfaac-dev libffi-dev libgflags-dev libfreetype6-dev libgoogle-glog-dev libgstreamer-plugins-base0.10-0 libgstreamer-plugins-base0.10-dev libgstreamer0.10-0 libgstreamer0.10-dev libgtk2.0-dev libhdf5-dev libhdf5-serial-dev libjasper-dev libjpeg-dev libjpeg8-dev libleveldb-dev liblmdb-dev libmp3lame-dev libopencore-amrnb-dev libopencore-amrwb-dev libopenexr-dev libpng12-dev libprotobuf-dev libqt4-dev libqt4-opengl-dev libreadline-dev libsnappy-dev libssl-dev libswscale-dev libtbb-dev libtheora-dev libtiff5-dev libvtk5-qt4-dev libv4l-dev libxine2-dev libvorbis-dev libx264-dev libatlas-base-dev libgphoto2-dev libxvidcore-dev libzmq3-dev -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get update \
 && apt-get install --no-install-recommends libboost-all-dev -y \
 && apt-get install pv locate inkscape -y \
 && updatedb
#  [ install pip2/3 ]
RUN apt-get update
RUN apt-get install python-dev python-pip python3-dev python3-pip -y
RUN pip2 install -U pip
RUN pip3 install -U pip
#  [ add Github creds ]
RUN git config --global user.name "docker" \
 && git config --global user.email "docker@docker.com"
#  [ Install Python2.7 packages ]
RUN pip2.7 install -U setuptools packaging pyparsing six cython svgwrite numpy sklearn scikit-image scikit-learn imutils Pillow matplotlib argparse jupyter scipy easydict click pandas ipdb python-osc tqdm xmltodict librosa uwsgi Flask requests bcolz sympy
#  [ Install Python3 packages ]
RUN pip3 install -U setuptools packaging pyparsing six cython svgwrite numpy sklearn scikit-image scikit-learn imutils Pillow matplotlib argparse jupyter scipy easydict click pandas ipdb python-osc tqdm xmltodict librosa uwsgi Flask requests python-dateutil bcolz sympy
#  [ Install ZSH ]
RUN apt-get install -y zsh
RUN git clone git://github.com/robbyrussell/oh-my-zsh.git /root/.oh-my-zsh
RUN cp /root/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc
RUN chsh -s /bin/zsh
#  [ ffmpeg ]
RUN apt-get update \
 && apt-get install ffmpeg -y --upgrade
#  [ update config ]
RUN adduser --disabled-password --gecos "" docker \
 && echo 'docker ALL=NOPASSWD: ALL' >> /etc/sudoers \
 && su -c 'python -c "import matplotlib.pyplot"' docker \
 && python -c 'import matplotlib.pyplot' \
 && echo 'ln -f /dev/null /dev/raw1394 2>/dev/null' >> /etc/bash.bashrc \
 && echo 'export PATH=/work/bin:/root/bin:${PATH}' >> /etc/bash.bashrc
#  this avoids the "libdc1394 error: Failed to initialize libdc1394"
#  warning which is otherwise printed every time cv2 is imported
