FROM ubuntu:16.04
#   Cuda 7.5 with cudnn 5
#  FROM nvidia/cuda:7.5-cudnn5-devel
#   Cuda 8 with cudnn 5
FROM nvidia/cuda:8.0-cudnn5-devel
#   ViZdoom dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 bzip2=1.0.6-8ubuntu0.2 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 libboost-all-dev=1.58.0.1ubuntu1 libbz2-dev=1.0.6-8ubuntu0.2 libfluidsynth-dev=1.1.6-3 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libgme-dev=0.6.0-3ubuntu0.16.04.1 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 libjpeg-dev=8c-2ubuntu8 libopenal-dev=1:1.16.0-3 libpng12-dev=1.2.54-1ubuntu1.1 libsdl2-dev=2.0.4+dfsg1-2ubuntu2.16.04.2 libwildmidi-dev=0.3.8-2 libzmq3-dev=4.1.4-7ubuntu0.1 nano=2.5.3-2ubuntu2 nasm=2.11.08-1ubuntu0.1 pkg-config=0.29.1-0ubuntu1 rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 sudo=1.8.16-0ubuntu1.10 tar=1.28-2.1ubuntu0.2 timidity=2.13.2-40.3 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 python3-dev=3.5.1-3 python3=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 -y
#   Python with pip
#  RUN apt-get install -y python-dev python python-pip
#  RUN pip install pip --upgrade
#   Python3 with pip3
RUN pip3 install pip --upgrade
#   Vizdoom and other pip packages if needed
#  RUN pip --no-cache-dir install \
#           git+https://github.com/mwydmuch/ViZDoom \
#           numpy \
#  RUN pip --no-cache-dir install \
#      https://storage.googleapis.com/tensorflow/linux/gpu/tensorflow-0.10.0-cp27-none-linux_x86_64.whl
#   Vizdoom and other pip3 packages if needed
RUN pip3 --no-cache-dir install git+https://github.com/mwydmuch/ViZDoom opencv-python
RUN pip3 --no-cache-dir install https://storage.googleapis.com/tensorflow/linux/gpu/tensorflow_gpu-1.0.1-cp35-cp35m-linux_x86_64.whl
#   Enables X11 sharing and creates user home directory
ENV USER_NAME="cig2017"
ENV HOME_DIR="/home/$USER_NAME"
#   Replace HOST_UID/HOST_GUID with your user / group id (needed for X11)
ENV HOST_UID="1000"
ENV HOST_GID="1000"
RUN export uid=${HOST_UID} gid=${HOST_GID} \
 && mkdir -p ${HOME_DIR} \
 && echo "$USER_NAME:x:${uid}:${gid}:$USER_NAME,,,:$HOME_DIR:/bin/bash" >> /etc/passwd \
 && echo "$USER_NAME:x:${uid}:" >> /etc/group \
 && echo "$USER_NAME ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/$USER_NAME \
 && chmod 0440 /etc/sudoers.d/$USER_NAME \
 && chown ${uid}:${gid} -R ${HOME_DIR}
USER ${USER_NAME}
WORKDIR ${HOME_DIR}
#   Copy agent files inside Docker image:
COPY config config
COPY sample_random_agent.py .
#  ## Do not change this ###
COPY cig2017.wad .
COPY _vizdoom.cfg .
#  #########################
#   Uncomment to use doom2.wad:
#  COPY doom2.wad /usr/local/lib/python3.5/dist-packages/vizdoom
CMD ./sample_random_agent.py
# Please add your HEALTHCHECK here!!!
