# #########################
#  https://github.com/tensorflow/tensorflow/blob/master/tensorflow/tools/docker/Dockerfile
#  https://github.com/tensorflow/tensorflow/blob/master/tensorflow/tools/docker/Dockerfile.gpu
#  https://gcr.io/tensorflow/tensorflow
FROM gcr.io/tensorflow/tensorflow:1.3.0-py3
# #########################
# #########################
#  Set working directory
ENV ROOT_DIR="/"
WORKDIR ${ROOT_DIR}
ENV HOME="/root"
# #########################
# #########################
#  Update OS
#  Configure 'bash' for 'source' commands
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections \
 && apt-get update -y \
 && apt-get install --no-install-recommends build-essential gcc apt-utils pkg-config software-properties-common apt-transport-https libssl-dev sudo bash bash-completion tar unzip curl wget git libcupti-dev rsync python python-pip python-dev python3-pip libhdf5-dev python-tk python3-tk -y \
 && rm /bin/sh \
 && ln -s /bin/bash /bin/sh \
 && ls -l $( which bash ;) \
 && echo "root ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers \
 && apt-get -y clean \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get update -y \
 && apt-get -y upgrade \
 && apt-get -y dist-upgrade \
 && apt-get update -y \
 && apt-get -y upgrade \
 && apt-get -y autoremove \
 && apt-get -y autoclean
# #########################
# #########################
#  built for CPU, no need to install cuda
# #########################
# #########################
#  install basic packages
RUN pip3 --no-cache-dir install requests glog humanize bcolz h5py
# #########################
# #########################
#  install Keras
RUN pip3 --no-cache-dir install theano keras==2.0.8 \
 && echo $'[global]\ndevice = cpu\nfloatX = float32\n[cuda]\nroot = /usr/local/cuda\n' > ${HOME}/.theanorc \
 && cat ${HOME}/.theanorc \
 && mkdir -p ${HOME}/.keras/datasets \
 && mkdir -p ${HOME}/.keras/models \
 && echo $'{\n "image_data_format": "channels_last",\n "epsilon": 1e-07,\n "floatx": "float32",\n "backend": "tensorflow"\n}\n' > ${HOME}/.keras/keras.json \
 && cat ${HOME}/.keras/keras.json
# #########################
# #########################
#  Clone source code, static assets
COPY ./datasets/parameters-cats.npy /root/datasets/parameters-cats.npy
COPY ./backend ${ROOT_DIR}/backend
COPY ./scripts/docker/run ${ROOT_DIR}/scripts/docker/run
COPY ./scripts/tests ${ROOT_DIR}/scripts/tests
# #########################
# #########################
#  Configure Jupyter
COPY ./jupyter_notebook_config.py /root/.jupyter/
#  Jupyter has issues with being run directly: https://github.com/ipython/ipython/issues/7062
#  We just add a little wrapper script.
COPY ./run_jupyter.sh /
# #########################
