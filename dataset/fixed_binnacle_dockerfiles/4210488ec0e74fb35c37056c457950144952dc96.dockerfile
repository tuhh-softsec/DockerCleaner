#   docker-keras-full - Deep learning environment with *Keras* and *Jupyter* using CPU or GPU
FROM gw000/keras:2.1.4-gpu
MAINTAINER gw0 [http://gw.tnode.com/] <gw.2018@ena.one>
#   install py2-tf-cpu/gpu (Python 2, TensorFlow, CPU/GPU)
#   (already installed in upstream image)
#   install py2-th-cpu (Python 2, Theano, CPU/GPU)
ARG THEANO_VERSION=1.0.1
ENV THEANO_FLAGS="device=cpu,floatX=float32"
RUN pip install git+https://github.com/Theano/Theano.git@rel-${THEANO_VERSION} --no-cache-dir
#   install py2-cntk-cpu (Python 2, CNTK, CPU/GPU)
#   requirements from old ubuntu repositories for cntk
RUN echo 'deb http://archive.ubuntu.com/ubuntu/ xenial main restricted universe multiverse' > /etc/apt/sources.list.d/ubuntu-16.04.list
RUN apt-get update -qq \
 && apt-get install --no-install-recommends ubuntu-archive-keyring -y \
 && apt-get update -qq \
 && apt-get install --no-install-recommends openmpi-bin=1.10.2-8ubuntu1 openmpi-common=1.10.2-8ubuntu1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ARG CNTK_VERSION=2.4
ARG CNTK_DEVICE=CPU-Only
RUN pip install https://cntk.ai/PythonWheel/${CNTK_DEVICE}/cntk-${CNTK_VERSION}-cp27-cp27mu-linux_x86_64.whl --no-cache-dir
#   install Keras for Python 2
#   (already installed in upstream image)
#   install py3-tf-cpu/gpu (Python 3, TensorFlow, CPU/GPU)
RUN apt-get update -qq \
 && apt-get install --no-install-recommends python3 python3-dev python3-pip python3-setuptools python3-virtualenv python3-wheel pkg-config libopenblas-base python3-numpy python3-scipy python3-h5py python3-yaml python3-pydot -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   manually update numpy
RUN pip3 --no-cache-dir install -U numpy==1.13.3
ARG TENSORFLOW_VERSION=1.5.0
ARG TENSORFLOW_DEVICE=gpu
ARG TENSORFLOW_APPEND=_gpu
RUN pip3 --no-cache-dir install https://storage.googleapis.com/tensorflow/linux/${TENSORFLOW_DEVICE}/tensorflow${TENSORFLOW_APPEND}-${TENSORFLOW_VERSION}-cp35-cp35m-linux_x86_64.whl
#   install py3-th-cpu/gpu (Python 3, Theano, CPU/GPU)
ARG THEANO_VERSION=1.0.1
ENV THEANO_FLAGS="device=cpu,floatX=float32"
RUN pip3 --no-cache-dir install git+https://github.com/Theano/Theano.git@rel-${THEANO_VERSION}
#   install py3-cntk-cpu/gpu (Python 3, CNTK, CPU/GPU)
ARG CNTK_VERSION=2.4
ARG CNTK_DEVICE=GPU
RUN pip3 --no-cache-dir install https://cntk.ai/PythonWheel/${CNTK_DEVICE}/cntk-${CNTK_VERSION}-cp35-cp35m-linux_x86_64.whl
#   install Keras for Python 3
ARG KERAS_VERSION=2.1.4
ENV KERAS_BACKEND="tensorflow"
RUN pip3 --no-cache-dir install --no-dependencies git+https://github.com/fchollet/keras.git@${KERAS_VERSION}
#   install additional debian packages
RUN apt-get update -qq \
 && apt-get install --no-install-recommends less procps vim-tiny build-essential libffi-dev python-matplotlib python-pillow python3-matplotlib python3-pillow -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   install additional python packages
RUN pip install ipython==8.12.0 ipykernel==6.22.0 jupyter==1.0.0 jupyter-kernel-gateway==2.5.2 jupyter-tensorboard==0.2.0 pandas==2.0.0 scikit-learn==1.2.2 statsmodels==0.13.5 --no-cache-dir \
 && python -m ipykernel.kernelspec \
 && pip3 --no-cache-dir install ipython ipykernel pandas scikit-learn statsmodels \
 && python3 -m ipykernel.kernelspec
#   configure console
RUN echo 'alias ll="ls --color=auto -lA"' >> /root/.bashrc \
 && echo '"\e[5~": history-search-backward' >> /root/.inputrc \
 && echo '"\e[6~": history-search-forward' >> /root/.inputrc
ENV SHELL="/bin/bash"
#   quick test and dump package lists
RUN jupyter notebook --version \
 && jupyter nbextension list 2>&1 \
 && python -c "import numpy; print(numpy.__version__)" \
 && python -c "import tensorflow; print(tensorflow.__version__)" \
 && python -c "import theano; print(theano.__version__)" \
 && python -c "import cntk; print(cntk.__version__)" \
 && MPLBACKEND=Agg python -c "import matplotlib.pyplot" \
 && python3 -c "import numpy; print(numpy.__version__)" \
 && python3 -c "import tensorflow; print(tensorflow.__version__)" \
 && python3 -c "import theano; print(theano.__version__)" \
 && python3 -c "import cntk; print(cntk.__version__)" \
 && MPLBACKEND=Agg python3 -c "import matplotlib.pyplot" \
 && rm -rf /tmp/* \
 && dpkg-query -l > /dpkg-query-l.txt \
 && pip2 freeze > /pip2-freeze.txt \
 && pip3 freeze > /pip3-freeze.txt
#   run as user 1000
RUN useradd --create-home --uid 1000 --user-group --groups video --shell /bin/bash user \
 && cp -a /root/.jupyter /root/.local /home/user \
 && chown -R user:user /home/user /srv
USER user
#   publicly accessible on any IP
ENV IP="*"
#   accessible only from localhost
#  ENV IP=127.0.0.1
#   only password authentication (password: keras)
#  ENV PASSWD='sha1:98b767162d34:8da1bc3c75a0f29145769edc977375a373407824'
#  unset ENV TOKEN=
#   password and token authentication (password and token: keras)
ENV PASSWD="sha1:98b767162d34:8da1bc3c75a0f29145769edc977375a373407824"
ENV TOKEN="keras"
#   random token authentication
#  unset ENV PASSWD=
#  unset ENV TOKEN=
EXPOSE 8888/tcp
WORKDIR /srv/
CMD /bin/bash -c 'jupyter notebook --NotebookApp.open_browser=False --NotebookApp.allow_root=True --NotebookApp.ip="$IP" ${PASSWD+--NotebookApp.password=\"$PASSWD\"} ${TOKEN+--NotebookApp.token=\"$TOKEN\"} --NotebookApp.allow_password_change=False --JupyterWebsocketPersonality.list_kernels=True "$@"'
# Please add your HEALTHCHECK here!!!
