#   Copyright (c) Jupyter Development Team.
#   Distributed under the terms of the Modified BSD License.
FROM digitalanatomist/mybase
MAINTAINER Samir Jabari <samir.jabari@fau.de>
#  MAINTAINER aburnap@mit.edu
#  MAINTAINER Jupyter Project <jupyter@googlegroups.com>
#  ENV CUDA_HOME /usr/local/cuda-9.2
#  ENV LD_LIBRARY_PATH /usr/local/cuda-9.2:/usr/local/cuda-9.2/lib64:$LD_LIBRARY_PATH
#  ENV LD_LIBRARY_PATH /usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH
#  ENV LIBRARY_PATH $LD_LIBRARY_PATH
#   For CUDA profiling, TensorFlow requires CUPTI.
ENV LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
ENV CUDA_HOME="/usr/local/cuda"
ENV CUDA_ROOT="$CUDA_HOME"
ENV PATH="$PATH:$CUDA_ROOT/bin:$HOME/bin"
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$CUDA_ROOT/lib64"
USER root
RUN :
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install python-software-properties
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install software-properties-common
RUN add-apt-repository ppa:fkrull/deadsnakes \
 && :
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python3.6 -y )
RUN curl https://bootstrap.pypa.io/get-pip.py | python3.6
RUN python3.6 -m pip install --upgrade pip
RUN ln -sf /usr/bin/python3.6 /usr/local/bin/python3 \
 && ln -sf /usr/local/bin/pip /usr/local/bin/pip3
#   Install all OS dependencies for fully functional notebook server
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.9ubuntu3 git=1:2.39.2-1ubuntu1 curl=7.88.1-7ubuntu1 freeglut3-dev=3.4.0-1 libcupti-dev=11.8.87~11.8.0-2ubuntu1 libcurl3-dev libfreetype6-dev=2.12.1+dfsg-4 libpng12-dev libzmq3-dev=4.3.4-6 pkg-config=1.8.1-1ubuntu2 unzip=6.0-27ubuntu1 rsync=3.2.7-1 software-properties-common=0.99.35 inkscape=1.2.2-2ubuntu1 jed=1:0.99.20~pre.178+dfsg-1 libsm6=2:1.2.3-1build2 libxext-dev=2:1.3.4-1build1 libxrender1=1:0.9.10-1.1 lmodern=2.005-1 pandoc=2.17.1.1-1.1ubuntu1 vim=2:9.0.1000-4ubuntu2 libpng-dev=1.6.39-2 g++=4:12.2.0-3ubuntu1 gfortran=4:12.2.0-3ubuntu1 libffi-dev=3.4.4-1 libhdf5-dev=1.10.8+repack1-1ubuntu1 libjpeg-dev=8c-2ubuntu11 liblcms2-dev=2.14-2 libopenblas-dev=0.3.21+ds-4 liblapack-dev=3.11.0-2 libssl-dev=3.0.8-1ubuntu1 libtiff5-dev=4.5.0-4ubuntu1 libwebp-dev=1.2.4-0.1build1 nano=7.2-1 libopenslide-dev=3.4.1+dfsg-6build1 wget=1.21.3-1ubuntu1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 qt5-default libvtk6-dev libjasper-dev libopenexr-dev=3.1.5-4 libgdal-dev=3.6.2+dfsg-1build1 libdc1394-22-dev libavcodec-dev=7:5.1.2-3ubuntu1 libavformat-dev=7:5.1.2-3ubuntu1 libswscale-dev=7:5.1.2-3ubuntu1 libtheora-dev=1.1.1+dfsg.1-16.1 libvorbis-dev=1.3.7-1build2 libxvidcore-dev=2:1.3.7-1 libx264-dev=2:0.164.3095+gitbaee400-2build1 yasm=1.3.0-4 libopencore-amrnb-dev=0.1.6-1 libopencore-amrwb-dev=0.1.6-1 libv4l-dev=1.22.1-5build1 libxine2-dev=1.2.13-1 libtbb-dev=2021.8.0-1ubuntu2 libeigen3-dev=3.4.0-4 ant=1.10.13-1 default-jdk=2:1.17-74 doxygen=1.9.4-4 bc=1.07.1-3build1 cmake=3.25.1-1 python-dev python-tk python-setuptools python-numpy python-scipy python-nose python-h5py python-skimage python-matplotlib python-pandas python-sklearn python-sympy python3-dev=3.11.2-1 python3-tk=3.11.2-2 python3-setuptools=66.1.1-1 python3-numpy=1:1.24.2-1 python3-scipy=1.10.1-1 python3-nose=1.3.7-9 python3-h5py=3.7.0-8 python3-skimage=0.19.3-8build1 python3-matplotlib=3.5.2-4ubuntu1 python3-pandas=1.5.3+dfsg-2 python3-sklearn=1.2.1+dfsg-1build1 python3-sympy=1.11.1-1 libav-tools -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Installing OpenSlide
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends openslide-tools=3.4.1+dfsg-6build1 -y )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python3-openslide=1.2.0-1build2 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Installing Libvips
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libvips libvips-dev=8.14.1-3 libvips-tools=8.14.1-3 libopenslide-dev=3.4.1+dfsg-6build1 -y )
COPY requirements.txt /opt/conda/envs/requirements/
RUN /opt/conda/envs/deep_learning_python3/bin/python -m pip install scikit-image pandas tqdm imgaug kaggle pydicom tensorboardX tensorflow-tensorboard sklearn plotly pretrainedmodels seaborn keras skorch ignite graphviz sklearn_pandas isoweek pandas_summary spacy pypng torchtext Pillow h5py ipykernel jupyter matplotlib numpy scipy bcolz Cython path.py six sphinx wheel pygments Flask statsmodels ipython scikit-learn zmq openslide-python jupyter-tensorboard \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
RUN /opt/conda/envs/deep_learning_python2/bin/python -m pip install scikit-image pandas tqdm imgaug kaggle pydicom tensorboardX tensorflow-tensorboard sklearn plotly pretrainedmodels seaborn keras skorch ignite graphviz sklearn_pandas isoweek pandas_summary spacy pypng torchtext Pillow h5py ipykernel jupyter matplotlib numpy scipy bcolz Cython path.py six sphinx wheel pygments Flask statsmodels ipython scikit-learn zmq openslide-python jupyter-tensorboard \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
#   Installing OpenCV TensorFlow Kerasand Pytorch into the environments
RUN conda install -n deep_learning_python2 -y opencv tensorboard tensorflow-gpu=1.12.0 keras pytorch torchvision cuda92 -c pytorch \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
RUN conda install -n deep_learning_python3 opencv tensorboard tensorflow-gpu=1.12.0 keras pytorch torchvision cuda92 -c pytorch \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
RUN conda install --yes 'tensorflow-gpu==1.11.0' \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
#   Import matplotlib the first time to build the font cache.
#  ENV XDG_CACHE_HOME /home/$NB_USER/.cache/
#  RUN MPLBACKEND=Agg python -c "import matplotlib.pyplot" && \
#  fix-permissions /home/$NB_USER
#   Install facets which does not have a pip or conda package at the moment
#  RUN cd /tmp && \
#      git clone https://github.com/PAIR-code/facets.git && \
#      cd facets && \
#      /opt/conda/envs/deep_learning_python2/bin/ jupyter nbextension install facets-dist/ --sys-prefix && \
#      /opt/conda/envs/deep_learning_python3/bin/ jupyter nbextension install facets-dist/ --sys-prefix && \
#      rm -rf facets && \
#      fix-permissions $CONDA_DIR &&\
#      fix-permissions /home/$NB_USER
RUN python3.6 -m pip install --upgrade --force-reinstall --no-cache-dir --ignore-installed --pre jupyter-tensorboard \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
EXPOSE 5000/tcp 6006/tcp
USER root
#   Autodetects jupyterhub and standalone modes
#  COPY jupyter_notebook_config.py /root/.jupyter/
#  COPY run_jupyter.sh /usr/local/bin/
#  RUN chmod +x /usr/local/bin/run_jupyter.sh
#  CMD ["/usr/local/bin/run_jupyter.sh", "--allow-root"]
USER $NB_USER
CMD ["start-notebook.sh"]
#  COPY start-notebook.sh /usr/local/bin/
#  USER $NB_USER
#   cleanup
#  RUN 	apt-get autoremove -y && \
#   apt-get autoclean && \
#   apt-get clean && \
#  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
# Please add your HEALTHCHECK here!!!
