#   set out our GPU lib versions
#   note even though we only specify the major version for cuDNN it will always pull
ARG CUDA_V=9.0
FROM nvidia/cuda:${CUDA_V}-devel
ENV CUDA_VERSION="${CUDA_V}"
ENV CUDNN_VERSION="7.0.5.15"
#   Install system packages
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2 g++ git graphviz libgl1-mesa-glx libhdf5-dev openmpi-bin cuda-command-line-tools-9-0 wget -y \
 && rm -rf /var/lib/apt/lists/*
#   Install correct CuDNN version for tensorflow
LABEL com.nvidia.cudnn.version="${CUDNN_VERSION}"
RUN apt-get update \
 && apt-get install --no-install-recommends libcudnn7=$CUDNN_VERSION-1+cuda9.0 libcudnn7-dev=$CUDNN_VERSION-1+cuda9.0 -y \
 && rm -rf /var/lib/apt/lists/*
#   Install conda
ENV CONDA_DIR="/opt/conda"
ENV PATH="$CONDA_DIR/bin:$PATH"
RUN wget --quiet --no-check-certificate https://repo.continuum.io/miniconda/Miniconda3-4.4.10-Linux-x86_64.sh \
 && /bin/bash /Miniconda3-4.4.10-Linux-x86_64.sh -f -b -p $CONDA_DIR \
 && rm Miniconda3-4.4.10-Linux-x86_64.sh \
 && echo export PATH=$CONDA_DIR/bin:'$PATH' > /etc/profile.d/conda.sh
RUN conda update -n base conda
RUN conda update openssl ca-certificates certifi
RUN conda config --add channels conda-forge
RUN apt-get install --no-install-recommends ca-certificates -y
#   Install Goodies
ENV NB_USER="geo"
ENV NB_UID="1000"
RUN useradd -m -s /bin/bash -N -u $NB_UID $NB_USER \
 && chown $NB_USER $CONDA_DIR -R \
 && mkdir -p /src \
 && chown $NB_USER /src
USER $NB_USER
ARG python_version=3.6
RUN conda install -y python=${python_version}
RUN conda config --set always_yes yes
RUN pip install pip==23.1 --upgrade
RUN pip install https://cntk.ai/PythonWheel/GPU/cntk-2.1-cp36-cp36m-linux_x86_64.whl
RUN pip install Cython==0.29.34 --no-cache-dir
#  # Base Python Packages
RUN conda install bcolz h5py matplotlib mkl nose notebook pygpu pyyaml six
RUN pip install python-dotenv==1.0.0
#  # Data Science
RUN conda install numpy scipy pandas tqdm colorcet seaborn networkx
#  # Image Processing
RUN conda install Pillow scikit-image
#  # ML Packages
RUN conda install scikit-learn six theano
RUN pip install sklearn_pandas==2.2.0 tensorflow-gpu==2.12.0 tensorboardX==2.6 jupyter-tensorboard==0.2.0 livelossplot==0.5.5
#  # TPOT plus Dependencies
RUN pip install deap==1.3.3 update_checker==0.18.0 tqdm==4.65.0 stopit==1.1.2 xgboost==1.7.5 scikit-mdr==0.4.4 skrebate==0.62 tpot==0.11.7
#  ## Torch (Because you're special)
RUN conda install pytorch torchvision cuda90 -c pytorch \
 && conda clean -ya
RUN pip install git+https://github.com/pytorch/tnt.git@master
#   keras
RUN git clone git://github.com/keras-team/keras.git /src \
 && pip install -e /src[tests] \
 && pip install git+git://github.com/keras-team/keras.git
#  # Geo Packages
RUN conda install geopandas shapely dask
RUN pip install obspy==1.4.0 pynoddy==1.1 gempy==2.2.12 segyio==1.9.11 bruges==0.5.4 welly==0.5.2 fiona==1.9.3 rasterio==1.3.6 simpeg==0.19.0 lasio==0.30 mplstereonet==0.6.2
#  # Package install over
RUN conda clean -yt
COPY theanorc /home/$NB_USER/.theanorc
ENV PYTHONPATH="/src/:$PYTHONPATH"
WORKDIR /src
#   Tensorboard
EXPOSE 6006/tcp
#   Jupyter / iPython
EXPOSE 8888/tcp
CMD jupyter notebook --port=8888 --ip=0.0.0.0
# Please add your HEALTHCHECK here!!!
