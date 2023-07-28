FROM ubuntu:14.04
MAINTAINER Fernando Andreotti <fernando.andreotti@eng.ox.ac.uk>
#   Versions used for some packages
ARG CONDA_VERSION=4.3.1
ARG CONDA_ENV
ARG TENSORFLOW_VERSION=1.0*
ARG KERAS_VERSION=2.0.2
ARG PYTHON_VERSION=3.5
ENTRYPOINT ["/bin/bash", "-c"]
USER root
#   Install some dependencies
ENV DEBIAN_FRONTEND="noninteractive"
ENV CONDA_ENV_PATH="/opt/miniconda"
ENV MYCONDA_ENV="\"deeplearn\""
ENV CONDA_ACTIVATE="\"source $CONDA_ENV_PATH/bin/activate $MYCONDA_ENV\""
RUN apt-get update --fix-missing -qq \
 && apt-get install --no-install-recommends autoconf=2.69-6 automake=1:1.14.1-2ubuntu1 bc=1.06.95-8ubuntu1 build-essential=11.6ubuntu6 bzip2=1.0.6-5 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 g++=4:4.8.2-1ubuntu6 gfortran=4:4.8.2-1ubuntu6 git=1:1.9.1-1ubuntu0.10 language-pack-en=1:14.04+20160720 libatlas-dev=3.10.1-4 libatlas3gf-base=3.10.1-4 libcurl4-openssl-dev=7.35.0-1ubuntu2.20 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libfreetype6-dev=2.5.2-1ubuntu2.8 libglib2.0-0=2.40.2-0ubuntu1.1 libhdf5-dev=1.8.11-5ubuntu7.1 liblcms2-dev=2.5-0ubuntu4.2 libopenblas-dev=0.2.8-6ubuntu1 libssl-dev=1.0.1f-1ubuntu2.27 libtiff5-dev=4.0.3-7ubuntu0.11 libtool=2.4.2-1.7ubuntu1 libwebp-dev=0.4.0-4 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 make=3.81-8.2ubuntu3 nano=2.2.6-1ubuntu1 pkg-config=0.26-1ubuntu4 software-properties-common=0.92.37.8 unzip=6.0-9ubuntu1.5 wget=1.15-1ubuntu1.14.04.5 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 qt5-default=5.2.1+dfsg-1ubuntu14.3 libvtk6-dev=6.0.0-6 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 -y
#   Install miniconda to /opt/miniconda
RUN curl -LO http://repo.continuum.io/miniconda/Miniconda-latest-Linux-x86_64.sh
RUN /bin/bash Miniconda-latest-Linux-x86_64.sh -p $CONDA_ENV_PATH -b
RUN rm Miniconda-latest-Linux-x86_64.sh
ENV PATH="$CONDA_ENV_PATH/bin:${PATH}"
RUN conda update --quiet --yes conda
ENV PATH="${CONDA_ENV_PATH}/bin:$PATH"
#   Creating Anaconda environment
RUN conda create -y --name $MYCONDA_ENV python=${PYTHON_VERSION}
#   Install Python 3 packages
RUN conda install -c conda-forge -y -n $MYCONDA_ENV 'beautifulsoup4=4.5*' 'hdf5=1.8.17' 'h5py=2.7*' 'ipython=5.1*' 'ipykernel=4.5*' 'ipywidgets=5.2*' 'jupyter=1.0*' 'lxml=3.8*' 'matplotlib=2.0*' 'notebook=4.3*' 'numpy=1.12*' 'pandas=0.20*' 'pillow=4.2*' 'pip=9.0*' 'python=3.5*' 'rpy2=2.8*' 'scipy=0.19*' 'scikit-learn=0.19*' 'scikit-image=0.13*' 'setuptools=36.3*' 'six=1.10*' 'sphinx=1.5*' 'spyder=3.1*' \
 && conda clean -tipsy
#   Some R dependencies
RUN conda install -c conda-forge -n $MYCONDA_ENV r-r.utils r-lme4 r-nlme
#   Install TensorFlow
RUN conda install -c conda-forge -n $MYCONDA_ENV tensorflow=${TENSORFLOW_VERSION}
#   Install Keras
ENV KERAS_BACKEND="tensorflow"
RUN conda install -c conda-forge -n $MYCONDA_ENV keras=${KERAS_VERSION}
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
RUN conda info --envs
RUN sed -i 's/theano/tensorflow/g' ${CONDA_ENV_PATH}/envs/${MYCONDA_ENV}/etc/conda/activate.d/keras_activate.sh
#   Visualization tools
RUN conda install -c conda-forge -y -n $MYCONDA_ENV graphviz=2.38.0 pydotplus=2.0.2
RUN $CONDA_ACTIVATE \
 && pip install pip==23.1 --upgrade \
 && pip install git+git://github.com/stared/keras-sequential-ascii.git pydot3==1.0.9
#  ###################
#  # Running tests ###
#  ###################
ENV HOME="/sharedfolder"
WORKDIR /sharedfolder
#   Add a notebook profile.
RUN mkdir -p -m 700 /sharedfolder/.jupyter/ \
 && echo "c.NotebookApp.ip = '*'" echo "c.NotebookApp.port = 8888" echo "c.NotebookApp.open_browser = False" echo "c.MultiKernelManager.default_kernel_name = 'python3'" echo "c.NotebookApp.allow_root = True" echo "c.NotebookApp.password_required = False" echo "c.NotebookApp.token = ''" >> /sharedfolder/.jupyter/jupyter_notebook_config.py >> /sharedfolder/.jupyter/jupyter_notebook_config.py >> /sharedfolder/.jupyter/jupyter_notebook_config.py >> /sharedfolder/.jupyter/jupyter_notebook_config.py >> /sharedfolder/.jupyter/jupyter_notebook_config.py >> /sharedfolder/.jupyter/jupyter_notebook_config.py >> /sharedfolder/.jupyter/jupyter_notebook_config.py
#   Expose Ports for TensorBoard (6006), Ipython (8888)
EXPOSE 6006/tcp 8888/tcp
CMD ["source", "activate", "deeplearn", "&&", "/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
