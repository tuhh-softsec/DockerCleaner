FROM nvidia/cuda:8.0-cudnn5-devel-ubuntu14.04
MAINTAINER Fernando Andreotti <fernando.andreotti@eng.ox.ac.uk>
#   Versions used for some packages
ARG CONDA_VERSION=4.3.1
ARG CONDA_ENV
ARG TENSORFLOW_VERSION=1.4*
ARG KERAS_VERSION=2.0.9
ARG PYTHON_VERSION=3.5
ENTRYPOINT ["/bin/bash", "-c"]
USER root
RUN echo -e "\n**********************\nNVIDIA Driver Version\n**********************\n" \
 && cat /proc/driver/nvidia/version \
 && echo -e "\n**********************\nCUDA Version\n**********************\n" \
 && nvcc -V \
 && echo -e "\n\nBuilding your Deep Learning Docker Image...\n"
#   Install some dependencies
ENV DEBIAN_FRONTEND="noninteractive"
ENV CONDA_ENV_PATH="/opt/miniconda"
ENV MYCONDA_ENV="\"deeplearn\""
ENV CONDA_ACTIVATE="\"source $CONDA_ENV_PATH/bin/activate $MYCONDA_ENV\""
#   install essentials
RUN apt-get update --fix-missing -qq \
 && apt-get install --no-install-recommends autoconf automake bc build-essential bzip2 cmake curl g++ gfortran git language-pack-en libatlas-dev libatlas3gf-base libcurl4-openssl-dev libffi-dev libfreetype6-dev libglib2.0-0 libhdf5-dev liblcms2-dev libopenblas-dev libssl-dev libtiff5-dev libtool libwebp-dev libzmq3-dev make nano pkg-config software-properties-common unzip wget zlib1g-dev qt5-default libvtk6-dev zlib1g-dev -y
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
RUN conda install -c conda-forge -y -n $MYCONDA_ENV 'beautifulsoup4=4.5*' 'graphviz=2.38.0' 'hdf5=1.8.17' 'h5py=2.7*' 'ipython=5.1*' 'ipykernel=4.5*' 'ipywidgets=5.2*' 'jupyter=1.0*' 'lxml=3.8*' 'matplotlib=2.0*' 'notebook=4.3*' 'numpy=1.13*' 'pandas=0.20*' 'pillow=4.2*' 'pip=9.0*' 'pydotplus=2.0.2' 'python=3.5*' 'scipy=0.19*' 'scikit-learn=0.19*' 'scikit-image=0.13*' 'setuptools=36.3*' 'six=1.10*' 'sphinx=1.5*' 'spyder=3.2*' \
 && conda clean -tipsy
#   Some further python libraries
#  RUN conda install -c conda-forge -n $MYCONDA_ENV r-r.utils r-lme4 r-nlme
RUN conda install -c glemaitre -n $MYCONDA_ENV imbalanced-learn
#   Install TensorFlow
#  RUN conda install -c conda-forge -n $MYCONDA_ENV tensorflow=${TENSORFLOW_VERSION}
RUN conda install -c conda-forge -n $MYCONDA_ENV tensorflow-gpu=${TENSORFLOW_VERSION}
#   Install Keras
ENV KERAS_BACKEND="tensorflow"
RUN conda install -c conda-forge -n $MYCONDA_ENV keras=${KERAS_VERSION}
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
RUN conda info --envs
RUN sed -i 's/theano/tensorflow/g' ${CONDA_ENV_PATH}/envs/${MYCONDA_ENV}/etc/conda/activate.d/keras_activate.sh
RUN $CONDA_ACTIVATE \
 && pip install pip==23.1 --upgrade \
 && pip install git+git://github.com/stared/keras-sequential-ascii.git pydot3==1.0.9
#  ##########################
#  # Finishing up details ###
#  ##########################
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
