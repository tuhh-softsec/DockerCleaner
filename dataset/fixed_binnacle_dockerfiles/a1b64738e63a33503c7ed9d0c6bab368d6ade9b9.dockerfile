#   Copyright (c) Jupyter Development Team.
#   Distributed under the terms of the Modified BSD License.
FROM jupyter/tensorflow-notebook:latest
LABEL maintainer="Florian JUDITH <florian.judith.b@gmail.com"
USER root
#   Install all OS dependencies for notebook server that starts but lacks all
#   features (e.g., download as all possible file formats)
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 curl=7.88.1-7ubuntu1 g++=4:12.2.0-3ubuntu1 jed=1:0.99.20~pre.178+dfsg-1 libav-tools libcupti-dev=11.8.87~11.8.0-2ubuntu1 lsb-release=12.0-1ubuntu1 openssh-client=1:9.0p1-1ubuntu8 pkg-config=1.8.1-1ubuntu2 python python-dev unzip=6.0-27ubuntu1 vim=2:9.0.1000-4ubuntu2 zip=3.0-13 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 -yq \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Install ksonnet
RUN wget --quiet https://github.com/ksonnet/ksonnet/releases/download/v0.9.2/ks_0.9.2_linux_amd64.tar.gz \
 && tar -zvxf ks_0.9.2_linux_amd64.tar.gz \
 && mv ks_0.9.2_linux_amd64/ks /usr/local/bin/ks \
 && rm -rf ks_0.9.2_linux_amd64* \
 && chmod +x /usr/local/bin/ks
#   Install CUDA Profile Tools and other python packages
RUN pip install Pillow==9.5.0 h5py==3.8.0 ipykernel==6.22.0 matplotlib==3.7.1 numpy==1.24.2 scipy==1.10.1 sklearn==0.0.post4 kubernetes==26.1.0 grpcio==1.53.0 ktext==0.40 annoy==1.17.2 nltk==3.8.1 pydot==1.4.2 pydot-ng==2.0.0 graphviz==0.20.1 --no-cache-dir \
 && python -m ipykernel.kernelspec
#   Install Python 3 packages
#   Remove pyqt and qt pulled in for matplotlib since we're only ever going to
#   use notebook-friendly backends in these images
RUN conda install --quiet --yes 'nomkl' 'ipywidgets=6.0*' 'pandas=0.22*' 'numexpr=2.6*' 'matplotlib=2.0*' 'scipy=0.19*' 'seaborn=0.7*' 'scikit-learn=0.18*' 'scikit-image=0.12*' 'sympy=1.0*' 'cython=0.25*' 'patsy=0.4*' 'statsmodels=0.8*' 'cloudpickle=0.2*' 'dill=0.2*' 'numba=0.31*' 'bokeh=0.12*' 'sqlalchemy=1.1*' 'hdf5=1.8.17' 'h5py=2.6*' 'vincent=0.4.*' 'beautifulsoup4=4.5.*' 'xlrd' \
 && conda remove --quiet --yes --force qt pyqt \
 && conda clean -tipsy
#   Install graphviz package
RUN apt-get update \
 && apt-get install --no-install-recommends graphviz=2.42.2-7build3 -yq \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Install Oracle Java 8
RUN apt-get update \
 && apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 unzip=6.0-27ubuntu1 python-pip python-sklearn python-pandas python-numpy python-matplotlib software-properties-common=0.99.35 python-software-properties -y \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update -q \
 && echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y oracle-java8-installer \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Install H2O.3
RUN pip install requests==2.28.2 tabulate==0.9.0 scikit-learn==1.2.2 colorama==0.4.6 future==0.18.3 --no-cache-dir
RUN pip --no-cache-dir --trusted-host h2o-release.s3.amazonaws.com install -f http://h2o-release.s3.amazonaws.com/h2o/latest_stable_Py.html h2o
ENV CLOUD_SDK_VERSION="168.0.0"
RUN export CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb https://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" > /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends kubectl google-cloud-sdk=${CLOUD_SDK_VERSION}-0 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && gcloud config set core/disable_usage_reporting true \
 && gcloud config set component_manager/disable_update_check true \
 && gcloud config set metrics/environment github_docker_image
#   Activate ipywidgets extension in the environment that runs the notebook server
RUN jupyter nbextension enable --py widgetsnbextension --sys-prefix
RUN curl -L -o bazel.sh https://github.com/bazelbuild/bazel/releases/download/0.8.0/bazel-0.8.0-installer-linux-x86_64.sh \
 && chmod a+x ./bazel.sh \
 && ./bazel.sh \
 && rm ./bazel.sh
SHELL ["/bin/bash", "-c"]
RUN git clone https://github.com/tensorflow/models.git /home/$NB_USER/tensorflow-models \
 && git clone https://github.com/tensorflow/benchmarks.git /home/$NB_USER/tensorflow-benchmarks
#   Import matplotlib the first time to build the font cache.
ENV XDG_CACHE_HOME="/home/$NB_USER/.cache/"
RUN pip install jupyter-tensorboard==0.2.0
#   Create a conda environment for Python 2. We want to include as many of the
#   packages from our root environment as we reasonably can, so we explicitly
#   list that environment, then include everything unless it is Conda (which
#   can only be in the root environment), Jupyterhub (which requires Python 3),
#   or Python itself. We also want to include the pip packages, but we cannot
#   install those via conda, so we list them, drop any conda packages, and
#   then install them via pip. We do this on a best-effort basis, so if any
#   packages from the Python 3 environment cannot be installed with Python 2,
#   then we just skip them.
RUN conda_packages=$( conda list -e | cut -d '=' -f 1 | grep -v '#' | sort ;) \
 && pip_packages=$( pip --no-cache-dir list --format=freeze | cut -d '=' -f 1 | grep -v '#' | sort ;) \
 && pip_only_packages=$( comm -23 <(echo "${pip_packages}" ) <(echo "${conda_packages}" ) ;) \
 && conda create -n ipykernel_py2 python=2 --file <(echo "${conda_packages}" | grep -v conda | grep -v python | grep -v jupyterhub ) \
 && source activate ipykernel_py2 \
 && python -m ipykernel install --user \
 && echo "${pip_only_packages}" | xargs -n 1 -I "{}" /bin/bash -c 'pip install --no-cache-dir {} || true' \
 && pip install tensorflow-transform==1.13.0 --no-cache-dir \
 && source deactivate
RUN chown -R $NB_USER:users /etc/jupyter/ \
 && chown -R $NB_USER /home/$NB_USER/ \
 && chmod a+rx /usr/local/bin/* \
 && fix-permissions /etc/jupyter/ \
 && fix-permissions /home/$NB_USER/
USER $NB_UID
# Please add your HEALTHCHECK here!!!
