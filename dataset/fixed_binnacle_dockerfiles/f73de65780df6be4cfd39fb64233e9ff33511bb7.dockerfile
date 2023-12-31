#   Copyright 2015 Google Inc. All rights reserved.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   We use different base images for GPU vs CPU Dockerfiles, so we expect
#   that the appropriate image is pulled and tagged locally.
#   CPU should use ubuntu:16.04
#   and GPU uses nvidia/cuda:9.1-cudnn7-devel-ubuntu16.04
FROM datalab-external-base-image
MAINTAINER Google Cloud DataLab
#   Container configuration
EXPOSE 8080/tcp
#   Path configuration
ENV DATALAB_CONDA_DIR="/usr/local"
ENV PATH="$PATH:/tools/node/bin:/tools/google-cloud-sdk/bin:$DATALAB_CONDA_DIR/bin"
ENV PYTHON_2_ENV="py2env"
ENV PYTHON_3_ENV="py3env"
#   Needed to run "source" for switching Conda environments.
SHELL ["/bin/bash", "-c"]
#   Setup OS and core packages
RUN echo "deb-src http://ftp.us.debian.org/debian testing main" >> /etc/apt/sources.list \
 && apt-get update -y \
 && apt-get install --no-install-recommends debian-archive-keyring=2021.1.1ubuntu2 debian-keyring=2022.12.24 -y -q \
 && apt-get update -y \
 && apt-get install --no-install-recommends build-essential=12.9ubuntu3 ca-certificates=20230311 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 locales=2.37-0ubuntu2 openssh-client=1:9.0p1-1ubuntu8 pkg-config=1.8.1-1ubuntu2 unzip=6.0-27ubuntu1 wget=1.21.3-1ubuntu1 zip=3.0-13 -y -q \
 && mkdir -p /tools \
 && mkdir -p /srcs \
 && cd /srcs \
 && apt-get source -d wget git python-zmq ca-certificates pkg-config libpng-dev \
 && cd / \
 && locale-gen en_US.UTF-8 \
 && update-locale LANG=en_US.UTF-8 \
 && wget --quiet -O ~/miniconda.sh http://repo.continuum.io/miniconda/Miniconda-latest-Linux-x86_64.sh \
 && chmod +x ~/miniconda.sh \
 && ~/miniconda.sh -b -f -p $DATALAB_CONDA_DIR \
 && rm ~/miniconda.sh \
 && conda update conda --quiet --yes \
 && conda config --system --append channels conda-forge \
 && conda config --system --set show_channel_urls true \
 && conda update --all --quiet --yes \
 && conda create --yes --quiet --name $PYTHON_2_ENV python=2.7 crcmod==1.7 dask==0.17.1 dill==0.2.6 future==0.16.0 futures==3.2.0 google-api-python-client==1.6.2 httplib2==0.10.3 h5py==2.7.1 ipykernel==4.8.2 ipywidgets==7.2.1 jinja2==2.8 jsonschema==2.6.0 matplotlib==2.1.2 mock==2.0.0 nltk==3.2.1 numpy==1.14.0 oauth2client==2.2.0 pandas-gbq==0.3.0 pandas==0.22.0 pandas-profiling==1.4.2 pandocfilters==1.4.2 pillow==5.0.0 pip==18.1 plotly==1.12.5 psutil==4.3.0 pygments==2.1.3 python-dateutil==2.5.0 python-snappy==0.5.1 pytz==2018.4 pyyaml==3.13 pyzmq==17.1.0 requests==2.18.4 scikit-image==0.13.0 scikit-learn==0.19.1 scipy==1.0.0 seaborn==0.7.0 six==1.11.0 statsmodels==0.8.0 sympy==0.7.6.1 tornado==4.5.1 widgetsnbextension==3.2.1 xgboost==0.6a2 \
 && source activate $PYTHON_2_ENV \
 && pip install only-if-needed==null apache-airflow==1.9.0 bs4==0.0.1 ggplot==0.6.8 google-cloud-dataflow==2.0.0 google-cloud-monitoring==0.28.0 lime==0.1.1.23 protobuf==3.6.1 tensorflow==1.8.0 --quiet -U --upgrade-strategy --no-cache-dir \
 && source deactivate \
 && conda clean -tipsy \
 && unset OLDPWD \
 && conda create --yes --quiet --name $PYTHON_3_ENV python=3.5 crcmod==1.7 dask==0.17.1 dill==0.2.6 google-api-python-client==1.6.2 httplib2==0.10.3 h5py==2.7.1 ipykernel==4.8.2 ipywidgets==7.2.1 jinja2==2.8 jsonschema==2.6.0 matplotlib==2.1.2 mock==2.0.0 nltk==3.2.1 notebook==5.6.0 numpy==1.14.0 oauth2client==2.2.0 pandas-gbq==0.3.0 pandas==0.22.0 pandocfilters==1.4.2 pillow==5.0.0 pip==18.0 plotly==1.12.5 psutil==4.3.0 pygments==2.1.3 python-dateutil==2.5.0 python-snappy==0.5.1 pytz==2018.4 pyzmq==17.1.0 requests==2.18.4 scikit-image==0.13.0 scikit-learn==0.19.1 scipy==1.0.0 seaborn==0.7.0 six==1.11.0 statsmodels==0.8.0 sympy==0.7.6.1 tornado==4.5.1 widgetsnbextension==3.2.1 xgboost==0.6a2 \
 && cp /usr/local/envs/py3env/bin/pip /usr/local/envs/py3env/bin/pip3 \
 && source deactivate \
 && source activate $PYTHON_2_ENV \
 && python -m ipykernel install --prefix=/usr/local/envs/py3env \
 && source deactivate \
 && conda clean -tipsy \
 && find $DATALAB_CONDA_DIR/envs/*/lib -type d -name tests | grep -v h5py | xargs rm -rf \
 && cd / \
 && mkdir -p /tools/node \
 && wget -nv https://nodejs.org/dist/v6.10.0/node-v6.10.0-linux-x64.tar.gz -O node.tar.gz \
 && tar xzf node.tar.gz -C /tools/node --strip-components=1 \
 && rm node.tar.gz \
 && wget -nv https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.zip \
 && unzip -qq google-cloud-sdk.zip -d tools \
 && rm google-cloud-sdk.zip \
 && tools/google-cloud-sdk/install.sh --usage-reporting=false --path-update=false --bash-completion=false --disable-installation-options \
 && tools/google-cloud-sdk/bin/gcloud -q components update gcloud core bq gsutil compute preview alpha beta \
 && tools/google-cloud-sdk/bin/gcloud config set component_manager/disable_update_check true \
 && touch /tools/google-cloud-sdk/lib/third_party/google.py \
 && /tools/node/bin/npm install bunyan@1.7.1 http-proxy@1.13.2 mkdirp@0.5.1 node-cache@3.2.0 node-uuid@1.4.7 tcp-port-used@0.1.2 ws@1.1.4 \
 && cd / \
 && /tools/node/bin/npm install -g forever \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* \
 && rm -rf /root/.cache/* \
 && rm -rf /usr/share/locale/* \
 && rm -rf /usr/share/i18n/locales/*
#   Install Python3 packages that aren't available or up-to-date in Conda.
#   For some reasons, merging it with the commands above does not work so creating a separate one.
RUN source activate $PYTHON_3_ENV \
 && pip install only-if-needed==null apache-airflow==1.9.0 bs4==0.0.1 ggplot==0.6.8 google-cloud-monitoring==0.28.0 lime==0.1.1.23 protobuf==3.6.1 tensorflow==1.8.0 --quiet -U --upgrade-strategy --no-cache-dir
ENV LANG="en_US.UTF-8"
#   Copy local configuration files
COPY config/ipython.py /etc/ipython/ipython_config.py
COPY config/nbconvert.py /etc/jupyter/jupyter_notebook_config.py
#   Directory "py" may be empty and in that case it will git clone pydatalab from repo
COPY pydatalab /datalab/lib/pydatalab
COPY nbconvert /datalab/nbconvert
RUN if [ -d /datalab/lib/pydatalab/.git ] ; then echo "use local lib" ; else git clone https://github.com/googledatalab/pydatalab.git /datalab/lib/pydatalab ; fi \
 && cd /datalab/lib/pydatalab \
 && /tools/node/bin/npm install -g typescript@3.0.3 \
 && tsc --module amd --noImplicitAny --outdir datalab/notebook/static datalab/notebook/static/*.ts \
 && tsc --module amd --noImplicitAny --outdir google/datalab/notebook/static google/datalab/notebook/static/*.ts \
 && /tools/node/bin/npm uninstall -g typescript \
 && cd /datalab/lib/pydatalab \
 && source activate $PYTHON_2_ENV \
 && pip install only-if-needed==null . --upgrade-strategy --no-cache-dir \
 && pip install only-if-needed==null /datalab/lib/pydatalab/solutionbox/image_classification/. --upgrade-strategy \
 && pip install only-if-needed==null /datalab/lib/pydatalab/solutionbox/structured_data/. --upgrade-strategy \
 && source deactivate \
 && source activate $PYTHON_3_ENV \
 && pip install only-if-needed==null . --upgrade-strategy --no-cache-dir \
 && pip install only-if-needed==null /datalab/lib/pydatalab/solutionbox/image_classification/. --upgrade-strategy \
 && pip install only-if-needed==null /datalab/lib/pydatalab/solutionbox/structured_data/. --upgrade-strategy \
 && pip install only-if-needed==null jupyter_highlight_selected_word==0.2.0 --upgrade-strategy \
 && jupyter nbextension install --py datalab.notebook \
 && jupyter nbextension install --py google.datalab.notebook \
 && jupyter nbextension install --py jupyter_highlight_selected_word \
 && jupyter nbextension enable --sys-prefix --py jupyter_highlight_selected_word \
 && jupyter nbextension enable --sys-prefix --py widgetsnbextension \
 && source deactivate \
 && rm datalab/notebook/static/*.js google/datalab/notebook/static/*.js \
 && mkdir -p /datalab/nbconvert \
 && cp -R /usr/local/share/jupyter/nbextensions/gcpdatalab/* /datalab/nbconvert \
 && ln -s $DATALAB_CONDA_DIR/envs/$PYTHON_3_ENV/lib/python3.5/site-packages/notebook/static/custom/custom.css /datalab/nbconvert/custom.css
#   Add third party license files
RUN mkdir -p /tools/license
COPY license.sh /tools/license
COPY third_party_licenses.csv /tools/license
RUN mkdir /usr/licenses \
 && /tools/license/license.sh /tools/license/third_party_licenses.csv /usr/licenses
COPY config/py2-kernel.json $DATALAB_CONDA_DIR/envs/$PYTHON_3_ENV/share/jupyter/kernels/python2/kernel.json
COPY config/py3-kernel.json $DATALAB_CONDA_DIR/envs/$PYTHON_3_ENV/share/jupyter/kernels/python3/kernel.json
COPY config/py2-kernel-startup.sh $DATALAB_CONDA_DIR/envs/$PYTHON_3_ENV/share/jupyter/kernels/python2/kernel-startup.sh
COPY config/py3-kernel-startup.sh $DATALAB_CONDA_DIR/envs/$PYTHON_3_ENV/share/jupyter/kernels/python3/kernel-startup.sh
RUN chmod 755 $DATALAB_CONDA_DIR/envs/$PYTHON_3_ENV/share/jupyter/kernels/python2/kernel-startup.sh \
 && chmod 755 $DATALAB_CONDA_DIR/envs/$PYTHON_3_ENV/share/jupyter/kernels/python3/kernel-startup.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
