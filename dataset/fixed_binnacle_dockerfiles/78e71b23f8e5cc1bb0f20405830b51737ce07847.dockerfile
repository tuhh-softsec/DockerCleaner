#
#   Licensed to the Apache Software Foundation (ASF) under one or more
#   contributor license agreements.  See the NOTICE file distributed with
#   this work for additional information regarding copyright ownership.
#   The ASF licenses this file to You under the Apache License, Version 2.0
#   (the "License"); you may not use this file except in compliance with
#   the License.  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
FROM predictionio/pio:latest
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.9ubuntu3 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 gcc=4:12.2.0-3ubuntu1 make=4.3-4.1build1 openssl=3.0.8-1ubuntu1 libssl-dev=3.0.8-1ubuntu1 libbz2-dev=1.0.8-5build1 apt-transport-https=2.6.0 ca-certificates=20230311 g++=4:12.2.0-3ubuntu1 gnupg=2.2.40-1ubuntu2 graphviz=2.42.2-7build3 lsb-release=12.0-1ubuntu1 openssh-client=1:9.0p1-1ubuntu8 zip=3.0-13 libreadline-dev=8.2-1.3 libsqlite3-dev=3.40.1-1 cmake=3.25.1-1 libxml2-dev=2.9.14+dfsg-1.1build2 wget=1.21.3-1ubuntu1 bzip2=1.0.8-5build1 sudo=1.9.13p1-1ubuntu2 vim=2:9.0.1000-4ubuntu2 unzip=6.0-27ubuntu1 locales=2.37-0ubuntu2 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen
ENV LC_ALL="en_US.UTF-8" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    NB_USER="jovyan" \
    NB_UID="1000" \
    NB_GID="100" \
    CONDA_DIR="/opt/conda" \
    PIP_DEFAULT_TIMEOUT="180"
ENV PATH="$CONDA_DIR/bin:$PATH" \
    HOME="/home/$NB_USER"
COPY fix-permissions /usr/local/bin/fix-permissions
RUN chmod +x /usr/local/bin/fix-permissions \
 && groupadd wheel -g 11 \
 && echo "auth required pam_wheel.so use_uid" >> /etc/pam.d/su \
 && useradd -m -s /bin/bash -N -u $NB_UID $NB_USER \
 && mkdir -p $CONDA_DIR \
 && chmod g+w /etc/passwd \
 && fix-permissions $HOME \
 && fix-permissions $CONDA_DIR
USER $NB_USER
ENV MINICONDA_VERSION="4.4.10"
RUN wget -q https://repo.continuum.io/miniconda/Miniconda3-${MINICONDA_VERSION}-Linux-x86_64.sh -O /tmp/miniconda.sh \
 && echo 'bec6203dbb2f53011e974e9bf4d46e93 */tmp/miniconda.sh' | md5sum -c - \
 && bash /tmp/miniconda.sh -f -b -p $CONDA_DIR \
 && rm /tmp/miniconda.sh \
 && conda config --system --prepend channels conda-forge \
 && conda config --system --set auto_update_conda false \
 && conda config --system --set show_channel_urls true \
 && conda install --quiet --yes conda="${MINICONDA_VERSION%.*}.*" \
 && conda update --all --quiet --yes \
 && conda clean -tipsy \
 && rm -rf /home/$NB_USER/.cache/yarn \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
RUN conda install --quiet --yes 'tini=0.18.0' \
 && conda list tini | grep tini | tr -s ' ' | cut -d ' ' -f 1,2 >> $CONDA_DIR/conda-meta/pinned \
 && conda clean -tipsy \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
RUN conda install --quiet --yes 'notebook=5.6.*' 'jupyterlab=0.34.*' nodejs \
 && jupyter labextension install @jupyterlab/hub-extension@^0.11.0 \
 && jupyter notebook --generate-config \
 && conda clean -tipsy \
 && npm cache clean --force \
 && rm -rf $CONDA_DIR/share/jupyter/lab/staging \
 && rm -rf /home/$NB_USER/.cache/yarn \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
COPY requirements.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt \
 && fix-permissions $CONDA_DIR \
 && fix-permissions /home/$NB_USER
COPY jupyter_notebook_config.py /home/$NB_USER/.jupyter/
COPY start*.sh /usr/local/bin/
USER root
RUN chmod +x /usr/local/bin/*.sh
EXPOSE 8888/tcp
WORKDIR $HOME
ENTRYPOINT ["tini", "--"]
CMD ["/usr/local/bin/start-jupyter.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
