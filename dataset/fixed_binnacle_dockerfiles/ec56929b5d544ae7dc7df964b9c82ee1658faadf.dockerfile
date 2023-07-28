#   Debian Jessie debootstrap from 2017-02-27
FROM debian@sha256:52af198afd8c264f1035206ca66a5c48e602afb32dc912ebf9e9478134601ec4
MAINTAINER Jupyter
USER root
#   Install all OS dependencies for notebook server that starts but lacks all
#   features (e.g., download as all possible file formats)
ENV DEBIAN_FRONTEND="noninteractive"
RUN REPO=http://cdn-fastly.deb.debian.org \
 && echo "deb $REPO/debian jessie main\ndeb $REPO/debian-security jessie/updates main" > /etc/apt/sources.list \
 && apt-get update \
 && apt-get -yq dist-upgrade \
 && apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 bzip2=1.0.8-5build1 ca-certificates=20230311 sudo=1.9.13p1-1ubuntu2 locales=2.37-0ubuntu2 git=1:2.39.2-1ubuntu1 -yq \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen
#   Install Tini
RUN wget --quiet https://github.com/krallin/tini/releases/download/v0.10.0/tini \
 && echo "1361527f39190a7338a0b434bd8c88ff7233ce7b9a4876f3315c22fce7eca1b0 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini
#   Configure environment
ENV CONDA_DIR="/opt/conda"
ENV PATH="$CONDA_DIR/bin:$PATH"
ENV SHELL="/bin/bash"
ENV NB_USER="jovyan"
ENV NB_UID="1000"
ENV HOME="/home/$NB_USER"
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#   Create jovyan user with UID=1000 and in the 'users' group
RUN useradd -m -s /bin/bash -N -u $NB_UID $NB_USER \
 && mkdir -p $CONDA_DIR \
 && chown $NB_USER $CONDA_DIR
USER $NB_USER
#   Setup jovyan home directory
RUN mkdir /home/$NB_USER/work \
 && mkdir /home/$NB_USER/.jupyter \
 && echo "cacert=/etc/ssl/certs/ca-certificates.crt" > /home/$NB_USER/.curlrc
ENV CONDA_VERSION="4.3.11"
#   https://repo.continuum.io/miniconda/Miniconda3-4.3.11-Linux-x86_64.sh
#   Install conda as jovyan
RUN cd /tmp \
 && mkdir -p $CONDA_DIR \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda3-${CONDA_VERSION}-Linux-x86_64.sh \
 && /bin/bash Miniconda3-${CONDA_VERSION}-Linux-x86_64.sh -f -b -p $CONDA_DIR \
 && rm Miniconda3-${CONDA_VERSION}-Linux-x86_64.sh \
 && $CONDA_DIR/bin/conda config --system --add channels conda-forge \
 && $CONDA_DIR/bin/conda config --system --set auto_update_conda false \
 && conda clean -tipsy
#   Install Jupyter Notebook and Hub
RUN conda install --quiet --yes 'python=3.6.1' 'notebook' 'jupyterhub' 'jupyterlab' \
 && conda clean -tipsy
RUN /opt/conda/bin/pip install rise \
 && jupyter-nbextension install rise --py --sys-prefix \
 && jupyter-nbextension enable rise --py --sys-prefix
USER root
EXPOSE 8888/tcp
WORKDIR /home/$NB_USER/work
#   Configure container startup
ENTRYPOINT ["tini", "--"]
CMD ["start-notebook.sh"]
#   Add local files as late as possible to avoid cache busting
COPY start-notebook.sh /usr/local/bin/
COPY start.sh /usr/local/bin/
COPY lectures.sh /usr/local/bin/
COPY start-singleuser.sh /usr/local/bin/
COPY jupyter_notebook_config.py /home/$NB_USER/.jupyter/
RUN chown -R $NB_USER:users /home/$NB_USER/.jupyter
#   Personal keybindings
COPY .config/main.js /opt/conda/share/jupyter/nbextensions/rise
#   Switch back to jovyan to avoid accidental container runs as root
USER $NB_USER
RUN pip install pip==23.1 howdoi==2.0.20 beeprint==2.4.10 mypy==1.2.0 --upgrade
# Please add your HEALTHCHECK here!!!
