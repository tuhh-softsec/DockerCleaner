FROM gassmoeller/aspect
LABEL Timo="Heister <timo.heister@gmail.com>"
USER root
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.9.13p1-1ubuntu2 Dpkg::Options::="--force-confold" -yq -o
#   Install all OS dependencies for fully functional notebook server
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 vim=2:9.0.1000-4ubuntu2 jed=1:0.99.20~pre.178+dfsg-1 emacs=1:28.2+1-13ubuntu3 build-essential=12.9ubuntu3 python-dev unzip=6.0-27ubuntu1 libsm6=2:1.2.3-1build2 pandoc=2.17.1.1-1.1ubuntu1 texlive-latex-base=2022.20230122-2 texlive-latex-extra=2022.20230122-2 texlive-fonts-extra=2022.20230122-2 texlive-fonts-recommended=2022.20230122-2 texlive-generic-recommended texlive-xetex=2022.20230122-2 lmodern=2.005-1 libxrender1=1:0.9.10-1.1 inkscape=1.2.2-2ubuntu1 wget=1.21.3-1ubuntu1 bzip2=1.0.8-5build1 ca-certificates=20230311 sudo=1.9.13p1-1ubuntu2 locales=2.37-0ubuntu2 fonts-liberation=1:1.07.4-11 -yq
#   Install Tini
RUN wget --quiet https://github.com/krallin/tini/releases/download/v0.10.0/tini \
 && echo "1361527f39190a7338a0b434bd8c88ff7233ce7b9a4876f3315c22fce7eca1b0 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini
ENV SHELL="/bin/bash"
ENV CONDA_DIR="/opt/conda"
ENV PATH="$CONDA_DIR/bin:$PATH"
ENV HOME="/home/$USER"
ENV NB_USER="$USER"
RUN mkdir -p $CONDA_DIR \
 && chown $USER $CONDA_DIR
USER $USER
#   Install conda 
RUN cd /tmp \
 && mkdir -p $CONDA_DIR \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda3-4.2.12-Linux-x86_64.sh \
 && echo "c59b3dd3cad550ac7596e0d599b91e75d88826db132e4146030ef471bb434e9a *Miniconda3-4.2.12-Linux-x86_64.sh" | sha256sum -c - \
 && /bin/bash Miniconda3-4.2.12-Linux-x86_64.sh -f -b -p $CONDA_DIR \
 && rm Miniconda3-4.2.12-Linux-x86_64.sh \
 && $CONDA_DIR/bin/conda config --system --add channels conda-forge \
 && $CONDA_DIR/bin/conda config --system --set auto_update_conda false \
 && conda clean -tipsy
#   Install Jupyter Notebook and Hub
RUN conda install --quiet --yes 'notebook=5.0.*' 'jupyterhub=0.7.*' 'jupyterlab=0.18.*' \
 && conda clean -tipsy
RUN conda install --quiet --yes 'nomkl' 'ipywidgets=6.0*' 'pandas=0.19*' 'numexpr=2.6*' 'matplotlib=2.0*' 'scipy=0.19*' 'seaborn=0.7*' 'scikit-learn=0.18*' 'scikit-image=0.12*' 'sympy=1.0*' 'cython=0.25*' 'patsy=0.4*' 'statsmodels=0.8*' 'cloudpickle=0.2*' 'dill=0.2*' 'numba=0.31*' 'bokeh=0.12*' 'sqlalchemy=1.1*' 'hdf5=1.8.17' 'h5py=2.6*' 'vincent=0.4.*' 'beautifulsoup4=4.5.*' 'pytables' 'xlrd' \
 && conda remove --quiet --yes --force qt pyqt \
 && conda clean -tipsy
#   Activate ipywidgets extension in the environment that runs the notebook server
RUN jupyter nbextension enable --py widgetsnbextension --sys-prefix
RUN mkdir -p $HOME/.ipython/profile_default/startup
COPY mplimporthook.py $HOME/.ipython/profile_default/startup/
USER root
EXPOSE 8888/tcp
ENTRYPOINT ["tini", "--"]
CMD ["start-notebook.sh"]
COPY start.sh /usr/local/bin/
COPY start-notebook.sh /usr/local/bin/
#  COPY start-singleuser.sh /usr/local/bin/
COPY jupyter_notebook_config.py /home/$NB_USER/.jupyter/
RUN chown -R $NB_USER:users /home/$NB_USER/.jupyter
COPY demo.ipynb /home/$NB_USER/
RUN chown $NB_USER:users /home/$NB_USER/demo.ipynb
WORKDIR /home/$USER/
USER $USER
RUN jupyter trust /home/$USER/*.ipynb
#   enable widgets:
RUN jupyter nbextension enable --py --sys-prefix widgetsnbextension
# Please add your HEALTHCHECK here!!!
