#  Copyright (c) Jupyter Development Team.
#  Distributed under the terms of the Modified BSD License.
#  Debian Jessie image released 2016 March 01.
#  Changed for py 2.7
FROM debian@sha256:a9c958be96d7d40df920e7041608f2f017af81800ca5ad23e327bc402626b58e
MAINTAINER Jupyter Project <jupyter@googlegroups.com>
USER root
#  Install all OS dependencies for fully functional notebook server
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends git jed wget build-essential python-dev ca-certificates bzip2 unzip libsm6 pandoc texlive-latex-base texlive-latex-extra texlive-fonts-extra texlive-fonts-recommended texlive-generic-recommended sudo locales libxrender1 -yq \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen
#  Install Tini
RUN wget --quiet https://github.com/krallin/tini/releases/download/v0.9.0/tini \
 && echo "faafbfb5b079303691a939a747d7f60591f2143164093727e870b289a44d9872 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini
#  Configure environment
ENV CONDA_DIR="/opt/conda"
ENV PATH="$CONDA_DIR/bin:$PATH"
ENV SHELL="/bin/bash"
ENV NB_USER="moc"
ENV NB_UID="1000"
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#  Create moc user with UID=1000 and in the 'users' group
RUN useradd -m -s /bin/bash -N -u $NB_UID $NB_USER \
 && mkdir -p /opt/conda \
 && chown moc /opt/conda
USER moc
#  Setup moc home directory
RUN mkdir /home/$NB_USER/work \
 && mkdir /home/$NB_USER/.jupyter \
 && mkdir /home/$NB_USER/.local \
 && echo "cacert=/etc/ssl/certs/ca-certificates.crt" > /home/$NB_USER/.curlrc
#  Install conda as moc
RUN cd /tmp \
 && mkdir -p $CONDA_DIR \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda2-3.19.0-Linux-x86_64.sh \
 && echo "646b4d5398f8d76a0664375ee6226611c43ee3d49de3eb03efe7480e3c3b9ebf *Miniconda2-3.19.0-Linux-x86_64.sh" | sha256sum -c - \
 && /bin/bash Miniconda2-3.19.0-Linux-x86_64.sh -f -b -p $CONDA_DIR \
 && rm Miniconda2-3.19.0-Linux-x86_64.sh \
 && $CONDA_DIR/bin/conda install --quiet --yes conda==3.19.1 \
 && conda clean -tipsy
#  Install Jupyter notebook as moc
RUN conda install --quiet --yes 'notebook=5.0*' terminado \
 && conda clean -tipsy
USER root
#  Configure container startup as root
EXPOSE 8888/tcp
WORKDIR /home/$NB_USER/work
ENTRYPOINT ["tini", "--"]
CMD ["start-notebook.sh"]
#  Add local files as late as possible to avoid cache busting
COPY start-notebook.sh /usr/local/bin/
COPY jupyter_notebook_config.py /home/$NB_USER/.jupyter/
RUN chown -R $NB_USER:users /home/$NB_USER/.jupyter
#  Switch back to moc to avoid accidental container runs as root
USER moc
MAINTAINER Jupyter Project <jupyter@googlegroups.com>
USER root
#  libav-tools for matplotlib anim
RUN apt-get update \
 && apt-get install --no-install-recommends libav-tools -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
USER moc
#  Install Python 2 packages
RUN conda create --quiet --yes -p $CONDA_DIR/envs/python2 python=2.7 'ipython=4.1*' 'ipywidgets=4.1*' 'pandas=0.17*' 'matplotlib=1.5*' 'scipy=0.17*' 'seaborn=0.7*' 'scikit-learn=0.17*' 'scikit-image=0.11*' 'statsmodels=0.6*' \
 && conda clean -tipsy
USER root
#  Install Python 2 kernel spec globally to avoid permission problems when NB_UID
#  switching at runtime.
RUN $CONDA_DIR/envs/python2/bin/python -m ipykernel install
USER moc
RUN pip install pip --upgrade \
 && pip install elasticsearch \
 && pip install plotly \
 && pip install elasticsearch-dsl \
 && pip install pandas \
 && pip install holoviews \
 && pip install pyupset \
 && pip install bokeh
