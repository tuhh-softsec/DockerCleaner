FROM python:3.7-slim
LABEL maintainer="owen.kaluza@monash.edu"
LABEL repo="https://github.com/OKaluza/LavaVu"
#   install things
RUN : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq --no-install-recommends bash-completion build-essential xorg-dev ssh curl libfreetype6-dev libpng-dev libtiff-dev libxft-dev xvfb freeglut3 freeglut3-dev libgl1-mesa-dri libgl1-mesa-glx mesa-utils libavcodec-dev libavformat-dev libavutil-dev libswscale-dev rsync xauth
#   Add Tini
ENV TINI_VERSION="v0.18.0"
RUN which wget &> /dev/null || (apt-get update ;apt-get install --no-install-recommends wget=1.20.3 ) ; wget --no-verbose --output-document /tini https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini
RUN chmod +x /tini
#   install the notebook package
RUN pip install pip==23.1 --no-cache --upgrade \
 && pip install notebook==6.5.4 --no-cache
RUN pip install setuptools==67.6.1
RUN pip install packaging==23.1 appdirs==1.4.4 numpy==1.24.2 jupyter==1.0.0 notebook==6.5.4 matplotlib==3.7.1 runipy==0.1.5 pillow==9.5.0 scipy==1.10.1 h5py==3.8.0 rise==5.7.1 jupyter-server-proxy==3.2.2 jupyterlab==3.6.3
#  Setup RISE for notebook slideshows
RUN jupyter-nbextension install rise --py --sys-prefix
RUN jupyter nbextension enable rise --py --sys-prefix
ENV NB_USER="jovyan"
ENV NB_UID="1000"
ENV HOME="/home/${NB_USER}"
#   create user with a home directory
ARG NB_USER
ARG NB_UID
ENV USER="${NB_USER}"
ENV HOME="/home/${NB_USER}"
RUN adduser --disabled-password --gecos "Default user" --uid ${NB_UID} ${NB_USER}
WORKDIR ${HOME}
#   script for xvfb-run.  all docker commands will effectively run under this via the entrypoint
RUN printf "#\041/bin/sh \n rm -f /tmp/.X99-lock \
 && xvfb-run -s '-screen 0 1600x1200x16' $@" >> /usr/local/bin/xvfbrun.sh \
 && chmod +x /usr/local/bin/xvfbrun.sh
#   Make sure the contents of our repo are in ${HOME}
COPY . ${HOME}
USER root
RUN chown -R ${NB_UID} ${HOME}
USER ${NB_USER}
#  Build LavaVu
#   setup environment
ENV PYTHONPATH="$PYTHONPATH:${HOME}"
#   Compile, delete some unnecessary files
RUN cd ~ \
 && make LIBPNG=1 TIFF=1 VIDEO=1 -j$( nproc ;) \
 && rm -fr tmp
#  Trust included notebooks
RUN cd ~ \
 && find notebooks -name *.ipynb -print0 | xargs -0 jupyter trust
#   Add a notebook profile.
RUN cd ~ \
 && mkdir .jupyter \
 && echo "c.NotebookApp.ip = '0.0.0.0'" >> .jupyter/jupyter_notebook_config.py \
 && echo "c.NotebookApp.token = ''" >> .jupyter/jupyter_notebook_config.py
#   note we use xvfb which to mimic the X display for lavavu
ENTRYPOINT ["/tini", "--", "/usr/local/bin/xvfbrun.sh"]
#   launch notebook
#   CMD scripts/run-jupyter.sh
CMD ["jupyter", "notebook", "--ip='0.0.0.0'", "--NotebookApp.token=''", "--no-browser"]
# Please add your HEALTHCHECK here!!!
