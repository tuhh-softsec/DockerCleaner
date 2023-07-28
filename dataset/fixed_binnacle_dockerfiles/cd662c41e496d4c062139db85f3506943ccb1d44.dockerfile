FROM underworldcode/underworld2_untested:v2.5
MAINTAINER romain.beucher@unimelb.edu
USER root
#   required dependency for pyBadlands and friends
RUN : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq --no-install-recommends apt-utils libavcodec-dev libavformat-dev libavutil-dev libswscale-dev openmpi-bin libhdf5-dev liblapack-dev llvm libedit-dev gfortran libnetcdf-dev libgeos-dev libgeos++-dev wget \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && pip install pillow==9.5.0 enum34==1.1.10 pyvirtualdisplay==3.0 scipy==1.10.1 Cython==0.20 markupsafe==2.1.2 zmq==0.0.0 singledispatch==4.0.0 backports_abc==0.5 certifi==2022.12.7 jsonschema==4.17.3 path.py==12.5.0 git+https://github.com/badlands-model/triangle pandas==2.0.0 plotly==5.14.1 numba==0.23.1 ez_setup==0.9 netcdf4==1.6.3 colorlover==0.3.0 cmocean==3.0.3 scikit-fuzzy==0.4.2 pyevtk==1.5.0 git+https://github.com/awickert/gFlex.git shapely==2.0.1 descartes==1.1.0 jupyter_contrib_nbextensions==0.7.0 tqdm==4.65.0 \
 && jupyter contrib nbextension install --system \
 && wget http://downloads.sourceforge.net/project/matplotlib/matplotlib-toolkits/basemap-1.0.7/basemap-1.0.7.tar.gz \
 && tar -zxvf basemap-1.0.7.tar.gz \
 && cd basemap-1.0.7 \
 && python setup.py build \
 && python setup.py install \
 && cd .. \
 && rm -rf basemap-1.0.7.tar.gz \
 && rm -rf basemap-1.0.7
#   switch user
ENV NB_USER="jovyan"
USER $NB_USER
WORKDIR /opt
ENV BLAND_DIR="/opt/pyBadlands"
ENV BCOMP_DIR="/opt/pyBadlands-Companion"
ENV BWORK_DIR="/opt/pyBadlands-workshop"
RUN git clone --depth 1 https://github.com/rbeucher/UWGeodynamics.git
USER root
RUN pip install -e /opt/UWGeodynamics
USER $NB_USER
RUN mkdir /workspace/UWGeodynamics \
 && rsync -av /opt/UWGeodynamics/examples/* /workspace/UWGeodynamics/examples/ \
 && rsync -av /opt/UWGeodynamics/tutorials/* /workspace/UWGeodynamics/tutorials/ \
 && rsync -av /opt/UWGeodynamics/manual/* /workspace/UWGeodynamics/manual/
#   download pyBadland, companion and workshop
WORKDIR /opt
RUN git clone --depth 1 https://github.com/badlands-model/pyBadlands_serial.git $BLAND_DIR \
 && git clone --depth 1 https://github.com/badlands-model/pyBadlands-Companion.git $BCOMP_DIR \
 && git clone --depth 1 https://github.com/badlands-model/pyBadlands-workshop.git $BWORK_DIR
#   compile pyBadlands and companion
WORKDIR $BLAND_DIR/pyBadlands/libUtils
USER root
RUN make \
 && pip install -e $BLAND_DIR \
 && cd /opt
RUN pip install -e $BCOMP_DIR \
 && jupyter nbextension enable hide_input/main --system \
 && jupyter nbextension enable init_cell/main --system
USER $NB_USER
RUN mkdir /workspace/volume \
 && mkdir /workspace/companion \
 && mkdir /workspace/workshop \
 && mkdir /workspace/LavaVu
#   Copy test files to workspace. Is this required??? takes 500Mb
RUN cp -av $BWORK_DIR/* /workspace/workshop/ \
 && cp -av $BLAND_DIR/Examples/* /workspace/examples/ \
 && cp -av $BCOMP_DIR/notebooks/* /workspace/companion/
#   NOT SURE ABOUT /workspace/volume???
ENV LD_LIBRARY_PATH="/workspace/volume/pyBadlands_serial/pyBadlands/libUtils:/$BLAND_DIR/pyBadlands/libUtils"
#   change user and update pythonpath
ENV PYTHONPATH="$PYTHONPATH:$UW2_DIR"
#   move back to workspace directory
WORKDIR /workspace
#   Trust underworld notebooks
RUN find -name *.ipynb -print0 | xargs -0 jupyter trust
#   launch notebook
CMD ["jupyter", "notebook", "--ip='*'", "--no-browser"]
# Please add your HEALTHCHECK here!!!
