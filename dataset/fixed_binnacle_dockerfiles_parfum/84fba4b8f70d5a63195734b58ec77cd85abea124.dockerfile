FROM nvidia/cuda:8.0-cudnn5-devel
# ################################################################################################################
#            Global
# ################################################################################################################
#  apt-get to skip any interactive post-install configuration steps with DEBIAN_FRONTEND=noninteractive and apt-get install -y
ENV LANG="C.UTF-8" \
    LC_ALL="C.UTF-8"
ARG DEBIAN_FRONTEND=noninteractive
# ################################################################################################################
#            Global Path Setting
# ################################################################################################################
ENV CUDA_HOME="/usr/local/cuda"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${CUDA_HOME}/lib64"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"
ENV OPENCL_LIBRARIES="/usr/local/cuda/lib64"
ENV OPENCL_INCLUDE_DIR="/usr/local/cuda/include"
# ################################################################################################################
#            TINI
# ################################################################################################################
#  Install tini
ENV TINI_VERSION="v0.14.0"
COPY https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /tini
RUN chmod +x /tini
# ################################################################################################################
#            SYSTEM
# ################################################################################################################
#  update: downloads the package lists from the repositories and "updates" them to get information on the newest versions of packages and their
#  dependencies. It will do this for all repositories and PPAs.
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential curl wget bzip2 ca-certificates libglib2.0-0 libxext6 libsm6 libxrender1 git vim mercurial subversion cmake libboost-dev libboost-system-dev libboost-filesystem-dev gcc g++ -y
#  Add OpenCL ICD files for LightGBM
RUN mkdir -p /etc/OpenCL/vendors \
 && echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd
# ################################################################################################################
#            CONDA
# ################################################################################################################
ARG CONDA_DIR=/opt/conda
#  add to path
ENV PATH="$CONDA_DIR/bin:$PATH"
#  Install miniconda
RUN echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda2-latest-Linux-x86_64.sh -O ~/miniconda.sh \
 && /bin/bash ~/miniconda.sh -b -p /opt/conda \
 && rm ~/miniconda.sh
RUN conda create -q -n py2 python=2.7 mkl numpy scipy scikit-learn jupyter notebook ipython pandas matplotlib
RUN conda create -q -n py3 python=3.5 mkl numpy scipy scikit-learn jupyter notebook ipython pandas matplotlib
# ################################################################################################################
#            LightGBM
# ################################################################################################################
RUN cd /usr/local/src \
 && mkdir lightgbm \
 && cd lightgbm \
 && git clone --recursive https://github.com/microsoft/LightGBM \
 && cd LightGBM \
 && mkdir build \
 && cd build \
 && cmake -DUSE_GPU=1 -DOpenCL_LIBRARY=/usr/local/cuda/lib64/libOpenCL.so -DOpenCL_INCLUDE_DIR=/usr/local/cuda/include/ .. \
 && make OPENCL_HEADERS=/usr/local/cuda-8.0/targets/x86_64-linux/include LIBOPENCL=/usr/local/cuda-8.0/targets/x86_64-linux/lib
ENV PATH="/usr/local/src/lightgbm/LightGBM:${PATH}"
RUN /bin/bash -c "source activate py2 \
 && cd /usr/local/src/lightgbm/LightGBM/python-package \
 && python setup.py install --precompile \
 && source deactivate"
RUN /bin/bash -c "source activate py3 \
 && cd /usr/local/src/lightgbm/LightGBM/python-package \
 && python setup.py install --precompile \
 && source deactivate"
# ################################################################################################################
#            System CleanUp
# ################################################################################################################
#  apt-get autoremove: used to remove packages that were automatically installed to satisfy dependencies for some package and that are no more needed.
#  apt-get clean: removes the aptitude cache in /var/cache/apt/archives. You'd be amazed how much is in there! the only drawback is that the packages
#  have to be downloaded again if you reinstall them.
RUN apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && conda clean -i -l -t -y
# ################################################################################################################
#            JUPYTER
# ################################################################################################################
#  password: keras
#  password key: --NotebookApp.password='sha1:98b767162d34:8da1bc3c75a0f29145769edc977375a373407824'
#  Add a notebook profile.
RUN mkdir -p -m 700 /root/.jupyter/ \
 && echo "c.NotebookApp.ip = '*'" >> /root/.jupyter/jupyter_notebook_config.py
VOLUME /home
WORKDIR /home
#  IPython
EXPOSE 8888/tcp
ENTRYPOINT ["/tini", "--"]
CMD /bin/bash -c "source activate py3 \
 && jupyter notebook --allow-root --no-browser --NotebookApp.password='sha1:98b767162d34:8da1bc3c75a0f29145769edc977375a373407824' \
 && source deactivate"
