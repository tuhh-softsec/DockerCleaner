#  NOTICE: To keep consistency across this docker file, scripts/setup_linux.sh
#  and scripts/setup_macos.sh, if there's any changes applied to this file,
#  specially regarding the installation of dependencies, apply those same
#  changes to the mentioned files.
ARG PARENT_IMAGE=ubuntu:16.04
FROM $PARENT_IMAGE
#  http://bugs.python.org/issue19846
#  > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
#  apt dependencies
RUN apt-get update -y -q \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y wget bzip2 unzip git curl cmake xorg-dev libglew-dev libosmesa6-dev patchelf libpq-dev ffmpeg libjpeg-dev swig libsdl2-dev libopenmpi-dev openmpi-bin \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  Build GLFW because the Ubuntu 16.04 version is too old
#  See https://github.com/glfw/glfw/issues/1004
RUN apt-get purge -y -v libglfw*
RUN git clone https://github.com/glfw/glfw.git \
 && cd glfw \
 && git checkout 0be4f3f75aebd9d24583ee86590a38e741db0904 \
 && mkdir glfw-build \
 && cd glfw-build \
 && cmake -DBUILD_SHARED_LIBS=ON -DGLFW_BUILD_EXAMPLES=OFF -DGLFW_BUILD_TESTS=OFF -DGLFW_BUILD_DOCS=OFF .. \
 && make -j"$( nproc ;)" \
 && make install \
 && cd ../../ \
 && rm -rf glfw
#  MuJoCo 2.0 (for dm_control)
RUN mkdir -p /root/.mujoco \
 && wget https://www.roboti.us/download/mujoco200_linux.zip -O mujoco.zip \
 && unzip mujoco.zip -d $HOME/.mujoco \
 && rm mujoco.zip \
 && ln -s $HOME/.mujoco/mujoco200_linux $HOME/.mujoco/mujoco200
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/root/.mujoco/mujoco200/bin"
#  conda
RUN wget https://repo.continuum.io/miniconda/Miniconda2-latest-Linux-x86_64.sh -O miniconda.sh \
 && bash miniconda.sh -b -p /opt/conda \
 && rm miniconda.sh \
 && ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh
ENV PATH="$PATH:/opt/conda/bin"
RUN conda update -q -y conda
#  conda environment
#  Copy over just environment.yml and setup.py first, so the Docker cache doesn't
#  expire until they change
#
#  Files needed to run setup.py
#  - README.md
#  - VERSION
#  - scripts/garage
#  - src/garage/__init__.py
#  - setup.py
COPY README.md /root/code/garage/README.md
COPY VERSION /root/code/garage/VERSION
COPY scripts/garage /root/code/garage/scripts/garage
COPY src/garage/__init__.py /root/code/garage/src/garage/__init__.py
COPY setup.py /root/code/garage/setup.py
COPY environment.yml /root/code/garage/environment.yml
#  We need a MuJoCo key to install mujoco_py
#  In this step only the presence of the file mjkey.txt is required, so we only
#  create an empty file
ARG MJKEY
RUN touch /root/.mujoco/mjkey.txt \
 && conda env create -f /root/code/garage/environment.yml \
 && rm -rf /opt/conda/pkgs/* \
 && rm /root/.mujoco/mjkey.txt
#  Extras
#  prevent pip from complaining about available upgrades
RUN ["/bin/bash", "-c", "source", "activate", "garage", "&&", "pip", "install", "pip", "--upgrade"]
#  Setup repo
WORKDIR /root/code/garage
#  Pre-build pre-commit env
COPY .pre-commit-config.yaml /root/code/garage
RUN ["/bin/bash", "-c", "source", "activate", "garage", "&&", "git", "init", "&&", "pre-commit"]
