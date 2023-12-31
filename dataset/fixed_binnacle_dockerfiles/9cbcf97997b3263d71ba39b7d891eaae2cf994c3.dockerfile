FROM ubuntu:16.04 AS glvnd
MAINTAINER Eric Heiden <heiden@usc.edu>
#   Set up libglvnd for OpenGL GUI support
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 ca-certificates=20210119~16.04.1 make=4.1-6 automake=1:1.15-4ubuntu1 autoconf=2.69-9 libtool=2.4.6-0.1 pkg-config=0.29.1-0ubuntu1 python=2.7.12-1~16.04 libxext-dev=2:1.3.3-1 libx11-dev=2:1.6.3-1ubuntu2.2 x11proto-gl-dev=1.4.17-1 -y ) \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /opt/libglvnd
RUN git clone --branch=v1.0.0 https://github.com/NVIDIA/libglvnd.git . \
 && ./autogen.sh \
 && ./configure --prefix=/usr/local --libdir=/usr/local/lib/x86_64-linux-gnu \
 && make -j"$( nproc ;)" install-strip \
 && find /usr/local/lib/x86_64-linux-gnu -type f -name 'lib*.la' -delete
RUN dpkg --add-architecture i386 \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends gcc-multilib=4:5.3.1-1ubuntu1 libxext-dev:i386 libx11-dev:i386 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   32-bit libraries
RUN make distclean \
 && ./autogen.sh \
 && ./configure --prefix=/usr/local --libdir=/usr/local/lib/i386-linux-gnu --host=i386-linux-gnu "CFLAGS=-m32" "CXXFLAGS=-m32" "LDFLAGS=-m32" \
 && make -j"$( nproc ;)" install-strip \
 && find /usr/local/lib/i386-linux-gnu -type f -name 'lib*.la' -delete
FROM tensorflow/tensorflow:1.8.0-gpu-py3
COPY --from=glvnd /usr/local/lib/x86_64-linux-gnu /usr/local/lib/x86_64-linux-gnu
COPY --from=glvnd /usr/local/lib/i386-linux-gnu /usr/local/lib/i386-linux-gnu
COPY internal/10_nvidia.json /usr/local/share/glvnd/egl_vendor.d/10_nvidia.json
RUN echo '/usr/local/lib/x86_64-linux-gnu' >> /etc/ld.so.conf.d/glvnd.conf \
 && echo '/usr/local/lib/i386-linux-gnu' >> /etc/ld.so.conf.d/glvnd.conf \
 && ldconfig
ENV LD_LIBRARY_PATH="/usr/local/lib/x86_64-linux-gnu:/usr/local/lib/i386-linux-gnu${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"
ENV NVIDIA_DRIVER_CAPABILITIES="${NVIDIA_DRIVER_CAPABILITIES},display"
ARG USER
ARG HOME
ENV LANG="C.UTF-8" \
    LC_ALL="C.UTF-8" \
    USER="$USER" \
    HOME="$HOME"
RUN echo "The working directory is: $HOME"
RUN echo "The user is: $USER"
RUN mkdir -p $HOME
WORKDIR $HOME
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 git=1:2.7.4-0ubuntu1.10 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   install dependencies
RUN : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential apt-utils curl nano vim libfreetype6-dev libpng12-dev libzmq3-dev git python-numpy python-dev python-opengl cmake zlib1g-dev libjpeg-dev xvfb libav-tools xorg-dev libboost-all-dev libsdl2-dev swig libgtk2.0-dev wget ca-certificates unzip aptitude pkg-config qtbase5-dev libqt5opengl5-dev libassimp-dev libpython3.5-dev libboost-python-dev libtinyxml-dev golang python-opencv terminator tmux libcanberra-gtk-module libfuse2 libnss3 fuse python3-tk libglfw3-dev libgl1-mesa-dev libgl1-mesa-glx libglew-dev libosmesa6-dev net-tools xpra xserver-xorg-dev libffi-dev libxslt1.1 feedgnuplot libglew-dev parallel htop apt-transport-https
#   install Sublime Text
RUN wget -qO - https://download.sublimetext.com/sublimehq-pub.gpg | sudo apt-key add - \
 && echo "deb https://download.sublimetext.com/ apt/stable/" | sudo tee /etc/apt/sources.list.d/sublime-text.list \
 && : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y sublime-text
RUN pip3 install --upgrade pip
COPY ./internal/ /
#   Install MuJoCo 1.50 and 1.31
WORKDIR /opt
RUN mkdir mujoco \
 && cd mujoco \
 && wget https://www.roboti.us/download/mjpro150_linux.zip \
 && unzip mjpro150_linux.zip \
 && rm mjpro150_linux.zip \
 && wget https://www.roboti.us/download/mjpro131_linux.zip \
 && unzip mjpro131_linux.zip \
 && rm mjpro131_linux.zip \
 && if [ -f "/mjkey.txt" ] ; then mv /mjkey.txt . \
 && cp mjkey.txt mjpro150/bin/ \
 && cp mjkey.txt mjpro131/bin/ \
 && echo "Installed MuJoCo Key file." ; else echo "Could not find MuJoCo key file (mjkey.txt) in ./internal!\nPlease copy it manually to ~/.mujoco when inside the docker container." 1>&2; fi
ENV MUJOCO_PY_MJPRO_PATH="/opt/mujoco/mjpro150"
ENV MUJOCO_LICENSE_KEY="/opt/mujoco/mjkey.txt"
ENV MUJOCO_PY_MUJOCO_PATH="/opt/mujoco"
ENV LD_LIBRARY_PATH="/opt/mujoco/mjpro150/bin:$LD_LIBRARY_PATH"
#   Temporarily copy MuJoCo to home folder to install mujoco-py
RUN mkdir ~/.mujoco \
 && cp -r /opt/mujoco/* ~/.mujoco
RUN pip3 --no-cache-dir install gym[all]==0.10.3 mujoco-py scikit-image plotly ipykernel jupyter jupyterlab matplotlib numpy scipy sklearn pandas Pillow empy tqdm pyopengl ipdb cloudpickle imageio mpi4py jsonpickle gtimer path.py cached-property flask joblib lasagne PyOpenGL six pyprind virtualenv
#   Set up permissions to use same UID and GID as host system user
#   https://denibertovic.com/posts/handling-permissions-with-docker-volumes/
RUN gpg --keyserver ha.pool.sks-keyservers.net --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4
RUN curl -o /usr/local/bin/gosu -SL "https://github.com/tianon/gosu/releases/download/1.4/gosu-$( dpkg --print-architecture ;)" \
 && curl -o /usr/local/bin/gosu.asc -SL "https://github.com/tianon/gosu/releases/download/1.4/gosu-$( dpkg --print-architecture ;).asc" \
 && gpg --verify /usr/local/bin/gosu.asc \
 && rm /usr/local/bin/gosu.asc \
 && chmod +x /usr/local/bin/gosu
#   disable password prompt for sudo
RUN echo "$USER ALL=(ALL:ALL) NOPASSWD: ALL" | sudo env EDITOR="tee -a" visudo
#   Install Anaconda 3
RUN echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh \
 && wget --quiet https://repo.continuum.io/archive/Anaconda3-5.1.0-Linux-x86_64.sh -O ~/anaconda.sh \
 && /bin/bash ~/anaconda.sh -b -p /opt/conda \
 && rm ~/anaconda.sh
#   Install Jupyter Lab
RUN jupyter serverextension enable --py jupyterlab --sys-prefix
#   Jupyter Lab Bokeh extension requires NodeJS
RUN curl -sL https://deb.nodesource.com/setup_9.x | sudo -E bash - \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y )
RUN jupyter labextension install jupyterlab_bokeh
#   Install Baselines
#   (Remove tensorflow from dependencies since we already installed the GPU version,
#   installing tensorflow again would deactivate GPU support!)
RUN cd /opt \
 && git clone https://github.com/openai/baselines.git \
 && cd baselines \
 && sed -i '/tensorflow/d' setup.py \
 && pip install -e .
#   Install Roboschool
ENV ROBOSCHOOL_PATH="/opt/roboschool"
RUN git clone https://github.com/openai/roboschool.git /opt/roboschool
RUN cd /opt \
 && git clone https://github.com/olegklimov/bullet3 -b roboschool_self_collision \
 && mkdir bullet3/build \
 && cd bullet3/build \
 && cmake -DBUILD_SHARED_LIBS=ON -DUSE_DOUBLE_PRECISION=1 -DCMAKE_INSTALL_PREFIX:PATH=$ROBOSCHOOL_PATH/roboschool/cpp-household/bullet_local_install -DBUILD_CPU_DEMOS=OFF -DBUILD_BULLET2_DEMOS=OFF -DBUILD_EXTRAS=OFF -DBUILD_UNIT_TESTS=OFF -DBUILD_CLSOCKET=OFF -DBUILD_ENET=OFF -DBUILD_OPENGL3_DEMOS=OFF .. \
 && make -j4 \
 && make install
RUN pip3 install -e /opt/roboschool
ENV DOCKER_HOME="$HOME"
#   Install VirtualGL
RUN dpkg -i /virtualgl_2.5.2_amd64.deb \
 && rm /virtualgl_2.5.2_amd64.deb
ENV TERM="xterm-256color"
#   TensorBoard
EXPOSE 6006/tcp
#   Jupyter
EXPOSE 8888/tcp
ENTRYPOINT ["/docker-entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
