# Base docker image
FROM ubuntu:xenial-20170417.1
RUN apt-get update -y
RUN apt-get install --no-install-recommends software-properties-common -y
RUN add-apt-repository ppa:george-edison55/cmake-3.x -y
RUN apt-get update -y
RUN apt-get install --no-install-recommends gawk git python-pip -y
RUN echo "export PS1=\\\\w\$" >> /etc/bash.bashrc
RUN apt-get install --no-install-recommends xterm -y
RUN pip install pip --upgrade
RUN apt-get install --no-install-recommends cmake -y
RUN apt-get install --no-install-recommends build-essential -y
RUN apt-get install --no-install-recommends gcc -y
# RUN apt-get -y install clang
RUN apt-get install --no-install-recommends unzip -y
RUN apt-get install --no-install-recommends vim -y
RUN apt-get install --no-install-recommends wget curl -y
# python packages
RUN apt-get install --no-install-recommends python-zmq -y
# window conrtol
RUN apt-get install --no-install-recommends wmctrl xdotool -y
# ### tmux new version
RUN apt-get install --no-install-recommends libprotoc-dev -y
RUN apt-get install --no-install-recommends protobuf-compiler -y
RUN apt-get install --no-install-recommends pkg-config -y
RUN apt-get install --no-install-recommends libevent1-dev libncurses5-dev -y
ENV LC_CTYPE="C.UTF-8"
RUN cd /tmp \
 && git clone https://github.com/tmux/tmux.git \
 && cd tmux \
 && git checkout tags/2.3
RUN apt-get install --no-install-recommends automake -y
RUN cd /tmp/tmux \
 && sh ./autogen.sh \
 && ./configure \
 && make install
# to enable copy paste in tmux mouse on https://coderwall.com/p/4b0d0a/how-to-copy-and-paste-with-tmux-on-ubuntu
RUN apt-get install --no-install-recommends xclip -y
ENV DEBIAN_FRONTEND="noninteractive"
# python3
# RUN apt-get -y install curl
# RUN curl -o /miniconda.sh https://repo.continuum.io/miniconda/Miniconda3-4.2.12-Linux-x86_64.sh
# RUN /bin/bash /miniconda.sh -b -p /miniconda
# RUN PATH=/miniconda/bin conda install -y pyzmq
# RUN PATH=/miniconda/bin conda install -y numpy
RUN apt-get install --no-install-recommends python3-dev -y
RUN apt-get install --no-install-recommends python3-pip -y
RUN apt-get install --no-install-recommends python3-numpy -y
RUN pip3 install --upgrade pip
RUN pip3 install pyzmq
# ####### ros kinetic ######
RUN echo "deb http://packages.ros.org/ros/ubuntu $( lsb_release -sc ;) main" > /etc/apt/sources.list.d/ros-latest.list
RUN apt-key adv --keyserver hkp://ha.pool.sks-keyservers.net:80 --recv-key 421C365BD9FF1F717815A3895523BAEEB01FA116
RUN apt-get update -y
RUN apt-get install --no-install-recommends ros-kinetic-desktop-full -y
RUN apt-get install --no-install-recommends python-wstool python-rosinstall-generator python-catkin-tools -y
# RUN bash -c "curl -ssL http://get.gazebosim.org | sh"
# RUN apt-get -y install ros-kinetic-desktop-full
# RUN apt-get -y install ros-kinetic-roslaunch
RUN apt-get install --no-install-recommends gazebo7 -y
RUN apt-get install --no-install-recommends libgazebo7-dev -y
# RUN apt-get -y install ros-kinetic-control-toolbox
# RUN apt-get -y install openssl libssl-dev
RUN apt-get install --no-install-recommends ros-kinetic-mavros ros-kinetic-mavros-extras -y
RUN /opt/ros/kinetic/lib/mavros/install_geographiclib_datasets.sh
# RUN apt-get -y install ros-kinetic-image-view2
# additional ros libs
RUN apt-get install --no-install-recommends ros-kinetic-image-pipeline -y
RUN apt-get install --no-install-recommends libglew-dev -y
#  git checkout tags/v0.5
#  .... build
RUN cd /tmp \
 && git clone https://github.com/stevenlovegrove/Pangolin.git
RUN cd /tmp/Pangolin \
 && git checkout tags/v0.5 \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make
# ############################# install opencv3 and numpy for python3
RUN apt-get install --no-install-recommends libjpeg8-dev libtiff5-dev libjasper-dev libpng12-dev libavcodec-dev libavformat-dev libswscale-dev libv4l-dev -y
RUN apt-get install --no-install-recommends libgtk2.0-dev libatlas-base-dev gfortran -y
RUN git clone https://github.com/Itseez/opencv.git
RUN cd /opencv \
 && git checkout 3.2.0
RUN git clone https://github.com/Itseez/opencv_contrib.git
RUN cd /opencv_contrib \
 && git checkout 3.2.0
RUN cd /opencv \
 && mkdir build
RUN cd /opencv/build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local -D OPENCV_EXTRA_MODULES_PATH=/opencv_contrib/modules -D WITH_IPP=OFF -D PYTHON3_PACKAGES_PATH=/usr/local/lib/python3.5/dist-packages .. \
 && make -j6
RUN cd /opencv/build \
 && make install
# ##### ORB_SLAM2
RUN cd /tmp \
 && git clone https://github.com/raulmur/ORB_SLAM2.git
# RUN apt-get install -y libopencv-dev
RUN cd /tmp/ORB_SLAM2 \
 && chmod +x build.sh
RUN cd /tmp/ORB_SLAM2 \
 && chmod +x build_ros.sh
RUN cd /tmp/ORB_SLAM2 \
 && ./build.sh
RUN chmod 777 /tmp/ORB_SLAM2/build
RUN apt-get install --no-install-recommends sudo -y
RUN apt-get install --no-install-recommends libprotobuf-dev -y
# ##python3 ros support
# RUN PATH=/miniconda/bin:$PATH pip install --upgrade pip
RUN pip3 install pyyaml rospkg catkin_pkg
# ####### nvidia part ######
ARG GDRIVER
COPY install_graphic_driver.sh /install_graphic_driver.sh
RUN chmod +x /install_graphic_driver.sh
RUN GDRIVER=$GDRIVER /install_graphic_driver.sh
# ##added more ros libs
RUN apt-get update -y
RUN apt-get install --no-install-recommends python-rosinstall -y
# ## add python dynamics
RUN apt-get install --no-install-recommends python-testresources -y
# RUN apt-get install -y python-scipy
# RUN pip2 install scipy
RUN pip2 install pydy
# user setings
ARG UID
RUN useradd -u $UID docker
RUN echo "docker:docker" | chpasswd
RUN echo "docker ALL=(ALL:ALL) NOPASSWD:ALL" >> /etc/sudoers
