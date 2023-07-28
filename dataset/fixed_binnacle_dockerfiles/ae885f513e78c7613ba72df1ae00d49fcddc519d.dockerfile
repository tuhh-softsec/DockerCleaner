#  Should set up a system to use GRL
#  Includes dependencies for GrlInverseKinematics, PivotCalibration & HandEyeCalibration
#  NB - This is a complicated docker file and might need to be __run twice__!
#  Install on ubuntu 16.04
#  Comment the following if you wish to use ROS 
#  --------------Start--------------------
FROM ubuntu:16.04
CMD ["bash"]
#  ---------------End-----------------
#  Uncomment the following if you wish to use ROS 
#  ------------Start---------------------
#  FROM osrf/ros:kinetic-desktop
#  RUN apt-get update && apt-get install -y \
#      ros-kinetic-desktop-full=1.3.0-0* \
#      && rm -rf /var/lib/apt/lists/*
#  RUN mkdir -p /etc/my_init.d
#  COPY startup.sh /etc/my_init.d/startup.sh
#  RUN chmod +x /etc/my_init.d/startup.sh
#  COPY pre-conf.sh /sbin/pre-conf
#  RUN chmod +x /sbin/pre-conf \
#      && /bin/bash -c /sbin/pre-conf \
#      && rm /sbin/pre-conf
#  CMD bash -c "/etc/my_init.d/startup.sh"; bash
#  -------------End-----------------------------
LABEL com.nvidia.volumes.needed="nvidia_driver"
ENV PATH="/usr/local/nvidia/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/nvidia/lib:/usr/local/nvidia/lib64:${LD_LIBRARY_PATH}"
ENV HOME="/root"
WORKDIR /root
#  Essentials
RUN apt-get clean \
 && : \
 && rm -rf /tmp/* /var/tmp/* \
 && rm -rf /var/lib/apt/lists/*
#  Compiling
RUN apt-get clean \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libtool=2.4.6-0.1 bash-completion=1:2.1-4.2ubuntu1.1 pkg-config=0.29.1-0ubuntu1 build-essential=12.1ubuntu2 autoconf=2.69-9 automake=1:1.15-4ubuntu1 cmake=3.5.1-1ubuntu3 cmake-curses-gui=3.5.1-1ubuntu3 -y )
RUN apt-get clean \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 vim=2:7.4.1689-3ubuntu1.5 sudo=1.8.16-0ubuntu1.10 unzip=6.0-20ubuntu1.1 curl=7.47.0-1ubuntu2.19 ctags git=1:2.7.4-0ubuntu1.10 tmux=2.1-3build1 -y )
#  LLVM
RUN apt-get clean \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends clang=1:3.8-33ubuntu3.1 lldb=1:3.8-33ubuntu3.1 -y )
#  apt-repository-add scripts
RUN apt-get clean \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -y )
#  GRL dependencies
RUN apt-get clean \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libboost-all-dev=1.58.0.1ubuntu1 libeigen3-dev=3.3~beta1-2 gfortran=4:5.3.1-1ubuntu1 -y )
#  OpenCV
RUN apt-get clean \
 && add-apt-repository --yes ppa:xqms/opencv-nonfree \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libopencv-nonfree-dev libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 -y )
RUN cd ~ \
 && git clone https://github.com/ahundt/robotics_setup.git
RUN ["/bin/bash", "-c", "cd", "~/robotics_setup", "&&", "~/robotics_setup/vrep.sh"]
RUN ["/bin/bash", "-c", "cd", "~/robotics_setup", "&&", "~/robotics_setup/spdlog.sh"]
#  two scripts below called from grl_kuka.sh
#  RUN ["/bin/bash","-c","./flatbuffers.sh"]
#  RUN ["/bin/bash","-c","./cmake-basis.sh"]
RUN ["/bin/bash", "-c", "cd", "~/robotics_setup", "&&", "~/robotics_setup/robotics_tasks.sh"]
#  NB - Ceres must run before Camodocal - should run ceres first in camodocal script
#  Better to isolate each script from it's dependencies?
RUN ["/bin/bash", "-c", "cd", "~/robotics_setup", "&&", "~/robotics_setup/ceres.sh"]
RUN ["/bin/bash", "-c", "cd", "~/robotics_setup", "&&", "~/robotics_setup/camodocal.sh"]
RUN ["/bin/bash", "-c", "cd", "~/robotics_setup", "&&", "~/robotics_setup/trtk.sh"]
#  Should be separate nonopb script
RUN curl -sSL https://koti.kapsi.fi/~jpa/nanopb/download/nanopb-0.3.7-linux-x86.tar.gz | tar -xvz \
 && cd nanopb-0.3.7-linux-x86 \
 && cmake . \
 && make \
 && make install
RUN ["/bin/bash", "-c", "cd", "~/robotics_setup", "&&", "~/robotics_setup/grl_kuka.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
