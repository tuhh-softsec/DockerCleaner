#  This is a Dockerfile for osrf/sros:kinetic with SROS
FROM ubuntu:xenial
#  install packages
RUN apt-get update \
 && apt-get install --no-install-recommends dirmngr gnupg2 -y \
 && rm -rf /var/lib/apt/lists/*
#  setup keys
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 421C365BD9FF1F717815A3895523BAEEB01FA116
#  setup sources.list
RUN echo "deb http://packages.ros.org/ros/ubuntu xenial main" > /etc/apt/sources.list.d/ros-latest.list
#  install bootstrap
#    and dev tools
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential python-catkin-tools python-rosdep python-rosinstall-generator python-wstool bash-completion byobu git less tree wget python-pip -y \
 && rm -rf /var/lib/apt/lists/*
#  setup environment
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
#  set envormetn and workspace
ENV ROS_DISTRO="kinetic"
ENV CATKIN_WS="/root/sros_catkin_ws"
RUN mkdir -p $CATKIN_WS/src
WORKDIR $CATKIN_WS/src
#  download sourcecode for sros
RUN rosinstall_generator ros_comm rospy_tutorials --rosdistro ${ROS_DISTRO} --deps --tar > ${ROS_DISTRO}-ros_comm-wet.rosinstall \
 && wstool init -j8 . ${ROS_DISTRO}-ros_comm-wet.rosinstall \
 && rm -rf ros_comm \
 && git clone -b sros https://github.com/ros/ros_comm \
 && git clone -b sros https://github.com/ros-infrastructure/rospkg ../rospkg
#  install dependencies
RUN apt-get update \
 && rosdep init \
 && rosdep update \
 && rosdep install -y --from-paths . --ignore-src --rosdistro ${ROS_DISTRO} --as-root=apt:false \
 && pip install ../rospkg/ --upgrade \
 && rm -rf /var/lib/apt/lists/*
#  build repo
WORKDIR $CATKIN_WS
ENV TERM="xterm"
ENV PYTHONIOENCODING="UTF-8"
RUN catkin config --install \
 && catkin build --no-status --summarize
# ####################
#    Install aztarna
# ####################
RUN apt-get update -qq \
 && apt-get -qqy upgrade
#  install aztarna build dependencies
RUN apt-get install --no-install-recommends build-essential cmake libgmp3-dev gengetopt libpcap-dev flex byacc libjson-c-dev pkg-config libunistring-dev wget unzip git -qqy
RUN apt-get install --no-install-recommends software-properties-common -y
RUN add-apt-repository ppa:deadsnakes/ppa
RUN apt-get update
RUN apt-get install --no-install-recommends python3.6 python3.6-dev python3-pip -qqy
#  RUN apt-get -qqy install python3 python3-dev python3-pip
RUN apt-get install --no-install-recommends libxml2-dev libxslt1-dev -qqy
RUN apt-get install --no-install-recommends zlib1g-dev -qqy
RUN apt-get install --no-install-recommends libffi-dev -qqy
RUN apt-get install --no-install-recommends libssl-dev -qqy
#  copy the aztarna files the FS and install it
COPY ./aztarna /root/aztarna
#  RUN cd /root/aztarna && git checkout ${AZTARNA_COMMIT} && python3 setup.py install
RUN cd /root/aztarna \
 && python3.6 setup.py install
# ####################
#    Finalize setup
# ####################
#  setup demo bashrc
RUN echo 'source "$CATKIN_WS/install/setup.bash"' >> ~/.bashrc
#  setup demo config
COPY ./config /root/.ros/sros/config/
#  setup entrypoint
COPY ./ros_entrypoint.sh /
ENTRYPOINT ["/ros_entrypoint.sh"]
CMD ["bash"]
