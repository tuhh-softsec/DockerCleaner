#   This is a Dockerfile for osrf/sros:kinetic with SROS
FROM ubuntu:xenial
#   install packages
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends dirmngr=2.1.11-6ubuntu2.1 gnupg2=2.1.11-6ubuntu2.1 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   setup keys
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 421C365BD9FF1F717815A3895523BAEEB01FA116
#   setup sources.list
RUN echo "deb http://packages.ros.org/ros/ubuntu xenial main" > /etc/apt/sources.list.d/ros-latest.list
#   install bootstrap
#     and dev tools
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.1ubuntu2 python-catkin-tools python-rosdep=0.11.4-2 python-rosinstall-generator=0.1.11-1 python-wstool=0.1.12-1 bash-completion=1:2.1-4.2ubuntu1.1 byobu=5.106-0ubuntu1 git=1:2.7.4-0ubuntu1.10 less=481-2.1ubuntu0.2 tree=1.7.0-3 wget=1.17.1-1ubuntu1.5 python-pip=8.1.1-2ubuntu0.6 -y ) \
 && rm -rf /var/lib/apt/lists/*
#   setup environment
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
#   set envormetn and workspace
ENV ROS_DISTRO="kinetic"
ENV CATKIN_WS="/root/sros_catkin_ws"
RUN mkdir -p $CATKIN_WS/src
WORKDIR $CATKIN_WS/src
#   download sourcecode for sros
RUN rosinstall_generator ros_comm rospy_tutorials --rosdistro ${ROS_DISTRO} --deps --tar > ${ROS_DISTRO}-ros_comm-wet.rosinstall \
 && wstool init -j8 . ${ROS_DISTRO}-ros_comm-wet.rosinstall \
 && rm -rf ros_comm \
 && git clone -b sros https://github.com/ros/ros_comm \
 && git clone -b sros https://github.com/ros-infrastructure/rospkg ../rospkg
#   install dependencies
RUN : \
 && rosdep init \
 && rosdep update \
 && rosdep install -y --from-paths . --ignore-src --rosdistro ${ROS_DISTRO} --as-root=apt:false \
 && pip install ../rospkg/ --upgrade \
 && rm -rf /var/lib/apt/lists/*
#   build repo
WORKDIR $CATKIN_WS
ENV TERM="xterm"
ENV PYTHONIOENCODING="UTF-8"
RUN catkin config --install \
 && catkin build --no-status --summarize
#  ####################
#     Install aztarna
#  ####################
RUN : \
 && apt-get -qqy upgrade
#   install aztarna build dependencies
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 libgmp3-dev=2:6.1.0+dfsg-2 gengetopt=2.22.6+dfsg0-1 libpcap-dev=1.7.4-2ubuntu0.1 flex=2.6.0-11 byacc=20140715-1 libjson-c-dev=0.11-4ubuntu2.6 pkg-config=0.29.1-0ubuntu1 libunistring-dev=0.9.3-5.2ubuntu1 wget=1.17.1-1ubuntu1.5 unzip=6.0-20ubuntu1.1 git=1:2.7.4-0ubuntu1.10 -qqy )
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y )
RUN add-apt-repository ppa:deadsnakes/ppa
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python3.6 python3.6-dev python3-pip=8.1.1-2ubuntu0.6 -qqy )
#   RUN apt-get -qqy install python3 python3-dev python3-pip
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxslt1-dev=1.1.28-2.1ubuntu0.3 -qqy )
RUN (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -qqy )
RUN (apt-get update ;apt-get install --no-install-recommends libffi-dev=3.2.1-4 -qqy )
RUN (apt-get update ;apt-get install --no-install-recommends libssl-dev=1.0.2g-1ubuntu4.20 -qqy )
#   copy the aztarna files the FS and install it
COPY ./aztarna /root/aztarna
#   RUN cd /root/aztarna && git checkout ${AZTARNA_COMMIT} && python3 setup.py install
RUN cd /root/aztarna \
 && python3.6 setup.py install
#  ####################
#     Finalize setup
#  ####################
#   setup demo bashrc
RUN echo 'source "$CATKIN_WS/install/setup.bash"' >> ~/.bashrc
#   setup demo config
COPY ./config /root/.ros/sros/config/
#   setup entrypoint
COPY ./ros_entrypoint.sh /
ENTRYPOINT ["/ros_entrypoint.sh"]
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
