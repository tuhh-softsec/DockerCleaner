#   A Dockerfile for the gym-gazebo environment
FROM ubuntu:16.04
#  --------------------
#   General setup
#  --------------------
#   Get the dependencies
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends xorg-dev=1:7.7+13ubuntu3.1 libgl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 xvfb=2:1.18.4-0ubuntu0.12 libxinerama1=2:1.1.3-1 libxcursor1=1:1.1.14-1ubuntu0.16.04.2 unzip=6.0-20ubuntu1.1 libglu1-mesa=9.0.0-2.1 libav-tools=7:2.8.17-0ubuntu0.1 python3=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 python3-setuptools=20.7.0-1 libpq-dev=9.5.25-0ubuntu0.16.04.1 libjpeg-dev=8c-2ubuntu8 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /usr/local/gym
#  --------------------
#   Install gym
#  --------------------
#   # Clone the official gym
#   RUN git clone https://github.com/openai/gym
#
#   # Install the gym's requirements
#   RUN pip install -r gym/requirements.txt
#
#   # Install the gym
#   RUN ls -l
#   RUN pip install -e gym/
#   Install from pip
RUN pip3 install gym
#   Checks
#  RUN python --version
#  RUN python -c "import gym"
#   Debug
#  RUN ls -l /usr/local/gym
#  RUN ls -l /usr/local/gym/gym-gazebo
#  RUN ls -l /usr/local/gym/gym
#   #--------------------
#   # Install Gazebo
#   #--------------------
RUN sh -c 'echo "deb http://packages.osrfoundation.org/gazebo/ubuntu-stable xenial main" > /etc/apt/sources.list.d/gazebo-stable.list'
RUN wget http://packages.osrfoundation.org/gazebo.key -O - | apt-key add -
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends gazebo8 -y )
#   RUN apt-get install -y libglib2.0-dev libgts-dev libgts-dev
RUN (apt-get update ;apt-get install --no-install-recommends libgazebo8-dev -y )
#   setup environment
EXPOSE 11345/tcp
#  --------------------
#   Install ROS
#  --------------------
#   RUN apt-get install -y locales-all
#   # setup environment
#   RUN locale-gen en_US.UTF-8
#   ENV LANG en_US.UTF-8
#   setup environment
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
#   setup keys
RUN apt-key adv --keyserver ha.pool.sks-keyservers.net --recv-keys 421C365BD9FF1F717815A3895523BAEEB01FA116
#   setup sources.list
RUN echo "deb http://packages.ros.org/ros/ubuntu xenial main" > /etc/apt/sources.list.d/ros-latest.list
#   RUN sh -c 'echo "deb http://packages.ros.org/ros/ubuntu $(lsb_release -sc) main" > /etc/apt/sources.list.d/ros-latest.list'
#
#   # install ros packages
ENV ROS_DISTRO="kinetic"
RUN (apt-get update ;apt-get install --no-install-recommends cmake=3.5.1-1ubuntu3 gcc=4:5.3.1-1ubuntu1 g++=4:5.3.1-1ubuntu1 )
#   # Install from repositories (for Python 2.7)
#  ----------------------------
#   install bootstrap tools
#   RUN apt-get update && apt-get install --no-install-recommends -y \
#       python-rosdep \
#       python-rosinstall \
#       python-vcstools \
#       && rm -rf /var/lib/apt/lists/*
#   RUN apt-get update && apt-get install -y \
#         ros-kinetic-ros-base && rm -rf /var/lib/apt/lists/*
#
#   RUN apt-get install ros-kinetic-*
#   # Install additional dependencies
#   RUN apt-get install -y ros-kinetic-cv-bridge
#   RUN apt-get install -y ros-kinetic-robot-state-publisher ros-kinetic-control-msgs
#   Install from sources
#  ----------------------------
#   RUN apt-get install python3-rosdep python3-rosinstall-generator python3-wstool \
#             python3-rosinstall build-essential
#   or alternatively,
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libboost-all-dev=1.58.0.1ubuntu1 -y )
RUN pip3 install --upgrade pip
RUN pip3 install -U rosdep rosinstall_generator wstool rosinstall
RUN pip3 install rospkg catkin_pkg empy
#   bootstrap rosdep
RUN rosdep init \
 && rosdep update
RUN mkdir ~/ros_catkin_ws
#   Create package set
RUN cd ~/ros_catkin_ws \
 && rosinstall_generator ros_comm --rosdistro kinetic --deps --wet-only --tar > kinetic-ros_comm-wet.rosinstall
#   Fetch packages
RUN cd ~/ros_catkin_ws \
 && wstool init -j1 src kinetic-ros_comm-wet.rosinstall
#   # Solve dependencies
#   RUN cd ~/ros_catkin_ws && rosdep install --from-paths src --ignore-src --rosdistro kinetic -y
#   Create symbolic link for the compilation
RUN cd /usr/bin \
 && ln -sf python3 python
#   Install console_bridge from packages
RUN (apt-get update ;apt-get install --no-install-recommends libconsole-bridge-dev=0.3.2-1 -y )
#   # Compile/install console_bridge as a library
#   RUN git clone git://github.com/ros/console_bridge.git
#   RUN cd console_bridge && cmake . && make
#   RUN cd console_bridge && make install
RUN (apt-get update ;apt-get install --no-install-recommends libtinyxml-dev=2.6.2-3 liblz4-dev=0.0~r131-2ubuntu2 libbz2-dev=1.0.6-8ubuntu0.2 liburdfdom-dev=0.4.1-1 libpoco-dev=1.3.6p1-5.1ubuntu0.1 libtinyxml2-dev=2.2.0-1.1ubuntu1 -y )
#   # Compile the basic ROS packages, optimize docker production
#   RUN cd ~/ros_catkin_ws && ./src/catkin/bin/catkin_make_isolated -DPYTHON_VERSION=3.5 --install -DCMAKE_BUILD_TYPE=Release
#   Add a few packages and dependencies by hand
#   RUN cd ~/ros_catkin_ws/src && git clone https://github.com/ros/console_bridge
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros-controls/control_toolbox
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros-controls/realtime_tools
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/actionlib
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/pluginlib
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/class_loader
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/urdf
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros-simulation/gazebo_ros_pkgs
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/common_msgs
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros-controls/control_msgs
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/vmayoral/dynamic_reconfigure
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/geometry
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/erlerobot/orocos_kinematics_dynamics
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/angles
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/geometry2
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/rosconsole_bridge
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/nodelet_core
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/bond_core
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros-perception/image_common
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros-perception/vision_opencv
#   #--------------------
#   # Follow up with the ROS intallation, splited in this funny way to optimize docker's performance
#   #--------------------
RUN (apt-get update ;apt-get install --no-install-recommends libeigen3-dev=3.3~beta1-2 python3-sip=4.17+dfsg-1ubuntu0.1 python3-sip-dev=4.17+dfsg-1ubuntu0.1 libyaml-cpp-dev=0.5.2-4ubuntu1~16.04.4 libboost-python-dev=1.58.0.1ubuntu1 unzip=6.0-20ubuntu1.1 -y )
#   this installs an old version, not valid: libopencv-dev
#   # Ignore some repositories due to some issues with libboost_python3
#   #       https://github.com/ros/ros-overlay/issues/93
#   RUN touch /root/ros_catkin_ws/src/image_common/camera_calibration_parsers/CATKIN_IGNORE
#   RUN touch /root/ros_catkin_ws/src/image_common/camera_info_manager/CATKIN_IGNORE
RUN cd /usr/lib/x86_64-linux-gnu/ \
 && ln -s libboost_python-py35.so libboost_python3.so
#   #--------------------
#   # Install OpenCV
#   #--------------------
#   # From sources
#   RUN git clone https://github.com/opencv/opencv
#   RUN cd opencv && mkdir build && cd build && cmake .. && make
#   RUN cd opencv/build && make install
#   Compile OpenCV from sources
WORKDIR /root
RUN wget https://github.com/opencv/opencv/archive/3.2.0.zip
RUN ls /root
RUN unzip 3.2.0.zip
RUN mv opencv-3.2.0 OpenCV
RUN cd OpenCV \
 && mkdir build \
 && cd build \
 && cmake -DWITH_QT=ON -DWITH_OPENGL=ON -DFORCE_VTK=ON -DWITH_TBB=ON -DWITH_GDAL=ON -DWITH_XINE=ON -DBUILD_EXAMPLES=ON -DENABLE_PRECOMPILED_HEADERS=OFF ..
RUN cd OpenCV/build \
 && make -j4
RUN cd OpenCV/build \
 && make install
RUN cd OpenCV/build \
 && ldconfig
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros/diagnostics
RUN cd ~/ros_catkin_ws/src \
 && git clone https://github.com/ros-controls/ros_control
#   Compile the again the workspace
RUN cd ~/ros_catkin_ws \
 && ./src/catkin/bin/catkin_make_isolated -DPYTHON_VERSION=3.5 --install -DCMAKE_BUILD_TYPE=Release -DCATKIN_ENABLE_TESTING=OFF
#   Debug
#   RUN ls -l /opt/ros
#   upgrade pip
#  RUN apt-get install python3-pyqt4
#   #--------------------
#   # Install ROS 2
#   #--------------------
#   Inspired on https://github.com/osrf/docker_images/blob/master/ros2/source/source/Dockerfile
RUN sh -c 'echo "deb http://packages.ros.org/ros/ubuntu xenial main" > /etc/apt/sources.list.d/ros-latest.list'
RUN apt-key adv --keyserver ha.pool.sks-keyservers.net --recv-keys 421C365BD9FF1F717815A3895523BAEEB01FA116
#   setup environment
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
RUN pip3 install configparser
#   RUN apt-get install python-pkg-resources
#   install packages
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends clang-format=1:3.8-33ubuntu3.1 libasio-dev=1:1.10.6-3 libpocofoundation9v5=1.3.6p1-5.1ubuntu0.1 libpocofoundation9v5-dbg=1.3.6p1-5.1ubuntu0.1 libssl-dev=1.0.2g-1ubuntu4.20 openssl=1.0.2g-1ubuntu4.20 python3-coverage=3.7.1+dfsg.1-1ubuntu7 python3-dev=3.5.1-3 python3-empy=3.3.2-1build1 python3-mock=1.3.0-2.1ubuntu1 python3-nose=1.3.7-1 python3-pep8=1.7.0-2 python3-pip=8.1.1-2ubuntu0.6 python3-setuptools=20.7.0-1 python3-vcstool python3-yaml=3.11-3build1 uncrustify=0.59+dfsg1-1.1 wget=1.17.1-1ubuntu1.5 -q -y ) \
 && rm -rf /var/lib/apt/lists/*
#   install python packages
RUN pip3 install -U argcomplete flake8 flake8-import-order
#   clone source
ENV ROS2_WS="/root/ros2_ws"
RUN mkdir -p $ROS2_WS/src
WORKDIR $ROS2_WS
RUN wget https://raw.githubusercontent.com/ros2/ros2/release-latest/ros2.repos \
 && vcs import src < ros2.repos
RUN pip3 install pyparsing pytest
#   build source
WORKDIR $ROS2_WS
RUN src/ament/ament_tools/scripts/ament.py build --build-tests --cmake-args -DSECURITY=OFF -- --isolated --parallel --symlink-install
WORKDIR /root
#  --------------------
#   Install Sophus
#  --------------------
#   RUN git clone https://github.com/stonier/sophus -b indigo && \
#      cd sophus && mkdir build && cd build && cmake .. && make
#   RUN ls -l
#   RUN cd sophus/build && make install
#   RUN echo "## Sophus installed ##\n"
#   # FROM pip
#   RUN pip3 install opencv-python
#  RUN cd /usr/local/gym
#   More dependencies
RUN pip3 install h5py
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends bash-completion=1:2.1-4.2ubuntu1.1 python3-defusedxml=0.4.1-2ubuntu0.16.04.1 python3-skimage=0.10.1-2build1 -y )
#  --------------------
#   Install baselines
#  --------------------
RUN pip3 install baselines
RUN pip3 install netifaces
#  --------------------
#   Install individual environments
#  --------------------
WORKDIR /root
#   Turtlebot
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/turtlebot/turtlebot
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/turtlebot/turtlebot_create
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/turtlebot/turtlebot_simulator
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-drivers/joystick_drivers.git
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/yujinrobot/kobuki
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/yujinrobot/kobuki_core
#   RUN cd ros_catkin_ws/src && git clone https://github.com/erlerobot/kobuki_desktop
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/yujinrobot/kobuki_msgs
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-planning/navigation
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-perception/perception_pcl.git
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros/xacro
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/yujinrobot/yocs_msgs
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/yujinrobot/yujin_ocs
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/stonier/ecl_core
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/stonier/ecl_lite
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/stonier/ecl_navigation
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/stonier/ecl_tools
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-drivers/driver_common.git
RUN (apt-get update ;apt-get install --no-install-recommends libftdi-dev=0.20-4build1 -y )
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-visualization/rqt_robot_dashboard
RUN (apt-get update ;apt-get install --no-install-recommends libsdl-dev libsdl-image1.2-dev=1.2.12-5+deb9u1ubuntu0.16.04.1 libspnav-dev=0.2.3-1 -y )
#   installing pcl-dev causes some conflicts
RUN (apt-get update ;apt-get install --no-install-recommends libpcl-dev=1.7.2-14ubuntu0.1 -y ) ; exit 0
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-perception/pcl_msgs
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-perception/pcl_conversions
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-gbp/bfl-release -b release/kinetic/bfl
#   # CATKIN_IGNORE kobuki_gazebo_plugins
#   RUN cd ros_catkin_ws/src/kobuki_desktop/kobuki_gazebo_plugins && touch CATKIN_IGNORE
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/yujinrobot/kobuki_desktop -b devel
#   Fix obtained from https://aur.archlinux.org/packages/ros-indigo-kobuki-gazebo-plugins/
#   checkout version 0.5.1, which compiles
RUN cd ros_catkin_ws/src/kobuki_desktop \
 && git checkout 3d837662928748cf1e229d2e0b0d98f1031ed4a4
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-perception/laser_geometry
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-planning/navigation_msgs
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros/roslint
#   CATKIN_IGNORE wiimote
RUN cd ros_catkin_ws/src/joystick_drivers/wiimote \
 && touch CATKIN_IGNORE
#   # CATKIN_IGNORE yocs_ar_marker_tracking
#   RUN cd ros_catkin_ws/src/yujin_ocs/yocs_ar_marker_tracking && touch CATKIN_IGNORE
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros-perception/ar_track_alvar
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros/resource_retriever
#   CATKIN_IGNORE kobuki test suites
RUN cd ros_catkin_ws/src/kobuki/kobuki_testsuite \
 && touch CATKIN_IGNORE
RUN cd ros_catkin_ws/src/kobuki_desktop/kobuki_qtestsuite \
 && touch CATKIN_IGNORE
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros/robot_state_publisher
RUN cd ros_catkin_ws/src \
 && git clone https://github.com/ros/kdl_parser
#   Compile the again the workspace
RUN cd ~/ros_catkin_ws \
 && ./src/catkin/bin/catkin_make_isolated -DPYTHON_VERSION=3.5 --install -DCMAKE_BUILD_TYPE=Release -DCATKIN_ENABLE_TESTING=OFF
#  --------------------
#   Copy the code
#  --------------------
#   this invalidates the cache
RUN mkdir gym-gazebo
#   COPY . /usr/local/gym/gym-gazebo
COPY . /root/gym-gazebo
#   # #--------------------
#   # # Install deep learning toolkits
#   # #--------------------
#   # # install dependencies
#   # RUN pip install h5py
#   # RUN apt-get install gfortran -y
#   #
#   # # install sript specific dependencies (temporal)
#   # RUN apt-get install python-skimage -y
#   #
#   # # install Theano
#   # #RUN git clone git://github.com/Theano/Theano.git
#   # #RUN cd Theano/ && python setup.py develop
#   # RUN pip install Theano
#   #
#   # # install Keras
#   # RUN pip install keras
#
#  --------------------
#   Install gym-gazebo
#  --------------------
RUN cd gym-gazebo \
 && pip3 install -e .
#   # old method
#   # install dependencies
#   RUN cd /usr/local/gym/gym-gazebo/gym_gazebo/envs/installation && bash setup.bash
#  WORKDIR /root
#  ENTRYPOINT ["/usr/local/gym/bin/docker_entrypoint"]
#   setup entrypoint
#  RUN ls /usr/local/gym/gym-gazebo/
#  RUN ls ./gym-gazebo
#  COPY /usr/local/gym/gym-gazebo/entrypoint.sh /
#
#  --------------------
#   Entry point
#  --------------------
COPY entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
