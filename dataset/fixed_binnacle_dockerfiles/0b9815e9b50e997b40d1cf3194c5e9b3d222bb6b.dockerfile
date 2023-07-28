FROM ubuntu:xenial
RUN apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 -qq -y )
RUN locale-gen en_US en_US.UTF-8 \
 && update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8
ENV LANG="en_US.UTF-8"
RUN sh -c 'echo "deb http://packages.ros.org/ros/ubuntu xenial main" > /etc/apt/sources.list.d/ros-latest.list' \
 && apt-key adv --keyserver ha.pool.sks-keyservers.net --recv-keys 421C365BD9FF1F717815A3895523BAEEB01FA116
#   upgrade distro
RUN : \
 && apt-get -qq dist-upgrade -y \
 && :
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 build-essential=12.1ubuntu2 cppcheck=1.72-1 cmake=3.5.1-1ubuntu3 libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 libpoco-dev=1.3.6p1-5.1ubuntu0.1 libpocofoundation9v5=1.3.6p1-5.1ubuntu0.1 libpocofoundation9v5-dbg=1.3.6p1-5.1ubuntu0.1 python-empy=3.3.2-1build1 python3-dev=3.5.1-3 python3-empy=3.3.2-1build1 python3-nose=1.3.7-1 python3-pip=8.1.1-2ubuntu0.6 python3-setuptools=20.7.0-1 python3-vcstool libtinyxml-dev=2.6.2-3 libeigen3-dev=3.3~beta1-2 libasio-dev=1:1.10.6-3 libtinyxml2-dev=2.2.0-1.1ubuntu1 -qq -y )
#   special depends for ORK renderer
RUN (apt-get update ;apt-get install --no-install-recommends libboost-dev=1.58.0.1ubuntu1 libassimp-dev=3.2~dfsg-3 freeglut3-dev=2.8.1-2 libgl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 libfreeimage-dev=3.17.0+ds1-2ubuntu0.1 libxmu-dev=2:1.1.2-2 libxi-dev=2:1.7.6-1 libsdl1.2-dev=1.2.15+dfsg1-3ubuntu0.1 libosmesa6-dev=18.0.5-0ubuntu0~16.04.1 libusb-1.0-0-dev=2:1.0.20-1 libudev-dev=229-4ubuntu21.31 -qq -y )
#   special depends for Astra camera driver
RUN (apt-get update ;apt-get install --no-install-recommends libboost-system-dev=1.58.0.1ubuntu1 libboost-thread-dev=1.58.0.1ubuntu1 -qq -y )
#   special depends for demo
RUN pip3 install --upgrade pip
RUN pip3 install numpy math3d
#   setup ros2 workspace
ENV ROS2_WS="/root/picky_robot_ws"
RUN rm -rf $ROS2_WS
RUN mkdir -p $ROS2_WS/src
WORKDIR $ROS2_WS
RUN wget https://raw.githubusercontent.com/ros2/ros2/master/ros2.repos
RUN vcs import src < ros2.repos
#   TODO temporary fix for python memory errors
#   RUN cd src/ros2/rosidl && git pull && git checkout fix_destroy_segfault
#   RUN cd src/ros2/rclpy && git pull && git checkout fix_destroy_segfault
#   set up picky robot demos
RUN wget https://raw.githubusercontent.com/Kukanani/picky_robot/ros2/picky_robot.repos
RUN vcs import src/ros2 < picky_robot.repos
#   LINEMOD data and meshes
RUN git clone https://github.com/Kukanani/picky_robot_data.git
#   TODO temporary fix to avoid dependency on kobuki drivers
RUN touch src/ros2/turtlebot2_demo/turtlebot2_follower/AMENT_IGNORE
RUN touch src/ros2/turtlebot2_demo/turtlebot2_drivers/AMENT_IGNORE
#   build everything
WORKDIR $ROS2_WS
RUN src/ament/ament_tools/scripts/ament.py build --symlink-install --isolated --parallel
RUN echo "export ROS2_WS=/root/picky_robot_ws" >> /root/.bashrc \
 && echo "export RMW_IMPLEMENTATION=rmw_fastrtps_cpp" >> /root/.bashrc \
 && echo "source $ROS2_WS/install_isolated/local_setup.bash" >> /root/.bashrc
LABEL com.nvidia.volumes.needed="nvidia_driver"
ENV PATH="/usr/local/nvidia/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/nvidia/lib:/usr/local/nvidia/lib64:${LD_LIBRARY_PATH}"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
