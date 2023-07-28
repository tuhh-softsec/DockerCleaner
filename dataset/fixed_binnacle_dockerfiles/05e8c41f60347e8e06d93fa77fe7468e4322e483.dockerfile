#   A Dockerfile for the gym-gazebo environment
#     based in ubuntu 14.04 and Python 2.7
#   Currently fails at:
#
#    E: Version '1.1.4-0*' for 'ros-indigo-ros-core' was not found
#    The command '/bin/sh -c apt-get update && apt-get install -y     ros-indigo-ros-core=1.1.4-0*
#    && rm -rf /var/lib/apt/lists/*' returned a non-zero code: 100
FROM ubuntu:14.04
#  --------------------
#   General setup
#  --------------------
#   Get the dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends xorg-dev=1:7.7+1ubuntu8.1 libgl1-mesa-dev=10.1.3-0ubuntu0.6 xvfb=2:1.15.1-0ubuntu2.11 libxinerama1=2:1.1.3-1 libxcursor1=1:1.1.14-1ubuntu0.14.04.2 unzip=6.0-9ubuntu1.5 libglu1-mesa=9.0.0-2 libav-tools=6:9.20-0ubuntu0.14.04.1 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 python-pyglet=1.1.4.dfsg-2build1 python-setuptools=3.3-1ubuntu2 libpq-dev=9.3.24-0ubuntu0.14.04 libjpeg-dev=8c-2ubuntu8 wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 cmake=2.8.12.2-0ubuntu3 git=1:1.9.1-1ubuntu0.10 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && easy_install pip
WORKDIR /usr/local/gym
#   Copy the code
RUN mkdir gym-gazebo
COPY . /usr/local/gym/gym-gazebo
#  --------------------
#   Install gym
#  --------------------
#   Clone the official gym
RUN git clone https://github.com/openai/gym
#   Install the gym's requirements
RUN pip install -r gym/requirements.txt
#   Install the gym
RUN ls -l
RUN pip install -e gym/
#   Checks
#  RUN python --version
#  RUN python -c "import gym"
#   Debug
#  RUN ls -l /usr/local/gym
#  RUN ls -l /usr/local/gym/gym-gazebo
#  RUN ls -l /usr/local/gym/gym
#  --------------------
#   Install ROS
#  --------------------
#   setup environment
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
#   setup keys
RUN apt-key adv --keyserver ha.pool.sks-keyservers.net --recv-keys 421C365BD9FF1F717815A3895523BAEEB01FA116
#   setup sources.list
RUN echo "deb http://packages.ros.org/ros/ubuntu trusty main" > /etc/apt/sources.list.d/ros-latest.list
#   install bootstrap tools
RUN apt-get update \
 && apt-get install --no-install-recommends python-rosdep python-rosinstall python-vcstools -y \
 && rm -rf /var/lib/apt/lists/*
#   bootstrap rosdep
RUN rosdep init \
 && rosdep update
#   install ros packages
ENV ROS_DISTRO="indigo"
RUN apt-get update \
 && apt-get install --no-install-recommends ros-indigo-ros-core=1.1.4-0* -y \
 && rm -rf /var/lib/apt/lists/*
#      ros-indigo-desktop-full
#  --------------------
#   Install Gazebo
#  --------------------
RUN sudo sh -c 'echo "deb http://packages.osrfoundation.org/gazebo/ubuntu-stable `lsb_release -cs` main" > /etc/apt/sources.list.d/gazebo-stable.list'
RUN wget http://packages.osrfoundation.org/gazebo.key -O - | sudo apt-key add -
RUN sudo apt-get update
RUN sudo apt-get install gazebo7 libgazebo7-dev -y
#   setup environment
EXPOSE 11345/tcp
#   Install additional dependencies
RUN apt-get install --no-install-recommends ros-indigo-cv-bridge -y
RUN apt-get install --no-install-recommends ros-indigo-robot-state-publisher -y
#  --------------------
#   Install deep learning toolkits
#  --------------------
#   install dependencies
RUN sudo pip install h5py
RUN sudo apt-get install gfortran -y
#   install sript specific dependencies (temporal)
RUN sudo apt-get install python-skimage -y
#   install Theano
#  RUN git clone git://github.com/Theano/Theano.git
#  RUN cd Theano/ && sudo python setup.py develop
RUN sudo pip install Theano
#   install Keras
RUN sudo pip install keras
#  --------------------
#   Install gym-gazebo
#  --------------------
RUN cd gym-gazebo \
 && sudo pip install -e .
#   install dependencies
RUN cd /usr/local/gym/gym-gazebo/gym_gazebo/envs/installation \
 && bash setup.bash
#  WORKDIR /root
#  ENTRYPOINT ["/usr/local/gym/bin/docker_entrypoint"]
#   setup entrypoint
#  RUN ls /usr/local/gym/gym-gazebo/
#  RUN ls ./gym-gazebo
#  COPY /usr/local/gym/gym-gazebo/entrypoint.sh /
#  --------------------
#   Entry point
#  --------------------
COPY entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
