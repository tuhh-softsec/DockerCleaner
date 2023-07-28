FROM ubuntu:bionic
ARG BRIDGE=false
ARG INSTALL_TURTLEBOT2_DEMO_DEPS=false
ARG INSTALL_CONNEXT_DEBS=false
ARG PLATFORM=x86
ARG ROS1_DISTRO=melodic
ARG UBUNTU_DISTRO=bionic
ARG COMPILE_WITH_CLANG=false
#   Prevent errors from apt-get.
#   See: http://askubuntu.com/questions/506158/unable-to-initialize-frontend-dialog-when-using-ssh
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 -y
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
#   net-tools is for ifconfig
#   Get curl for fetching the repo keys.
#   Get https transport for APT.
RUN apt-get update \
 && apt-get install --no-install-recommends lsb-release=9.20170808ubuntu1 net-tools=1.60+git20161116.90da8a0-1ubuntu1 sudo=1.8.21p2-3ubuntu1.5 curl=7.58.0-2ubuntu3.24 gnupg2=2.2.4-1ubuntu1.6 apt-transport-https=1.6.14 -y
#   Add the ROS repositories to the apt sources list.
RUN echo "deb http://repositories.ros.org/ubuntu/testing/ `lsb_release -cs ` main" > /etc/apt/sources.list.d/ros-latest.list
RUN echo "Bust Cache for key update 2019-06-08" \
 && curl --silent http://repositories.ros.org/repos.key | apt-key add -
#   Add the OSRF repositories to the apt sources list.
RUN echo "deb http://packages.osrfoundation.org/gazebo/ubuntu `lsb_release -cs ` main" > /etc/apt/sources.list.d/gazebo-latest.list
RUN curl --silent http://packages.osrfoundation.org/gazebo.key | apt-key add -
#   Install some development tools.
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 ccache=3.4.1-1 cmake=3.10.2-1ubuntu2.18.04.2 pkg-config=0.29.1-0ubuntu2 python3-empy=3.3.2-1build1 python3-setuptools=39.0.1-2ubuntu0.1 python3-vcstool -y
RUN if test ${UBUNTU_DISTRO} != xenial ; then apt-get update \
 && apt-get install --no-install-recommends python3-lark-parser python3-opencv=3.2.0+dfsg-4ubuntu0.1 -y ; fi
#   Install build and test dependencies of ROS 2 packages.
RUN apt-get update \
 && apt-get install --no-install-recommends clang-format=1:6.0-41~exp5~ubuntu1 cppcheck=1.82-1 git=1:2.17.1-1ubuntu0.17 liblog4cxx-dev=0.10.0-13ubuntu2 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxml2-utils=2.9.4+dfsg1-6.1ubuntu1.8 libxslt-dev pydocstyle=2.0.0-1 pyflakes=1.6.0-1 python3-coverage=4.5+dfsg.1-3 python3-cryptography=2.1.4-1ubuntu1.4 python3-flake8=3.5.0-1 python3-lxml=4.2.1-1ubuntu0.6 python3-mock=2.0.0-3 python3-nose=1.3.7-3 python3-numpy=1:1.13.3-2ubuntu1 python3-pep8=1.7.1-1ubuntu1 python3-pyparsing=2.2.0+dfsg1-2 python3-yaml=3.12-1build2 uncrustify=0.66.1+dfsg1-1 -y
#   Install and self update pip/setuptools to the latest version.
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pip=9.0.1-2.3~ubuntu1.18.04.8 -y
RUN pip3 install -U setuptools pip virtualenv
#   Install clang if build arg is true
RUN if test ${COMPILE_WITH_CLANG} = true ; then apt-get update \
 && apt-get install --no-install-recommends clang=1:6.0-41~exp5~ubuntu1 libc++-dev=6.0-2 libc++abi-dev=6.0-2 -y ; fi
#   Install coverage build dependencies.
RUN apt-get update \
 && apt-get install --no-install-recommends gcovr=3.4-1 -y
#   Install the OpenSplice binary from the OSRF repositories.
RUN apt-get update \
 && apt-get install --no-install-recommends libopensplice69=6.9.190403+osrf1-1~$UBUNTU_DISTRO -y
#   Update default domain id.
RUN sed -i "s/<Id>0<\/Id>/<Id>108<\/Id>/" /usr/etc/opensplice/config/ospl.xml
#   Install the Connext binary from the OSRF repositories.
RUN
#   Install the RTI dependencies.
RUN if test ${PLATFORM} = x86 ; then apt-get update \
 && apt-get install --no-install-recommends default-jre-headless=2:1.11-68ubuntu1~18.04.1 -y ; fi
#   Install dependencies for RTI web binaries install script.
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pexpect=4.2.1-1 -y
#   Get and install the RTI web binaries.
RUN cd /tmp \
 && curl --silent https://s3.amazonaws.com/RTI/Bundles/5.3.1/Evaluation/rti_connext_dds_secure-5.3.1-eval-x64Linux3gcc5.4.0.tar.gz | tar -xz
RUN cd /tmp \
 && tar -xvf /tmp/openssl-1.0.2n-target-x64Linux3gcc5.4.0.tar.gz
COPY rti_web_binaries_install_script.py /tmp/rti_web_binaries_install_script.py
#   Add the RTI license file.
COPY rticonnextdds-license/rti_license.dat /tmp/rti_license.dat
#   Add the RTI binaries we made.
#   ADD rticonnextdds-src/librticonnextdds52_5.2.3-1_amd64.deb /tmp/librticonnextdds52_5.2.3-1_amd64.deb
#   ADD rticonnextdds-src/librticonnextdds52-dev_5.2.3-1_amd64.deb /tmp/librticonnextdds52-dev_5.2.3-1_amd64.deb
#   ADD rticonnextdds-src/rticonnextdds-tools_5.2.3-1_amd64.deb /tmp/rticonnextdds-tools_5.2.3-1_amd64.deb
#   Install the eProsima dependencies.
RUN apt-get update \
 && apt-get install --no-install-recommends libasio-dev=1:1.10.8-1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libtinyxml2-dev=6.0.0+dfsg-1 valgrind=1:3.13.0-2ubuntu2.3 -y
#   Install OpenCV.
RUN apt-get update \
 && apt-get install --no-install-recommends libopencv-dev=3.2.0+dfsg-4ubuntu0.1 -y
#   Install console_bridge for class_loader et al.
RUN if test ${UBUNTU_DISTRO} != xenial ; then apt-get update \
 && apt-get install --no-install-recommends libconsole-bridge-dev=0.4.0+dfsg-2 -y ; fi
#   Install build dependencies for class_loader.
#   We are building poco from source on xenial as we need at least 1.4.1p1 and xenial ships with 1.3.6p1 (https://github.com/ros2/poco_vendor/pull/10)
RUN if test ${UBUNTU_DISTRO} != xenial ; then apt-get update \
 && apt-get install --no-install-recommends libpoco-dev=1.8.0.1-1ubuntu4 -y ; fi
#   Install build dependencies for rviz et al.
RUN apt-get update \
 && apt-get install --no-install-recommends libassimp-dev=4.1.0~dfsg-3 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libfreetype6-dev=2.8.1-2ubuntu2.2 libgles2-mesa-dev=20.0.8-0ubuntu1~18.04.1 libglu1-mesa-dev=9.0.0-2.1build1 libqt5core5a=5.9.5+dfsg-0ubuntu2.6 libqt5gui5=5.9.5+dfsg-0ubuntu2.6 libqt5opengl5=5.9.5+dfsg-0ubuntu2.6 libqt5widgets5=5.9.5+dfsg-0ubuntu2.6 libxaw7-dev=2:1.0.13-1 libxrandr-dev=2:1.5.1-1 qtbase5-dev=5.9.5+dfsg-0ubuntu2.6 -y
#   Install build dependencies for rqt et al.
RUN apt-get update \
 && apt-get install --no-install-recommends pyqt5-dev=5.10.1+dfsg-1ubuntu2 python3-pyqt5=5.10.1+dfsg-1ubuntu2 python3-pyqt5.qtsvg=5.10.1+dfsg-1ubuntu2 python3-sip-dev=4.19.7+dfsg-1ubuntu0.1 python3-pydot=1.2.3-1 python3-pygraphviz=1.4~rc1-1build2.1 -y
#   Install dependencies for robot_model and robot_state_publisher
RUN apt-get update \
 && apt-get install --no-install-recommends libtinyxml-dev=2.6.2-4 libeigen3-dev=3.3.4-4 -y
#   Install Python3 development files.
RUN apt-get update \
 && apt-get install --no-install-recommends python3-dev=3.6.7-1~18.04 -y
#   automatic invalidation once every day.
RUN echo "@today_str"
RUN
#   Install build and test dependencies of ros1_bridge.
RUN if test ${BRIDGE} = true ; then apt-get update \
 && apt-get install --no-install-recommends python-rospkg=1.1.4-1 ros-${ROS1_DISTRO}-common-msgs ros-${ROS1_DISTRO}-rosbash ros-${ROS1_DISTRO}-roscpp ros-${ROS1_DISTRO}-roslaunch ros-${ROS1_DISTRO}-rosmsg ros-${ROS1_DISTRO}-roscpp-tutorials ros-${ROS1_DISTRO}-rospy-tutorials ros-${ROS1_DISTRO}-tf2-msgs -y ; fi
#   Install build dependencies for turtlebot demo (not supported on xenial).
RUN
#   Install dependencies for RViz visual tests
RUN apt-get update \
 && apt-get install --no-install-recommends libgl1-mesa-dri=20.0.8-0ubuntu1~18.04.1 libglapi-mesa=20.0.8-0ubuntu1~18.04.1 libosmesa6=20.0.8-0ubuntu1~18.04.1 mesa-utils=8.4.0-1 xvfb=2:1.19.6-1ubuntu4.14 matchbox-window-manager=1.2-osso21-2 -y
ENV DISPLAY=":99"
#   Create a user to own the build output.
RUN useradd -u 1234 -m rosbuild
RUN sudo -H -u rosbuild -- git config --global user.email "jenkins@ci.ros2.org"
RUN sudo -H -u rosbuild -- git config --global user.name "Jenkins ROS 2"
RUN echo 'rosbuild ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
WORKDIR "@workdir"
#   Add an entry point which changes rosbuild's UID from 1234 to the UID of the invoking user.
#   This means that the generated files will have the same ownership as the host OS user.
COPY entry_point.sh /entry_point.sh
RUN chmod 755 /entry_point.sh
ENTRYPOINT ["/entry_point.sh"]
CMD ["matchbox-window-manager", ">", "/dev/null", "2>&1&;", "python3", "-u", "run_ros2_batch.py", "$CI_ARGS"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
