FROM ubuntu:bionic
ARG BRIDGE=false
ARG INSTALL_TURTLEBOT2_DEMO_DEPS=false
ARG INSTALL_CONNEXT_DEBS=false
ARG PLATFORM=x86
ARG ROS1_DISTRO=melodic
ARG UBUNTU_DISTRO=bionic
ARG COMPILE_WITH_CLANG=false
#  Prevent errors from apt-get.
#  See: http://askubuntu.com/questions/506158/unable-to-initialize-frontend-dialog-when-using-ssh
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends locales -y
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
#  net-tools is for ifconfig
#  Get curl for fetching the repo keys.
#  Get https transport for APT.
RUN apt-get update \
 && apt-get install --no-install-recommends lsb-release net-tools sudo curl gnupg2 apt-transport-https -y
#  Add the ROS repositories to the apt sources list.
RUN echo "deb http://repositories.ros.org/ubuntu/testing/ `lsb_release -cs ` main" > /etc/apt/sources.list.d/ros-latest.list
RUN echo "Bust Cache for key update 2019-06-08" \
 && curl --silent http://repositories.ros.org/repos.key | apt-key add -
#  Add the OSRF repositories to the apt sources list.
RUN echo "deb http://packages.osrfoundation.org/gazebo/ubuntu `lsb_release -cs ` main" > /etc/apt/sources.list.d/gazebo-latest.list
RUN curl --silent http://packages.osrfoundation.org/gazebo.key | apt-key add -
#  Install some development tools.
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential ccache cmake pkg-config python3-empy python3-setuptools python3-vcstool -y
RUN if test ${UBUNTU_DISTRO} != xenial ; then apt-get update \
 && apt-get install --no-install-recommends python3-lark-parser python3-opencv -y ; fi
#  Install build and test dependencies of ROS 2 packages.
RUN apt-get update \
 && apt-get install --no-install-recommends clang-format cppcheck git liblog4cxx-dev libxml2-dev libxml2-utils libxslt-dev pydocstyle pyflakes python3-coverage python3-cryptography python3-flake8 python3-lxml python3-mock python3-nose python3-numpy python3-pep8 python3-pyparsing python3-yaml uncrustify -y
#  Install and self update pip/setuptools to the latest version.
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pip -y
RUN pip3 install -U setuptools pip virtualenv
#  Install clang if build arg is true
RUN if test ${COMPILE_WITH_CLANG} = true ; then apt-get update \
 && apt-get install --no-install-recommends clang libc++-dev libc++abi-dev -y ; fi
#  Install coverage build dependencies.
RUN apt-get update \
 && apt-get install --no-install-recommends gcovr -y
#  Install the OpenSplice binary from the OSRF repositories.
RUN apt-get update \
 && apt-get install --no-install-recommends libopensplice69=6.9.190403+osrf1-1~$UBUNTU_DISTRO -y
#  Update default domain id.
RUN sed -i "s/<Id>0<\/Id>/<Id>108<\/Id>/" /usr/etc/opensplice/config/ospl.xml
#  Install the Connext binary from the OSRF repositories.
RUN if test ( ${PLATFORM} = x86 -a ${INSTALL_CONNEXT_DEBS} = true ) ; then apt-get update \
 && RTI_NC_LICENSE_ACCEPTED=yes apt-get install -y rti-connext-dds-5.3.1 ; fi
#  Install the RTI dependencies.
RUN if test ${PLATFORM} = x86 ; then apt-get update \
 && apt-get install --no-install-recommends default-jre-headless -y ; fi
#  Install dependencies for RTI web binaries install script.
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pexpect -y
#  Get and install the RTI web binaries.
RUN cd /tmp \
 && curl --silent https://s3.amazonaws.com/RTI/Bundles/5.3.1/Evaluation/rti_connext_dds_secure-5.3.1-eval-x64Linux3gcc5.4.0.tar.gz | tar -xz
RUN cd /tmp \
 && tar -xvf /tmp/openssl-1.0.2n-target-x64Linux3gcc5.4.0.tar.gz
COPY rti_web_binaries_install_script.py /tmp/rti_web_binaries_install_script.py
#  Add the RTI license file.
COPY rticonnextdds-license/rti_license.dat /tmp/rti_license.dat
#  Add the RTI binaries we made.
#  ADD rticonnextdds-src/librticonnextdds52_5.2.3-1_amd64.deb /tmp/librticonnextdds52_5.2.3-1_amd64.deb
#  ADD rticonnextdds-src/librticonnextdds52-dev_5.2.3-1_amd64.deb /tmp/librticonnextdds52-dev_5.2.3-1_amd64.deb
#  ADD rticonnextdds-src/rticonnextdds-tools_5.2.3-1_amd64.deb /tmp/rticonnextdds-tools_5.2.3-1_amd64.deb
#  Install the eProsima dependencies.
RUN apt-get update \
 && apt-get install --no-install-recommends libasio-dev libssl-dev libtinyxml2-dev valgrind -y
#  Install OpenCV.
RUN apt-get update \
 && apt-get install --no-install-recommends libopencv-dev -y
#  Install console_bridge for class_loader et al.
RUN if test ${UBUNTU_DISTRO} != xenial ; then apt-get update \
 && apt-get install --no-install-recommends libconsole-bridge-dev -y ; fi
#  Install build dependencies for class_loader.
#  We are building poco from source on xenial as we need at least 1.4.1p1 and xenial ships with 1.3.6p1 (https://github.com/ros2/poco_vendor/pull/10)
RUN if test ${UBUNTU_DISTRO} != xenial ; then apt-get update \
 && apt-get install --no-install-recommends libpoco-dev -y ; fi
#  Install build dependencies for rviz et al.
RUN apt-get update \
 && apt-get install --no-install-recommends libassimp-dev libcurl4-openssl-dev libfreetype6-dev libgles2-mesa-dev libglu1-mesa-dev libqt5core5a libqt5gui5 libqt5opengl5 libqt5widgets5 libxaw7-dev libxrandr-dev qtbase5-dev -y
#  Install build dependencies for rqt et al.
RUN apt-get update \
 && apt-get install --no-install-recommends pyqt5-dev python3-pyqt5 python3-pyqt5.qtsvg python3-sip-dev python3-pydot python3-pygraphviz -y
#  Install dependencies for robot_model and robot_state_publisher
RUN apt-get update \
 && apt-get install --no-install-recommends libtinyxml-dev libeigen3-dev -y
#  Install Python3 development files.
RUN apt-get update \
 && apt-get install --no-install-recommends python3-dev -y
#  automatic invalidation once every day.
RUN echo "@today_str"
RUN if test ( ${BRIDGE} = true -o ${INSTALL_TURTLEBOT2_DEMO_DEPS} = true ) ; then apt-get update \
 && apt-get install --no-install-recommends ros-${ROS1_DISTRO}-catkin -y ; fi
#  Install build and test dependencies of ros1_bridge.
RUN if test ${BRIDGE} = true ; then apt-get update \
 && apt-get install --no-install-recommends python-rospkg ros-${ROS1_DISTRO}-common-msgs ros-${ROS1_DISTRO}-rosbash ros-${ROS1_DISTRO}-roscpp ros-${ROS1_DISTRO}-roslaunch ros-${ROS1_DISTRO}-rosmsg ros-${ROS1_DISTRO}-roscpp-tutorials ros-${ROS1_DISTRO}-rospy-tutorials ros-${ROS1_DISTRO}-tf2-msgs -y ; fi
#  Install build dependencies for turtlebot demo (not supported on xenial).
RUN if test ( ${UBUNTU_DISTRO} != xenial -a ${INSTALL_TURTLEBOT2_DEMO_DEPS} = true ) ; then apt-get update \
 && apt-get install --no-install-recommends libatlas-base-dev libboost-iostreams-dev libboost-regex-dev libboost-system-dev libboost-thread-dev libcairo2-dev libceres-dev libgoogle-glog-dev liblua5.2-dev libpcl-dev libprotobuf-dev libprotoc-dev libsdl1.2-dev libsdl-image1.2-dev libudev-dev libusb-1.0-0-dev libyaml-cpp-dev protobuf-compiler python3-sphinx ros-${ROS1_DISTRO}-kobuki-driver ros-${ROS1_DISTRO}-kobuki-ftdi -y ; fi
#  Install dependencies for RViz visual tests
RUN apt-get update \
 && apt-get install --no-install-recommends libgl1-mesa-dri libglapi-mesa libosmesa6 mesa-utils xvfb matchbox-window-manager -y
ENV DISPLAY=":99"
#  Create a user to own the build output.
RUN useradd -u 1234 -m rosbuild
RUN sudo -H -u rosbuild -- git config --global user.email "jenkins@ci.ros2.org"
RUN sudo -H -u rosbuild -- git config --global user.name "Jenkins ROS 2"
RUN echo 'rosbuild ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
WORKDIR "@workdir"
#  Add an entry point which changes rosbuild's UID from 1234 to the UID of the invoking user.
#  This means that the generated files will have the same ownership as the host OS user.
COPY entry_point.sh /entry_point.sh
RUN chmod 755 /entry_point.sh
ENTRYPOINT ["/entry_point.sh"]
CMD ["matchbox-window-manager", ">", "/dev/null", "2>&1&;", "python3", "-u", "run_ros2_batch.py", "$CI_ARGS"]
