#   Jderobot for developers without ROS
#   only use ROS for opencv
FROM jderobot/ubuntu:base
LABEL manteiner="Aitor Martínez Fernández+aitor.martinez.fernandez@gmail.com"
#  # ROS ##
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key 421C365BD9FF1F717815A3895523BAEEB01FA116
RUN echo "deb http://packages.ros.org/ros/ubuntu xenial main" > /etc/apt/sources.list.d/ros-latest.list
#  # ZeroC ##
RUN echo deb http://zeroc.com/download/apt/ubuntu16.04 stable main > /etc/apt/sources.list.d/zeroc.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 5E6DA83306132997
#  # Gazebo ##
RUN echo "deb http://packages.osrfoundation.org/gazebo/ubuntu-stable xenial main" > /etc/apt/sources.list.d/gazebo-stable.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key 67170598AF249743
#  # JdeRobot ##
RUN echo "deb [arch=amd64] http://jderobot.org/apt xenial main" > /etc/apt/sources.list.d/jderobot.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 24E521A4
#  # install deps ##
#   Basic libraries
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential libtool cmake g++ gcc git make -q -y \
 && rm -rf /var/lib/apt/lists/*
#   OpenGL
RUN apt-get update \
 && apt-get install --no-install-recommends freeglut3 freeglut3-dev libgl1-mesa-dev libglu1-mesa -q -y \
 && rm -rf /var/lib/apt/lists/*
#   GTK2
RUN apt-get update \
 && apt-get install --no-install-recommends libgtk2.0-0 libgtk2.0-bin libgtk2.0-cil libgtk2.0-common libgtk2.0-dev libgtkgl2.0-1 libgtkgl2.0-dev libgtkglext1 libgtkglext1-dev libglademm-2.4-dev libgtkmm-2.4-dev libgnomecanvas2-0 libgnomecanvas2-dev libgtkglext1-doc libgnomecanvasmm-2.6-dev libgnomecanvasmm-2.6-1v5 libgtkglextmm-x11-1.2-0v5 libgtkglextmm-x11-1.2-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   GTK3
RUN apt-get update \
 && apt-get install --no-install-recommends libgoocanvasmm-2.0-6 libgoocanvasmm-2.0-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   GSL
RUN apt-get update \
 && apt-get install --no-install-recommends libgsl2 gsl-bin libgsl-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   LibXML
RUN apt-get update \
 && apt-get install --no-install-recommends libxml++2.6-2v5 libxml++2.6-dev libtinyxml-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   EIGEN
RUN apt-get update \
 && apt-get install --no-install-recommends libeigen3-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   FIREWARE
RUN apt-get update \
 && apt-get install --no-install-recommends libdc1394-22 libdc1394-22-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   USB
RUN apt-get update \
 && apt-get install --no-install-recommends libusb-1.0-0 libusb-1.0-0-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   YAML
RUN apt-get update \
 && apt-get install --no-install-recommends libyaml-cpp0.5v5 libyaml-cpp-dev python-yaml -q -y \
 && rm -rf /var/lib/apt/lists/*
#   PYTHON
RUN apt-get update \
 && apt-get install --no-install-recommends python-matplotlib python-pyqt5 python-pip python-numpy python-pyqt5.qtsvg -q -y \
 && rm -rf /var/lib/apt/lists/*
#   QFI
RUN apt-get update \
 && apt-get install --no-install-recommends qfi -q -y \
 && rm -rf /var/lib/apt/lists/*
#   QT5
RUN apt-get update \
 && apt-get install --no-install-recommends qtbase5-dev libqt5declarative5 libqt5script5 libqt5svg5-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   BOOST
RUN apt-get update \
 && apt-get install --no-install-recommends libboost-system-dev libboost-filesystem-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   GSTREAMER
RUN apt-get update \
 && apt-get install --no-install-recommends libgstreamer0.10-dev libgstreamer-plugins-base0.10-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   ICE
RUN apt-get update \
 && apt-get install --no-install-recommends libzeroc-ice3.6 zeroc-ice-utils libzeroc-icestorm3.6 zeroc-ice-slice libzeroc-ice-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   ICE PYTHON
RUN apt-get update \
 && apt-get install --no-install-recommends libssl-dev libbz2-dev -q -y \
 && pip2 install zeroc-ice==3.6.3 \
 && rm -rf /var/lib/apt/lists/*
#   OPENNI 2
RUN apt-get update \
 && apt-get install --no-install-recommends libopenni2-dev libopenni-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   GAZEBO
RUN apt-get update \
 && apt-get install --no-install-recommends gazebo7 libgazebo7-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   PCL
RUN apt-get update \
 && apt-get install --no-install-recommends libpcl-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   OPENCV
RUN apt-get update \
 && apt-get install --no-install-recommends ros-kinetic-opencv3 -q -y \
 && rm -rf /var/lib/apt/lists/*
#   NODEJS
RUN apt-get update \
 && apt-get install --no-install-recommends nodejs -q -y \
 && rm -rf /var/lib/apt/lists/*
#   ARDRONELIB
RUN apt-get update \
 && apt-get install --no-install-recommends ardronelib -q -y \
 && rm -rf /var/lib/apt/lists/*
#   GLOGS
RUN apt-get update \
 && apt-get install --no-install-recommends libgoogle-glog-dev -q -y \
 && rm -rf /var/lib/apt/lists/*
#   YOUTUBE-DL
RUN apt-get update \
 && apt-get install --no-install-recommends youtube-dl -q -y \
 && rm -rf /var/lib/apt/lists/*
COPY ./testPR /bin/
RUN git config --global user.name "JdeRobot" \
 && git config --global user.email johndoe@example.com
WORKDIR /root
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
