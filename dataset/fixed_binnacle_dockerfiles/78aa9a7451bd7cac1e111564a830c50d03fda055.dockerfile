FROM ubuntu:16.04
MAINTAINER Kenji Funaoka <kenji.funaoka@tier4.jp>
#   Develop
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 cmake=3.5.1-1ubuntu3 cmake-curses-gui=3.5.1-1ubuntu3 libboost-all-dev=1.58.0.1ubuntu1 libflann-dev=1.8.4-4.1 libgsl0-dev libgoogle-perftools-dev=2.4-0ubuntu5.16.04.1 libeigen3-dev=3.3~beta1-2 scons=2.4.1-1 pkg-config=0.29.1-0ubuntu1 llvm-dev=1:3.8-33ubuntu3.1 libgl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 libxi-dev=2:1.7.6-1 libx11-dev=2:1.6.3-1ubuntu2.2 libxcb1-dev=1.11.1-1ubuntu1 libxrender-dev=1:0.9.9-0ubuntu1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 flex=2.6.0-11 bison=2:3.0.4.dfsg-1 python-mako=1.0.3+ds1-1ubuntu1 -y
#   Intall some basic GUI and sound libs
RUN apt-get update \
 && apt-get install --no-install-recommends xz-utils=5.1.1alpha+20120614-2ubuntu2 file=1:5.25-2ubuntu1.4 locales=2.23-0ubuntu11.3 dbus-x11=1.10.6-1ubuntu3.6 pulseaudio=1:8.0-0ubuntu3.15 dmz-cursor-theme=0.4.4ubuntu1 fonts-dejavu=2.35-1 fonts-liberation=1.07.4-1 hicolor-icon-theme=0.15-0ubuntu1.1 libcanberra-gtk3-0=0.30-2.1ubuntu1 libcanberra-gtk-module=0.30-2.1ubuntu1 libcanberra-gtk3-module=0.30-2.1ubuntu1 libasound2=1.1.0-0ubuntu1 libgtk2.0-0=2.24.30-1ubuntu1.16.04.2 libdbus-glib-1-2=0.106-1 libxt6=1:1.1.5-0ubuntu1 libexif12=0.6.21-2ubuntu0.6 libgl1-mesa-glx=18.0.5-0ubuntu0~16.04.1 libgl1-mesa-dri=18.0.5-0ubuntu0~16.04.1 language-pack-en=1:16.04+20161009 -y \
 && update-locale LANG=en_US.UTF-8 LC_MESSAGES=POSIX
#   Intall some basic GUI tools
RUN apt-get update \
 && apt-get install --no-install-recommends cmake-qt-gui=3.5.1-1ubuntu3 gnome-terminal=3.18.3-1ubuntu1 -y
#   Intall ROS
RUN sh -c 'echo "deb http://packages.ros.org/ros/ubuntu $(lsb_release -sc) main" > /etc/apt/sources.list.d/ros-latest.list'
RUN apt-key adv --keyserver hkp://ha.pool.sks-keyservers.net:80 --recv-key 421C365BD9FF1F717815A3895523BAEEB01FA116
RUN apt-get update \
 && apt-get install --no-install-recommends ros-kinetic-desktop-full ros-kinetic-nmea-msgs ros-kinetic-nmea-navsat-driver ros-kinetic-sound-play ros-kinetic-jsk-visualization ros-kinetic-grid-map ros-kinetic-gps-common -y
RUN apt-get update \
 && apt-get install --no-install-recommends ros-kinetic-controller-manager ros-kinetic-ros-control ros-kinetic-ros-controllers ros-kinetic-gazebo-ros-control ros-kinetic-joystick-drivers ros-kinetic-rosbridge-server -y
RUN apt-get update \
 && apt-get install --no-install-recommends libnlopt-dev=2.4.2+dfsg-2 freeglut3-dev=2.8.1-2 qtbase5-dev=5.5.1+dfsg-16ubuntu7.7 libqt5opengl5-dev=5.5.1+dfsg-16ubuntu7.7 libssh2-1-dev=1.5.0-2ubuntu0.1 libarmadillo-dev=1:6.500.5+dfsg-1 libpcap-dev=1.7.4-2ubuntu0.1 gksu=2.0.2-9ubuntu1 libgl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 libglew-dev=1.13.0-2 python-wxgtk3.0=3.0.2.0+dfsg-1build1 software-properties-common=0.96.20.10 libmosquitto-dev=1.4.8-1ubuntu0.16.04.7 libyaml-cpp-dev=0.5.2-4ubuntu1~16.04.4 python-flask=0.10.1-2ubuntu0.1 python-requests=2.9.1-3ubuntu0.1 -y
#   Add basic user
ENV USERNAME="autoware"
ENV PULSE_SERVER="/run/pulse/native"
RUN useradd -m $USERNAME \
 && echo "$USERNAME:$USERNAME" | chpasswd \
 && usermod --shell /bin/bash $USERNAME \
 && usermod -aG sudo $USERNAME \
 && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/$USERNAME \
 && chmod 0440 /etc/sudoers.d/$USERNAME \
 && usermod --uid 1000 $USERNAME \
 && groupmod --gid 1000 $USERNAME
#   Setup .bashrc for ROS
RUN echo "source /opt/ros/kinetic/setup.bash" >> /home/$USERNAME/.bashrc \
 && echo "export QT_X11_NO_MITSHM=1" >> /home/$USERNAME/.bashrc \
 && echo "cd" >> /home/$USERNAME/.bashrc
RUN cd /opt \
 && git clone -b mesa-17.0.0 https://anongit.freedesktop.org/git/mesa/mesa.git \
 && cd mesa \
 && scons build=release libgl-xlib \
 && cd .. \
 && mv mesa/build/linux-x86_64/gallium/targets/libgl-xlib ./ \
 && rm -rf mesa
#   Change user
USER autoware
RUN sudo rosdep init \
 && rosdep update \
 && echo "source /opt/ros/kinetic/setup.bash" >> ~/.bashrc
#   YOLO_V2
RUN cd \
 && git clone https://github.com/pjreddie/darknet.git
RUN cd ~/darknet \
 && git checkout 56d69e73aba37283ea7b9726b81afd2f79cd1134
RUN cd ~/darknet/data \
 && wget https://pjreddie.com/media/files/yolo.weights
#   Install Autoware
RUN cd \
 && git clone https://github.com/CPFL/Autoware.git /home/$USERNAME/Autoware
RUN /bin/bash -c 'source /opt/ros/kinetic/setup.bash; cd /home/$USERNAME/Autoware/ros/src; git submodule update --init --recursive; catkin_init_workspace; cd ../; ./catkin_make_release'
RUN echo "source /home/$USERNAME/Autoware/ros/devel/setup.bash" >> /home/$USERNAME/.bashrc
#   Setting
ENV LANG="en_US.UTF-8"
RUN echo "export LANG=\"en_US.UTF-8\"" >> /home/$USERNAME/.bashrc
#   Install dev tools
RUN sudo apt-get -y install vim tmux
#   Change Terminal Color
RUN gconftool-2 --set "/apps/gnome-terminal/profiles/Default/use_theme_background" --type bool false
RUN gconftool-2 --set "/apps/gnome-terminal/profiles/Default/use_theme_colors" --type bool false
RUN gconftool-2 --set "/apps/gnome-terminal/profiles/Default/background_color" --type string "#000000"
#   this will make applications to use libGL.so file from /opt/libgl-xlib folder
ENV LD_LIBRARY_PATH="/opt/libgl-xlib:$LD_LIBRARY_PATH"
#   Default CMD
CMD ["/bin/bash"]
# Please add your HEALTHCHECK here!!!
