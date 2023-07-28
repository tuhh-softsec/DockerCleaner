FROM ubuntu:16.04
MAINTAINER Gist Noesis <gistnoeis@gmail.com>
ENV DEBIAN_FRONTEND="noninteractive"
RUN sed -i 's#http://archive.ubuntu.com/#http://fr.archive.ubuntu.com/#' /etc/apt/sources.list
#   built-in packages
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 curl=7.47.0-1ubuntu2.19 -y ) \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends supervisor=3.2.0-2ubuntu0.2 openssh-server=1:7.2p2-4ubuntu2.10 pwgen=2.07-1.1ubuntu1 sudo=1.8.16-0ubuntu1.10 vim-tiny=2:7.4.1689-3ubuntu1.5 net-tools=1.60-26ubuntu1 lxde=7ubuntu1 x11vnc=0.9.13-1.2build1 xvfb=2:1.18.4-0ubuntu0.12 gtk2-engines-murrine=0.98.2-0ubuntu2.2 ttf-ubuntu-font-family=1:0.83-0ubuntu2 libreoffice=1:5.1.6~rc2-0ubuntu1~xenial10 firefox=88.0+build2-0ubuntu0.16.04.1 fonts-wqy-microhei=0.2.0-beta-2 language-pack-zh-hant=1:16.04+20160627 language-pack-gnome-zh-hant=1:16.04+20160627 firefox-locale-zh-hant=88.0+build2-0ubuntu0.16.04.1 libreoffice-l10n-zh-tw=1:5.1.4-0ubuntu1 nginx=1.10.3-0ubuntu0.16.04.5 python-pip=8.1.1-2ubuntu0.6 python-dev=2.7.12-1~16.04 build-essential=12.1ubuntu2 mesa-utils=8.3.0-1 libgl1-mesa-dri=18.0.5-0ubuntu0~16.04.1 gnome-themes-standard=3.18.0-2ubuntu2 gtk2-engines-pixbuf=2.24.30-1ubuntu1.16.04.2 gtk2-engines-murrine=0.98.2-0ubuntu2.2 pinta=1.6-2 dbus-x11=1.10.6-1ubuntu3.6 x11-utils=7.7+3 -y --allow-unauthenticated )
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial universe multiverse" >> /etc/apt/sources.list \
 && : \
 && apt-get -y upgrade
#   install some basic utilities
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.1ubuntu2 -y ) \
 && (apt-get update ;apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 htop=2.0.1-1ubuntu1 man unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 nano=2.5.3-2ubuntu2 -y )
#   tini for subreap                                   
ENV TINI_VERSION="v0.9.0"
RUN which wget &> /dev/null || (apt-get update ;apt-get install --no-install-recommends wget=1.20.3 ) ; wget --no-verbose --output-document /bin/tini https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini
RUN chmod +x /bin/tini
RUN (apt-get update ;apt-get install --no-install-recommends v4l-utils=1.10.0-1 ffmpeg=7:2.8.17-0ubuntu0.1 -y )
#   bluez dependencies
RUN (apt-get update ;apt-get install --no-install-recommends libglib2.0-dev=2.48.2-0ubuntu4.8 libical-dev=1.0.1-0ubuntu2 libreadline-dev=6.3-8ubuntu2 libudev-dev=229-4ubuntu21.31 libdbus-1-dev=1.10.6-1ubuntu3.6 libdbus-glib-1-dev=0.106-1 -y )
#   debugging
RUN (apt-get update ;apt-get install --no-install-recommends usbutils=1:007-4 strace=4.11-1ubuntu3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends bluez=5.37-0ubuntu5.3 bluez-hcidump=5.37-0ubuntu5.3 blueman=2.0.4-1ubuntu2.1 checkinstall=1.6.2-4ubuntu1 libusb-dev=2:0.1.12-28 libbluetooth-dev=5.37-0ubuntu5.3 joystick=1:1.4.9-1 -y )
RUN wget http://www.pabr.org/sixlinux/sixpair.c
RUN gcc -o sixpair sixpair.c -lusb
RUN git clone https://github.com/aaronp24/QtSixA.git \
 && cd QtSixA/sixad \
 && make \
 && sudo mkdir -p /var/lib/sixad/profiles \
 && sudo checkinstall -y
RUN (apt-get update ;apt-get install --no-install-recommends python-dbus=1.2.0-3 -y )
#  install opencv3 python 3
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 unzip=6.0-20ubuntu1.1 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 pkg-config=0.29.1-0ubuntu1 libatlas-base-dev=3.10.2-9 gfortran=4:5.3.1-1ubuntu1 libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libswscale-dev=7:2.8.17-0ubuntu0.1 libjpeg-dev=8c-2ubuntu8 libpng-dev libtiff-dev libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libv4l-dev=1.10.0-1 -y )
RUN sudo apt-get install -y python3-pip
RUN pip3 install numpy
RUN wget https://github.com/Itseez/opencv/archive/3.3.0.zip
RUN unzip 3.3.0.zip
RUN mv opencv-3.3.0 /opencv
RUN mkdir /opencv/release
WORKDIR /opencv/release 
RUN cmake -DBUILD_TIFF=ON -DBUILD_opencv_java=OFF -DWITH_CUDA=OFF -DENABLE_AVX=ON -DWITH_OPENGL=ON -DWITH_OPENCL=ON -DWITH_IPP=OFF -DWITH_TBB=ON -DWITH_EIGEN=ON -DWITH_V4L=ON -DWITH_VTK=OFF -DBUILD_TESTS=OFF -DBUILD_PERF_TESTS=OFF -DCMAKE_BUILD_TYPE=RELEASE -DBUILD_opencv_python2=OFF -DCMAKE_INSTALL_PREFIX=$( python3 -c "import sys; print(sys.prefix)" ;) -DPYTHON3_EXECUTABLE=$( which python3 ;) -DPYTHON3_INCLUDE_DIR=$( python3 -c "from distutils.sysconfig import get_python_inc; print(get_python_inc())" ;) -DPYTHON3_PACKAGES_PATH=$( python3 -c "from distutils.sysconfig import get_python_lib; print(get_python_lib())" ;) ..
RUN make -j4
RUN make install
RUN (apt-get update ;apt-get install --no-install-recommends ipython3=2.4.1-1 -y )
RUN pip3 install scipy pyserial evdev zmq
COPY image /
RUN pip install setuptools==67.6.1 wheel==0.40.0 \
 && pip install -r /usr/lib/web/requirements.txt
RUN useradd -ms /bin/bash linn
RUN usermod -a -G audio linn
#  currently we always build against latest but we 'll probably fix the version to increase stability in the future
RUN git clone https://github.com/GistNoesis/Linn-Photobooth.git /root/Linn-Photobooth
EXPOSE 80/tcp
WORKDIR /root
ENV HOME="/home/ubuntu" \
    SHELL="/bin/bash"
ENTRYPOINT ["/startup.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
