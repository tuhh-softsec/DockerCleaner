#   Image: introlab3it/rtabmap:jfr2018
FROM ubuntu:16.04
WORKDIR /root/
#   Install build dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends libsqlite3-dev=3.11.0-1ubuntu1.5 libpcl-dev=1.7.2-14ubuntu0.1 git=1:2.7.4-0ubuntu1.10 cmake=3.5.1-1ubuntu3 libproj-dev=4.9.2-2 libqt5svg5-dev=5.5.1-2build1 libfreenect-dev=1:0.5.3-1 libopenni2-dev=2.2.0.33+dfsg-6 libyaml-cpp-dev=0.5.2-4ubuntu1~16.04.4 software-properties-common=0.96.20.10 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 pkg-config=0.29.1-0ubuntu1 -y
#   OpenCV (use version <= 3.1.0 for rtabmap 0.16.3 for a solvePnpRansac issue where we have worst ransac performance on newer opencv versions)
RUN git clone https://github.com/opencv/opencv_contrib.git
RUN git clone https://github.com/opencv/opencv.git
RUN cd opencv_contrib \
 && git checkout tags/3.1.0 \
 && cd \
 && cd opencv \
 && git checkout tags/3.1.0 \
 && mkdir build \
 && cd build \
 && cmake -DOPENCV_EXTRA_MODULES_PATH=/root/opencv_contrib/modules -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DBUILD_TESTS=OFF -DBUILD_PERF_TESTS=OFF .. \
 && make -j4 \
 && make install \
 && cd \
 && rm -rf opencv opencv_contrib
#   Install ROS to build other odometry approaches (depending on ROS)
#   install packages
RUN apt-get update \
 && apt-get install --no-install-recommends dirmngr=2.1.11-6ubuntu2.1 gnupg2=2.1.11-6ubuntu2.1 lsb-release=9.20160110ubuntu0.2 -q -y \
 && rm -rf /var/lib/apt/lists/*
#   setup keys
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 421C365BD9FF1F717815A3895523BAEEB01FA116
#   setup sources.list
RUN echo "deb http://packages.ros.org/ros/ubuntu `lsb_release -sc ` main" > /etc/apt/sources.list.d/ros-latest.list
#   install bootstrap tools
RUN apt-get update \
 && apt-get install --no-install-recommends python-rosdep=0.11.4-2 python-rosinstall=0.7.7-1 python-vcstools=0.1.38-1 -y \
 && rm -rf /var/lib/apt/lists/*
#   setup environment
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
#   bootstrap rosdep
RUN rosdep init \
 && rosdep update
#   install ros packages
ENV ROS_DISTRO="kinetic"
RUN apt-get update \
 && apt-get install --no-install-recommends ros-kinetic-ros-core=1.3.2-0* -y \
 && rm -rf /var/lib/apt/lists/*
#   install ROS dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends ros-kinetic-tf ros-kinetic-pcl-ros ros-kinetic-eigen-conversions ros-kinetic-tf-conversions ros-kinetic-random-numbers ros-kinetic-image-transport libblas-dev=3.6.0-2ubuntu2 liblapack-dev=3.6.0-2ubuntu2 libsuitesparse-dev=1:4.4.6-1 libvtk6.2=6.2.0+dfsg1-10ubuntu0.1 libyaml-cpp-dev=0.5.2-4ubuntu1~16.04.4 wget=1.17.1-1ubuntu1.5 libgoogle-glog-dev=0.3.4-0.1 libatlas-base-dev=3.10.2-9 ros-kinetic-sophus ros-kinetic-octomap libglew-dev=1.13.0-2 -y
#   g2o should be built to link on csparse installed system-wide (for rtabmap 0.17.4 build below)
RUN git clone https://github.com/RainerKuemmerle/g2o.git
RUN cd g2o \
 && git checkout 20170730_git \
 && mkdir build \
 && cd build \
 && cmake -DBUILD_LGPL_SHARED_LIBS=ON -DG2O_BUILD_APPS=OFF -DBUILD_WITH_MARCH_NATIVE=OFF -DG2O_BUILD_EXAMPLES=OFF -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release .. \
 && make -j3 \
 && make install \
 && cd \
 && rm -r g2o
#   GTSAM
RUN git clone https://bitbucket.org/gtborg/gtsam.git
RUN cd gtsam \
 && git checkout 4.0.0-alpha2 \
 && mkdir build \
 && cd build \
 && cmake -DMETIS_SHARED=ON -DGTSAM_BUILD_STATIC_LIBRARY=OFF -DGTSAM_USE_SYSTEM_EIGEN=ON -DGTSAM_BUILD_TESTS=OFF -DGTSAM_BUILD_EXAMPLES_ALWAYS=OFF -DCMAKE_BUILD_TYPE=Release .. \
 && make -j3 \
 && make install \
 && cd \
 && rm -r gtsam
#   libpointmatcher 
RUN git clone https://github.com/ethz-asl/libnabo.git
#  commit Apr 25 2018
RUN cd libnabo \
 && git checkout 7e378f6765393462357b8b74d8dc8c5554542ae6 \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release .. \
 && make -j3 \
 && make install \
 && cd \
 && rm -r libnabo
RUN git clone https://github.com/ethz-asl/libpointmatcher.git
#  commit Jan 19 2018
RUN cd libpointmatcher \
 && git checkout 00004bd41e44a1cf8de24ad87e4914760717cbcc \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release .. \
 && make -j3 \
 && make install \
 && cd \
 && rm -r libpointmatcher
#   Clone dependencies
RUN mkdir -p catkin_ws/src
WORKDIR /root/catkin_ws/src
RUN /bin/bash -c '. /opt/ros/${ROS_DISTRO}/setup.bash; catkin_init_workspace'
RUN git clone https://github.com/tum-vision/dvo_slam.git \
 && cd dvo_slam \
 && git checkout kinetic-devel \
 && rm dvo_slam/package.xml \
 && rm dvo_benchmark/package.xml \
 && rm dvo_ros/package.xml
RUN git clone https://github.com/srv/viso2.git \
 && cd viso2 \
 && git checkout kinetic
RUN git clone https://github.com/KumarRobotics/msckf_vio.git \
 && cd msckf_vio \
 && git checkout a9386c5 \
 && wget https://gist.githubusercontent.com/matlabbe/f2518d7427e7f6740af2110e540b1f2b/raw/900a76b0a6741e172e76722e07c0aae50d3b94f3/msckf_vio_a9386c5_ros_commented.patch \
 && git apply --ignore-space-change --ignore-whitespace msckf_vio_a9386c5_ros_commented.patch
RUN git clone https://github.com/srv/libfovis.git \
 && cd libfovis \
 && git checkout db2fc39451e59317cf8486d92085da1c8e414785
RUN git clone https://github.com/ros-perception/vision_opencv.git \
 && cd vision_opencv \
 && git checkout kinetic
RUN git clone https://github.com/laboshinl/loam_velodyne.git \
 && cd loam_velodyne \
 && git checkout a4c364a677647f2a35831439032dc5a58378b3fd
WORKDIR /root
RUN git clone https://github.com/ethz-asl/okvis.git \
 && cd okvis \
 && git checkout 1dce9129f22dd4d21d944788cd9da3a4341586aa \
 && wget https://gist.githubusercontent.com/matlabbe/383be55b5cb682fea217d45ef9a37ef8/raw/3dfc07f3b0b21d07c090f4bb81950f013163f9cf/okvis_1dce912_marchnative_disabled.patch \
 && git apply --ignore-space-change --ignore-whitespace okvis_1dce912_marchnative_disabled.patch
RUN git clone https://github.com/introlab/rtabmap.git \
 && cd rtabmap \
 && git checkout 0.16.3 \
 && wget https://gist.githubusercontent.com/matlabbe/54c69e8c1008d2b3c237f335cb8a6c99/raw/1dc18f66930be3257ab6d93e80a4ce87d0f110bf/rtabmap-0.16.3-euroc-tool-fix.patch \
 && git apply --ignore-space-change --ignore-whitespace rtabmap-0.16.3-euroc-tool-fix.patch
RUN git clone https://github.com/stevenlovegrove/Pangolin.git
RUN git clone https://github.com/raulmur/ORB_SLAM2.git \
 && cd ORB_SLAM2 \
 && wget https://gist.githubusercontent.com/matlabbe/c10403c5d44af85cc3585c0e1c601a60/raw/48adf04098960d86ddf225f1a8c68af87bfcf56e/orbslam2_f2e6f51_marchnative_disabled.patch \
 && git apply --ignore-space-change --ignore-whitespace orbslam2_f2e6f51_marchnative_disabled.patch
#   for dvo, TBB is required (docker related issue)
#   Download and install TBB
ENV TBB_RELEASE="2018_U2"
ENV TBB_VERSION="2018_20171205"
ENV TBB_DOWNLOAD_URL="https://github.com/01org/tbb/releases/download/${TBB_RELEASE}/tbb${TBB_VERSION}oss_lin.tgz"
ENV TBB_INSTALL_DIR="/opt"
RUN wget ${TBB_DOWNLOAD_URL} \
 && tar -C ${TBB_INSTALL_DIR} -xf tbb${TBB_VERSION}oss_lin.tgz \
 && rm tbb${TBB_VERSION}oss_lin.tgz \
 && sed -i "s%SUBSTITUTE_INSTALL_DIR_HERE%${TBB_INSTALL_DIR}/tbb${TBB_VERSION}oss%" ${TBB_INSTALL_DIR}/tbb${TBB_VERSION}oss/bin/tbbvars.* \
 && echo "source ${TBB_INSTALL_DIR}/tbb${TBB_VERSION}oss/bin/tbbvars.sh intel64" >> ~/.bashrc
RUN cd okvis \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j3 \
 && make install \
 && mkdir -p /usr/local/lib/cmake \
 && mv /usr/local/lib/CMake/* /usr/local/lib/cmake/.
#   Pangolin needed for ORB_SLAM2
RUN cd Pangolin \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j3 \
 && make install \
 && cd \
 && rm -rf Pangolin
RUN cd ORB_SLAM2 \
 && cd Thirdparty/DBoW2 \
 && mkdir build \
 && cd build \
 && cmake .. -DCMAKE_BUILD_TYPE=Release \
 && make -j3 \
 && rm -rf * \
 && cd ../../g2o \
 && mkdir build \
 && cd build \
 && cmake .. -DCMAKE_BUILD_TYPE=Release \
 && make -j3 \
 && rm -rf * \
 && cd ../../../ \
 && cd Vocabulary \
 && tar -xf ORBvoc.txt.tar.gz \
 && cd .. \
 && mkdir build \
 && cd build \
 && cmake .. -DCMAKE_BUILD_TYPE=Release \
 && make -j3 \
 && rm -rf *
RUN mv okvis/config/config_fpga_p2_euroc.yaml .
RUN mv ORB_SLAM2/Vocabulary/ORBvoc.txt .
#   Catkin_make
WORKDIR /root/catkin_ws
RUN /bin/bash -c '. /opt/ros/${ROS_DISTRO}/setup.bash;. ${TBB_INSTALL_DIR}/tbb${TBB_VERSION}oss/bin/tbbvars.sh intel64; catkin_make -j3 -DCMAKE_BUILD_TYPE=Release; rm -rf build;'
RUN echo "source /root/catkin_ws/devel/setup.bash" >> ~/.bashrc
ENV LD_LIBRARY_PATH="/opt/tbb2018_20171205oss/lib/intel64/gcc4.7:/root/ORB_SLAM2/lib:/root/ORB_SLAM2/Thirdparty/g2o/lib:/root/ORB_SLAM2/Thirdparty/DBoW2/lib:/root/okvis/build/lib:/root/catkin_ws/devel/lib:/opt/ros/kinetic/lib:$LD_LIBRARY_PATH"
ENV PATH="/root/catkin_ws/devel/bin:/opt/ros/kinetic/bin:$PATH"
ENV ORB_SLAM2_ROOT_DIR="/root/ORB_SLAM2"
WORKDIR /root
#   Build RTAB-Map (using standard g2o, then a version for orbslam2, which uses its own g2o version)
RUN cp -r rtabmap rtabmap_os2 \
 && cp -r rtabmap rtabmap_msckf \
 && cp -r rtabmap rtabmap_loam
RUN cd rtabmap \
 && cd build \
 && cmake -DWITH_G2O=ON .. \
 && make -j3 \
 && make install \
 && rm -rf * \
 && ldconfig
RUN cd rtabmap_os2 \
 && cd build \
 && cmake -DWITH_G2O=OFF .. \
 && make -j3 \
 && rm -rf *
#   rtabmap is crashing if LOAM and MSCKF dependencies are used at the same time, so split them
RUN /bin/bash -c '. /root/catkin_ws/devel/setup.bash; cd rtabmap_loam ; git checkout tools/EurocDataset/main.cpp ;git checkout 0.17.4 ;cd build ;cmake -DWITH_MSCKF_VIO=OFF -DWITH_LOAM=ON -DWITH_G2O=ON -DWITH_FOVIS=OFF -DWITH_DVO=OFF -DWITH_OKVIS=OFF -DWITH_VISO2=OFF .. ;make -j3 ;rm -rf *'
RUN /bin/bash -c '. /root/catkin_ws/devel/setup.bash; cd rtabmap_msckf ; git checkout tools/EurocDataset/main.cpp ;git checkout 0.17.4 ;cd build ;cmake -DWITH_MSCKF_VIO=ON -DWITH_LOAM=OFF -DWITH_G2O=ON -DWITH_FOVIS=OFF -DWITH_DVO=OFF -DWITH_OKVIS=OFF -DWITH_VISO2=OFF .. ;make -j3 ;rm -rf *'
WORKDIR /root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
