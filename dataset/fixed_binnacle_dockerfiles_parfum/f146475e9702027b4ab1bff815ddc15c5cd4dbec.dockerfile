#  Image: introlab3it/rtabmap:jfr2018
FROM ubuntu:16.04
WORKDIR /root/
#  Install build dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends libsqlite3-dev libpcl-dev git cmake libproj-dev libqt5svg5-dev libfreenect-dev libopenni2-dev libyaml-cpp-dev software-properties-common libgtk2.0-dev pkg-config -y
#  OpenCV (use version <= 3.1.0 for rtabmap 0.16.3 for a solvePnpRansac issue where we have worst ransac performance on newer opencv versions)
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
#  Install ROS to build other odometry approaches (depending on ROS)
#  install packages
RUN apt-get update \
 && apt-get install --no-install-recommends dirmngr gnupg2 lsb-release -q -y \
 && rm -rf /var/lib/apt/lists/*
#  setup keys
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 421C365BD9FF1F717815A3895523BAEEB01FA116
#  setup sources.list
RUN echo "deb http://packages.ros.org/ros/ubuntu `lsb_release -sc ` main" > /etc/apt/sources.list.d/ros-latest.list
#  install bootstrap tools
RUN apt-get update \
 && apt-get install --no-install-recommends python-rosdep python-rosinstall python-vcstools -y \
 && rm -rf /var/lib/apt/lists/*
#  setup environment
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
#  bootstrap rosdep
RUN rosdep init \
 && rosdep update
#  install ros packages
ENV ROS_DISTRO="kinetic"
RUN apt-get update \
 && apt-get install --no-install-recommends ros-${ROS_DISTRO}-ros-core=1.3.2-0* -y \
 && rm -rf /var/lib/apt/lists/*
#  install ROS dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends libblas-dev liblapack-dev libsuitesparse-dev libvtk6.2 libyaml-cpp-dev wget libgoogle-glog-dev libatlas-base-dev libglew-dev ros-${ROS_DISTRO}-tf ros-${ROS_DISTRO}-pcl-ros ros-${ROS_DISTRO}-eigen-conversions ros-${ROS_DISTRO}-tf-conversions ros-${ROS_DISTRO}-random-numbers ros-${ROS_DISTRO}-image-transport ros-${ROS_DISTRO}-sophus ros-${ROS_DISTRO}-octomap -y
#  g2o should be built to link on csparse installed system-wide
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
#  GTSAM
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
#  libpointmatcher
RUN git clone https://github.com/ethz-asl/libnabo.git
# commit Apr 25 2018
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
# commit Jan 19 2018
RUN cd libpointmatcher \
 && git checkout 00004bd41e44a1cf8de24ad87e4914760717cbcc \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release .. \
 && make -j3 \
 && make install \
 && cd \
 && rm -r libpointmatcher
#  Clone dependencies
RUN mkdir -p catkin_ws/src
WORKDIR /root/catkin_ws/src
RUN /bin/bash -c '. /opt/ros/${ROS_DISTRO}/setup.bash; catkin_init_workspace'
#  OPENCV ROS
RUN git clone https://github.com/ros-perception/vision_opencv.git \
 && cd vision_opencv \
 && git checkout kinetic
#  DVO
RUN git clone https://github.com/tum-vision/dvo_slam.git \
 && cd dvo_slam \
 && git checkout ${ROS_DISTRO}-devel \
 && rm dvo_slam/package.xml \
 && rm dvo_benchmark/package.xml \
 && rm dvo_ros/package.xml
#  VISO2
RUN git clone https://github.com/srv/viso2.git \
 && cd viso2 \
 && git checkout ${ROS_DISTRO}
#  MSCKF-VIO
RUN git clone https://github.com/KumarRobotics/msckf_vio.git \
 && cd msckf_vio \
 && git checkout a9386c5 \
 && wget https://gist.githubusercontent.com/matlabbe/f2518d7427e7f6740af2110e540b1f2b/raw/900a76b0a6741e172e76722e07c0aae50d3b94f3/msckf_vio_a9386c5_ros_commented.patch \
 && git apply --ignore-space-change --ignore-whitespace msckf_vio_a9386c5_ros_commented.patch
#  FOVIS
RUN git clone https://github.com/srv/libfovis.git \
 && cd libfovis \
 && git checkout 896acc8425e9fd7c5609153b8bad349ae1abbb50
#  LOAM
RUN git clone https://github.com/laboshinl/loam_velodyne.git \
 && cd loam_velodyne \
 && git checkout a4c364a677647f2a35831439032dc5a58378b3fd
#  VINS-Fusion
RUN git clone https://github.com/HKUST-Aerial-Robotics/VINS-Fusion.git \
 && cd VINS-Fusion \
 && git checkout e72b5f7ae7437e8cf481cc0bcbfb5fe91bd3d640 \
 && wget https://gist.githubusercontent.com/matlabbe/795ab37067367dca58bbadd8201d986c/raw/0657cc30fca6b1b61faeef42be9f3428861d2598/vins-fusion_e72b5f7.patch \
 && git apply --ignore-space-change --ignore-whitespace vins-fusion_e72b5f7.patch
WORKDIR /root
#  OKVIS
RUN git clone https://github.com/ethz-asl/okvis.git \
 && cd okvis \
 && git checkout 1dce9129f22dd4d21d944788cd9da3a4341586aa \
 && wget https://gist.githubusercontent.com/matlabbe/383be55b5cb682fea217d45ef9a37ef8/raw/bd4f6886eeba3800f0e5171e77abc5dd6dc8bfd9/okvis_1dce912_marchnative_disabled.patch \
 && git apply --ignore-space-change --ignore-whitespace okvis_1dce912_marchnative_disabled.patch
#  ORB_SLAM2
RUN git clone https://github.com/stevenlovegrove/Pangolin.git
RUN git clone https://github.com/raulmur/ORB_SLAM2.git \
 && cd ORB_SLAM2 \
 && wget https://gist.githubusercontent.com/matlabbe/c10403c5d44af85cc3585c0e1c601a60/raw/48adf04098960d86ddf225f1a8c68af87bfcf56e/orbslam2_f2e6f51_marchnative_disabled.patch \
 && git apply --ignore-space-change --ignore-whitespace orbslam2_f2e6f51_marchnative_disabled.patch
RUN git clone https://github.com/introlab/rtabmap.git
#  for dvo, TBB is required (docker related issue)
#  Download and install TBB
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
#  Pangolin needed for ORB_SLAM2
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
RUN cp catkin_ws/src/VINS-Fusion/config/euroc/* .
RUN cp catkin_ws/src/VINS-Fusion/config/kitti_odom/* .
#  Catkin_make
WORKDIR /root/catkin_ws
RUN /bin/bash -c '. /opt/ros/${ROS_DISTRO}/setup.bash;. ${TBB_INSTALL_DIR}/tbb${TBB_VERSION}oss/bin/tbbvars.sh intel64; catkin_make -j3 -DCMAKE_BUILD_TYPE=Release; rm -rf build;'
RUN echo "source /root/catkin_ws/devel/setup.bash" >> ~/.bashrc
ENV LD_LIBRARY_PATH="/opt/tbb2018_20171205oss/lib/intel64/gcc4.7:/root/ORB_SLAM2/lib:/root/ORB_SLAM2/Thirdparty/g2o/lib:/root/ORB_SLAM2/Thirdparty/DBoW2/lib:/root/catkin_ws/devel/lib:/opt/ros/${ROS_DISTRO}/lib:$LD_LIBRARY_PATH"
ENV PATH="/root/catkin_ws/devel/bin:/opt/ros/${ROS_DISTRO}/bin:$PATH"
ENV ORB_SLAM2_ROOT_DIR="/root/ORB_SLAM2"
WORKDIR /root
#  Build RTAB-Map (using standard g2o, then a version for orbslam2, which uses its own g2o version)
RUN cp -r rtabmap rtabmap_os2 \
 && cp -r rtabmap rtabmap_msckf \
 && cp -r rtabmap rtabmap_loam
RUN /bin/bash -c '. /root/catkin_ws/devel/setup.bash; cd rtabmap ; cd build ; cmake -DWITH_G2O=ON .. ; make -j3 ; make install ; rm -rf * ; ldconfig'
#  Version with orb_slam2 (system g2o disabled)
RUN /bin/bash -c '. /root/catkin_ws/devel/setup.bash; cd rtabmap_os2 ; cd build ; cmake -DWITH_G2O=OFF .. ; make -j3 ; rm -rf *'
#  rtabmap is crashing if LOAM and MSCKF dependencies are used at the same time, so split them
RUN /bin/bash -c '. /root/catkin_ws/devel/setup.bash; cd rtabmap_loam ;cd build ;cmake -DWITH_MSCKF_VIO=OFF -DWITH_LOAM=ON -DWITH_G2O=ON -DWITH_FOVIS=OFF -DWITH_DVO=OFF -DWITH_OKVIS=OFF -DWITH_VINS=OFF -DWITH_VISO2=OFF .. ;make -j3 ;rm -rf *'
RUN /bin/bash -c '. /root/catkin_ws/devel/setup.bash; cd rtabmap_msckf ;cd build ;cmake -DWITH_MSCKF_VIO=ON -DWITH_LOAM=OFF -DWITH_G2O=ON -DWITH_FOVIS=OFF -DWITH_DVO=OFF -DWITH_OKVIS=OFF -DWITH_VINS=OFF -DWITH_VISO2=OFF .. ;make -j3 ;rm -rf *'
WORKDIR /root
