FROM caffe2ai/caffe2:c2v0.8.0.cpu.full.ubuntu16.04
#  Add Bazel distribution URI as a package source
RUN echo "deb [arch=amd64] http://storage.googleapis.com/bazel-apt stable jdk1.8" | tee /etc/apt/sources.list.d/bazel.list \
 && curl https://bazel.build/bazel-release.pub.gpg | apt-key add -
#  Install some dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends tcl tk ant apt-utils bazel bc build-essential cmake default-jdk doxygen gfortran golang iptables libav-tools libboost-all-dev libeigen3-dev libfreetype6-dev libhdf5-dev libjpeg-turbo8-dev liblcms2-dev libopenblas-dev liblapack-dev libpng12-dev libprotobuf-dev libsdl2-dev libtiff-dev libtiff5-dev libvncserver-dev libzmq3-dev nano net-tools openmpi-bin -y \
 && update-alternatives --set libblas.so.3 /usr/lib/openblas-base/libblas.so.3
RUN pip install Cython h5py ipykernel jupyter matplotlib numpy pandas path.py pyyaml scipy six sklearn sympy Pillow zmq dlib incremental nltk textacy scikit-image spacy tqdm wheel datmo --no-cache-dir
#  Install xgboost
RUN git clone --recursive https://github.com/dmlc/xgboost \
 && cd xgboost \
 && make -j$( nproc ;) \
 && cd python-package \
 && python setup.py install \
 && cd ../.. \
 && rm -rf xgboost
RUN apt-get update \
 && apt-get install --no-install-recommends python-opencv libavcodec-dev libavformat-dev libavresample-dev libdc1394-22-dev libgdal-dev libgphoto2-dev libgtk2.0-dev libjasper-dev liblapacke-dev libopencore-amrnb-dev libopencore-amrwb-dev libopencv-dev libopenexr-dev libswscale-dev libtbb2 libtbb-dev libtheora-dev libv4l-dev libvorbis-dev libvtk6-dev libx264-dev libxine2-dev libxvidcore-dev qt5-default -y
#  Installing opencv
ARG OPENCV_VERSION=3.4.0
RUN cd ~/ \
 && git clone https://github.com/Itseez/opencv.git --branch ${OPENCV_VERSION} --single-branch \
 && git clone https://github.com/Itseez/opencv_contrib.git --branch ${OPENCV_VERSION} --single-branch \
 && cd opencv \
 && mkdir build \
 && cd build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -DWITH_QT=ON -DWITH_OPENGL=ON -D ENABLE_FAST_MATH=1 -DFORCE_VTK=ON -DWITH_TBB=ON -DWITH_GDAL=ON -DWITH_XINE=ON -DBUILD_EXAMPLES=ON -D OPENCV_EXTRA_MODULES_PATH=~/opencv_contrib/modules .. \
 && make -j"$( nproc ;)" \
 && make install \
 && ldconfig \
 && rm -rf ~/opencv*
