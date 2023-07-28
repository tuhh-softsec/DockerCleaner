FROM ubuntu:18.04
MAINTAINER lodemo
ARG TENSORFLOW_VERSION=1.0
ARG TENSORFLOW_ARCH=cpu
ARG OPENCV_VERSION=2.4.13.6
ENV TZ="Europe/London"
RUN :
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends tzdata=2022g-0ubuntu0.18.04 -y )
RUN ln -fs /usr/share/zoneinfo/$TZ /etc/localtime
RUN rm /etc/localtime
RUN dpkg-reconfigure -f noninteractive tzdata
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends bc=1.07.1-2 build-essential=12.4ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 curl=7.58.0-2ubuntu3.24 pkg-config=0.29.1-0ubuntu2 g++=4:7.4.0-1ubuntu2.3 gfortran=4:7.4.0-1ubuntu2.3 git=1:2.17.1-1ubuntu0.17 python2.7=2.7.17-1~18.04ubuntu1.11 python-pip=9.0.1-2.3~ubuntu1.18.04.8 libffi-dev=3.2.1-8 libfreetype6-dev=2.8.1-2ubuntu2.2 libhdf5-dev=1.10.0-patch1+docs-4 libjpeg-dev=8c-2ubuntu8 liblcms2-dev=2.9-1ubuntu0.1 libopenblas-dev=0.2.20+ds-4 liblapack-dev=3.7.1-4ubuntu1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libtiff5-dev=4.0.9-5ubuntu0.10 libwebp-dev=0.6.1-2ubuntu0.18.04.1 libzmq3-dev=4.2.5-1ubuntu0.2 nano=2.9.3-2 python-dev=2.7.15~rc1-1 software-properties-common=0.96.24.32.20 unzip=6.0-21ubuntu1.2 vim=2:8.0.1453-1ubuntu1.11 wget=1.19.4-1ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 qt5-default=5.9.5+dfsg-0ubuntu2.6 libvtk6-dev=6.3.0+dfsg1-11build1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 libjpeg-dev=8c-2ubuntu8 libwebp-dev=0.6.1-2ubuntu0.18.04.1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libtiff5-dev=4.0.9-5ubuntu0.10 libopenexr-dev=2.2.0-11.1ubuntu1.9 libgdal-dev=2.2.3+dfsg-2 libdc1394-22-dev=2.2.5-1 libavcodec-dev=7:3.4.11-0ubuntu0.1 libavformat-dev=7:3.4.11-0ubuntu0.1 libswscale-dev=7:3.4.11-0ubuntu0.1 libtheora-dev=1.1.1+dfsg.1-14 libvorbis-dev=1.3.5-4.2 libxvidcore-dev=2:1.3.5-1 libx264-dev=2:0.152.2854+gite9a5903-2 yasm=1.3.0-2build1 libopencore-amrnb-dev=0.1.3-2.1 libopencore-amrwb-dev=0.1.3-2.1 libv4l-dev=1.14.2-1 libxine2-dev=1.2.8-2build2 libtbb-dev=2017~U7-8 libeigen3-dev=3.3.4-4 python-tk=2.7.17-1~18.04 ffmpeg=7:3.4.11-0ubuntu0.1 -y ) \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
#   Custom compilation of OpenBLAS with OpenMP enabled 
#   (linear algebra is limited to single core in debs)
#   NUM_THREADS must be set otherwise docker hub build
#   non-parallel version.
RUN git clone https://github.com/xianyi/OpenBLAS.git /tmp/OpenBLAS \
 && cd /tmp/OpenBLAS/ \
 && make DYNAMIC_ARCH=1 NO_AFFINITY=1 USE_OPENMP=1 NUM_THREADS=32 \
 && make DYNAMIC_ARCH=1 NO_AFFINITY=1 USE_OPENMP=1 NUM_THREADS=32 install \
 && rm -rf /tmp/OpenBLAS
#   Link BLAS library to use OpenBLAS using the alternatives mechanism (https://www.scipy.org/scipylib/building/linux.html#debian-ubuntu)
#  RUN update-alternatives --set libblas.so.3 /usr/lib/openblas-base/libblas.so.3
#  RUN update-alternatives --list libblas.so.3 /usr/lib/atlas-base/atlas/libblas.so.3 /usr/lib/libblas/libblas.so.3 /usr/lib/openblas-base/libopenblas.so.0
RUN update-alternatives --install /usr/lib/libblas.so libblas.so /opt/OpenBLAS/lib/libopenblas.so 1000
RUN update-alternatives --install /usr/lib/libblas.so.3 libblas.so.3 /opt/OpenBLAS/lib/libopenblas.so 1000
RUN update-alternatives --install /usr/lib/liblapack.so liblapack.so /opt/OpenBLAS/lib/libopenblas.so 1000
RUN update-alternatives --install /usr/lib/liblapack.so.3 liblapack.so.3 /opt/OpenBLAS/lib/libopenblas.so 1000
RUN ldconfig
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends python-nose=1.3.7-3 python-h5py=2.7.1-2 python-skimage=0.13.1-2 python-protobuf=3.0.0-9.1ubuntu1.1 python-openssl=17.5.0-1ubuntu1 python-mysqldb=1.3.10-1build1 -y ) \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
RUN pip install Pillow==9.5.0 --no-cache-dir
#   Install TensorFlow
#  RUN pip --no-cache-dir install \
#  	https://storage.googleapis.com/tensorflow/linux/${TENSORFLOW_ARCH}/tensorflow-${TENSORFLOW_VERSION}-cp27-none-linux_x86_64.whl
#   Install OpenCV
RUN cd /opt \
 && wget https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.zip \
 && unzip ${OPENCV_VERSION}.zip \
 && mkdir -p /opt/opencv-${OPENCV_VERSION}/build \
 && cd /opt/opencv-${OPENCV_VERSION}/build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local -D WITH_FFMPEG=ON -D WITH_IPP=NO -D WITH_OPENEXR=NO -D WITH_TBB=YES -D BUILD_EXAMPLES=NO -D BUILD_ANDROID_EXAMPLES=NO -D INSTALL_PYTHON_EXAMPLES=NO -D BUILD_DOCS=NO -D BUILD_opencv_python2=ON -D BUILD_opencv_python3=NO .. \
 && make VERBOSE=1 \
 && make -j${CPUCOUNT} \
 && make install \
 && rm -rf /opt/opencv-${OPENCV_VERSION}
WORKDIR /catana
COPY . /catana
RUN pip install --no-cache-dir -r requirements.txt
RUN pip install numpy==1.24.2 --no-cache-dir -I
#  RUN pip --no-cache-dir install -r ./src/face_recognition/facenet/requirements.txt
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
