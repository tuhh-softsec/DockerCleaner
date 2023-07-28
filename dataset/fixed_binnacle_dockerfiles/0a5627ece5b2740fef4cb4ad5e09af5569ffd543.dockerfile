FROM caffe2ai/caffe2:c2v0.8.1.cuda8.cudnn7.ubuntu16.04
RUN apt-get update \
 && apt-get install --no-install-recommends curl -y
#   Add Bazel distribution URI as a package source
RUN echo "deb [arch=amd64] http://storage.googleapis.com/bazel-apt stable jdk1.8" | tee /etc/apt/sources.list.d/bazel.list \
 && curl https://bazel.build/bazel-release.pub.gpg | apt-key add -
#   Install some dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends tcl tk ant apt-utils bazel bc build-essential cmake default-jdk doxygen gfortran golang iptables libav-tools libboost-all-dev libeigen3-dev libfreetype6-dev libhdf5-dev libjpeg-turbo8-dev liblcms2-dev libopenblas-dev liblapack-dev libpng12-dev libprotobuf-dev libsdl2-dev libtiff-dev libtiff5-dev libvncserver-dev libzmq3-dev nano net-tools openmpi-bin -y \
 && update-alternatives --set libblas.so.3 /usr/lib/openblas-base/libblas.so.3
RUN pip install Cython==0.29.34 h5py==3.8.0 ipykernel==6.22.0 jupyter==1.0.0 matplotlib==3.7.1 numpy==1.24.2 pandas==2.0.0 path.py==12.5.0 pyyaml==6.0 scipy==1.10.1 six==1.16.0 sklearn==0.0.post4 sympy==1.11.1 Pillow==9.5.0 zmq==0.0.0 dlib==19.24.1 incremental==22.10.0 nltk==3.8.1 textacy==0.13.0 scikit-image==0.20.0 spacy==3.5.2 tqdm==4.65.0 wheel==0.40.0 datmo==0.0.40 --no-cache-dir
#   Install xgboost
RUN git clone --recursive https://github.com/dmlc/xgboost \
 && cd xgboost \
 && make -j$( nproc ;) \
 && cd python-package \
 && python setup.py install \
 && cd ../.. \
 && rm -rf xgboost
ARG OPENCV_VERSION=3.4.0
RUN apt-get update \
 && apt-get install --no-install-recommends python-opencv libavcodec-dev libavformat-dev libavresample-dev libdc1394-22-dev libgdal-dev libgphoto2-dev libgtk2.0-dev libjasper-dev liblapacke-dev libopencore-amrnb-dev libopencore-amrwb-dev libopencv-dev libopenexr-dev libswscale-dev libtbb2 libtbb-dev libtheora-dev libv4l-dev libvorbis-dev libvtk6-dev libx264-dev libxine2-dev libxvidcore-dev qt5-default -y
RUN cd ~/ \
 && git clone https://github.com/Itseez/opencv.git --branch ${OPENCV_VERSION} --single-branch \
 && git clone https://github.com/Itseez/opencv_contrib.git --branch ${OPENCV_VERSION} --single-branch \
 && cd opencv \
 && mkdir build \
 && cd build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -DWITH_QT=ON -DWITH_OPENGL=ON -D WITH_CUDA=ON -D CUDA_CUDA_LIBRARY=/usr/local/cuda/lib64/stubs/libcuda.so -D ENABLE_FAST_MATH=1 -D CUDA_FAST_MATH=1 -D WITH_CUBLAS=1 -DFORCE_VTK=ON -DWITH_TBB=ON -DWITH_GDAL=ON -DWITH_XINE=ON -DBUILD_EXAMPLES=ON -D OPENCV_EXTRA_MODULES_PATH=~/opencv_contrib/modules .. \
 && make -j"$( nproc ;)" \
 && make install \
 && ldconfig \
 && rm -rf ~/opencv*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
