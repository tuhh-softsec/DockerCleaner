FROM ubuntu:14.04
WORKDIR /fmxnet
COPY lightened_moon /fmxnet/lightened_moon/
COPY mxnet /fmxnet/mxnet/
WORKDIR "mxnet"
RUN pwd
RUN ls -al
WORKDIR /fmxnet
COPY ./align_dlib.py /fmxnet
COPY ./facenet.py /fmxnet
COPY ./lightened_moon.py /fmxnet
COPY ./requirements.txt /fmxnet
COPY ./shape_predictor_68_face_landmarks.dat /fmxnet
COPY ./testcv2.py /fmxnet
WORKDIR "mxnet"
RUN ls
WORKDIR /fmxnet
WORKDIR "lightened_moon"
RUN ls
WORKDIR /fmxnet
#  necessary for dependencies
EXPOSE 80/tcp
#  Install OpenCV
RUN echo "deb http://us.archive.ubuntu.com/ubuntu/ precise multiverse\ndeb-src http://us.archive.ubuntu.com/ubuntu/ precise multiverse\ndeb http://us.archive.ubuntu.com/ubuntu/ precise-updates multiverse\ndeb-src http://us.archive.ubuntu.com/ubuntu/ precise-updates multiverse\n" >> /etc/apt/sources.list
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.35.0-1ubuntu2.20 cmake=2.8.12.2-0ubuntu3 wget=1.15-1ubuntu1.14.04.5 unzip=6.0-9ubuntu1.5 libopencv-dev=2.4.8+dfsg1-2ubuntu1.2 build-essential=11.6ubuntu6 git=1:1.9.1-1ubuntu0.10 libgtk2.0-dev=2.24.23-0ubuntu1.4 pkg-config=0.26-1ubuntu4 python-dev=2.7.5-5ubuntu3 python-numpy=1:1.8.2-0ubuntu0.1 libdc1394-22=2.2.1-2ubuntu2 libdc1394-22-dev=2.2.1-2ubuntu2 libjpeg-dev=8c-2ubuntu8 libpng12-dev=1.2.50-1ubuntu2.14.04.3 libtiff4-dev=4.0.3-7ubuntu0.11 libjasper-dev=1.900.1-14ubuntu3.5 libavcodec-dev=6:9.20-0ubuntu0.14.04.1 libavformat-dev=6:9.20-0ubuntu0.14.04.1 libswscale-dev=6:9.20-0ubuntu0.14.04.1 libxine-dev=1.1.21-2ubuntu1 libgstreamer0.10-dev=0.10.36-1.2ubuntu3 libgstreamer-plugins-base0.10-dev=0.10.36-1.1ubuntu2.1 libv4l-dev=1.0.1-1 libtbb-dev=4.2~20130725-1.1ubuntu1 libqt4-dev=4:4.8.5+git192-g085f851+dfsg-2ubuntu4.1 libfaac-dev=1.28-6 libmp3lame-dev=3.99.5+repack1-3ubuntu1 libopencore-amrnb-dev=0.1.3-2ubuntu1 libopencore-amrwb-dev=0.1.3-2ubuntu1 libtheora-dev=1.1.1+dfsg.1-3.2 libvorbis-dev=1.3.2-1.3ubuntu1.2 libxvidcore-dev=2:1.3.2-9ubuntu1 x264=2:0.142.2389+git956c8d8-2 v4l-utils=1.0.1-1 -y
RUN mkdir opencv
WORKDIR opencv
RUN wget https://github.com/Itseez/opencv/archive/2.4.8.zip -O opencv-2.4.8.zip
RUN unzip opencv-2.4.8.zip
RUN mkdir opencv-2.4.8/build
WORKDIR opencv-2.4.8/build
RUN cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local -D WITH_TBB=ON -D BUILD_NEW_PYTHON_SUPPORT=ON -D WITH_V4L=ON -D WITH_OPENGL=ON ..
RUN make -j $( nproc ;) \
 && make install
RUN echo "/usr/local/lib" > /etc/ld.so.conf.d/opencv.conf
RUN ldconfig
#  Test OpenCV installation
WORKDIR /fmxnet
RUN pwd
RUN ls
RUN python ./testcv2.py
#  install OpenCV and dependencies for mxnet
RUN apt-get update \
 && apt-get install --no-install-recommends python-setuptools=3.3-1ubuntu2 python-pip=1.5.4-1ubuntu4 make=3.81-8.2ubuntu3 cmake=2.8.12.2-0ubuntu3 libopenblas-dev=0.2.8-6ubuntu1 liblapack-dev=3.5.0-2ubuntu1 libgtk-3-dev=3.10.8-0ubuntu1.6 libboost-all-dev=1.54.0.1ubuntu1 -y
RUN pip install pip==23.1 --upgrade
RUN pip install --upgrade -r requirements.txt
#  build mxnet from source
WORKDIR "mxnet"
RUN make -j $( nproc ;) USE_OPENCV=1 USE_BLAS=openblas
WORKDIR "python"
ENV PYTHONPATH="\"
RUN python setup.py build
RUN python setup.py install
WORKDIR /fmxnet
#  should be at directory containing mxnet and everything else
RUN pwd
RUN ls
#  add detect.py
COPY ./detect.py /fmxnet
#  does this work as intended?
#  should be at toplevel directory
RUN export PYTHONPATH=$PYTHONPATH:$( pwd ;)
#  run the detection script
#   need to run with a mounted volume that adds /data/fmxnet_clips to the /fmxnet directory
CMD ["python", "./detect.py"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
