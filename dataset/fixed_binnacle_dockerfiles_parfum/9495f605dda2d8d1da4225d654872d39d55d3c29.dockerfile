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
# necessary for dependencies
EXPOSE 80/tcp
# Install OpenCV
RUN echo "deb http://us.archive.ubuntu.com/ubuntu/ precise multiverse\ndeb-src http://us.archive.ubuntu.com/ubuntu/ precise multiverse\ndeb http://us.archive.ubuntu.com/ubuntu/ precise-updates multiverse\ndeb-src http://us.archive.ubuntu.com/ubuntu/ precise-updates multiverse\n" >> /etc/apt/sources.list
RUN apt-get update \
 && apt-get install --no-install-recommends curl cmake wget unzip libopencv-dev build-essential git libgtk2.0-dev pkg-config python-dev python-numpy libdc1394-22 libdc1394-22-dev libjpeg-dev libpng12-dev libtiff4-dev libjasper-dev libavcodec-dev libavformat-dev libswscale-dev libxine-dev libgstreamer0.10-dev libgstreamer-plugins-base0.10-dev libv4l-dev libtbb-dev libqt4-dev libfaac-dev libmp3lame-dev libopencore-amrnb-dev libopencore-amrwb-dev libtheora-dev libvorbis-dev libxvidcore-dev x264 v4l-utils -y
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
# Test OpenCV installation
WORKDIR /fmxnet
RUN pwd
RUN ls
RUN python ./testcv2.py
# install OpenCV and dependencies for mxnet
RUN apt-get update \
 && apt-get install --no-install-recommends python-setuptools python-pip make cmake libopenblas-dev liblapack-dev libgtk-3-dev libboost-all-dev -y
RUN pip install pip --upgrade
RUN pip install --upgrade -r requirements.txt
# build mxnet from source
WORKDIR "mxnet"
RUN make -j $( nproc ;) USE_OPENCV=1 USE_BLAS=openblas
WORKDIR "python"
ENV PYTHONPATH=""\""
RUN python setup.py build
RUN python setup.py install
WORKDIR /fmxnet
# should be at directory containing mxnet and everything else
RUN pwd
RUN ls
# add detect.py
COPY ./detect.py /fmxnet
# does this work as intended?
# should be at toplevel directory
RUN export PYTHONPATH=$PYTHONPATH:$( pwd ;)
# run the detection script
#  need to run with a mounted volume that adds /data/fmxnet_clips to the /fmxnet directory
CMD ["python", "./detect.py"]
