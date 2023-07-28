FROM ubuntu:16.04
#  MAINTAINER Gary Bradski
#  #######################################################################
#   Running this docker to set up a shared directory and display with the host:
#
#   To newly create and build this docker image:
#   ============================================
#
#   Create a directory <container_name>: 
#      $ mkdir <container_name>
#   Copy this Dockerfile into that directory:
#      cp Dockerfile <container_name>/.
#   Move to that directory:
#      $ cd <container_name>
#   To build the docker file (might have to run with sudo 
#      $ sudo docker build -t <container_name> .
#
#   To run the image, or run it again retaining its state 
#   =====================================================
#      but also exporting display from the container and
#      sharing a directory between host and container:
#
#   Allow other processes to share the display:
#      $ xhost +    #Allows or other processes to capture (show) the display
#   Now run the docker (Usually $DISPLAY is :0) and allow use of the camera -- you may need sudo privalage
#      $ sudo docker run  -it  -e DISPLAY=$DISPLAY  -v /tmp/.X11-unix:/tmp/.X11-unix \
#                         --device /dev/video0 \
#                         -v /<path_to_a host_directory>/<directory_on_host>/:/<directory_path/name>/  <container_name>
#
#   =======================================================
#   Handy docker commands:
#   List all the docker images
#      $ sudo docker ps -a  
#   If the docker image is stopped (otherwise can skip the first command below if not stopped)
#      $ sudo docker start <container ID from ps -a above>
#      $ sudo docker attach <container ID from ps -a above>
#  #######################################################################
#   This is a docker file which will, from scratch:
#
#     * pull in all the dependencies needed for OpenCV 3.2 including python 2 dependencies
#     * pull in OpenCV 3.2 and opencv_contrib and build them
#         + executable files end up in opencv-3.2.0/build/bin
#     * pull in the Learning OpenCV 3 example code and build it
#         + executable files end up in Learning_OpenCV-3_examples/build
#     * To get to the top level directory, just type: cd
#
#   If you just want to do this "by hand" in your home, replace the "RUN"s below with "sudo"
#
#   This Docker uses the ubuntu 16.04 version of ffmpeg, which is older than the ones in my other dockerfiles.
#   this shouldn't cause you any problems but definitely *DO NOT* use this for generating audiofiles / movies for redistribution.
#
#   But it is somewhat less capable than the ones in the ffmpeg containers.
#  #######################################################################
#   First: get all the dependencies:
#
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 pkg-config=0.29.1-0ubuntu1 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libswscale-dev=7:2.8.17-0ubuntu0.1 python-dev=2.7.12-1~16.04 python-numpy=1:1.11.0-1ubuntu1 libtbb2=4.4~20151115-0ubuntu3 libtbb-dev=4.4~20151115-0ubuntu3 libjpeg-dev=8c-2ubuntu8 libpng-dev libtiff-dev libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libdc1394-22-dev=2.2.4-1 unzip=6.0-20ubuntu1.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 -y )
#   Just get a simple editor for convienience (you could just cancel this line)
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:7.4.1689-3ubuntu1.5 -y )
#   Second: get and build OpenCV 3.2
#
RUN cd \
 && wget https://github.com/opencv/opencv/archive/3.2.0.zip \
 && unzip 3.2.0.zip \
 && cd opencv-3.2.0 \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j8 \
 && make install \
 && cd \
 && rm 3.2.0.zip
#   Third: install and build opencv_contrib
#
RUN cd \
 && wget https://github.com/opencv/opencv_contrib/archive/3.2.0.zip \
 && unzip 3.2.0.zip \
 && cd opencv-3.2.0/build \
 && cmake -DOPENCV_EXTRA_MODULES_PATH=../../opencv_contrib-3.2.0/modules/ .. \
 && make -j8 \
 && make install \
 && cd ../.. \
 && rm 3.2.0.zip
#   Forth: get and build the Learning OpenCV 3 examples:
#      I copy the needed data to where the executables will be: opencv-3.2.0/build/bin
#
RUN cd \
 && git clone https://github.com/oreillymedia/Learning-OpenCV-3_examples.git \
 && cd Learning-OpenCV-3_examples \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j8
#  You could then run python and do your stuff...
#  CMD ["python"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
