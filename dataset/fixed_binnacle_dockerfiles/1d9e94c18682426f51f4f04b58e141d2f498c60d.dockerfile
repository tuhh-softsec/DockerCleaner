FROM nvidia/cuda
#   TODO REPLACE CUDA_ARCH_BIN with your GPU value: https://developer.nvidia.com/cuda-gpus
#   For example my GeForce GTX 1050 is 6.1
#   This is a dev image, needed to compile OpenCV with CUDA
#   Install  Gstreamer and OpenCV Pre-requisite libs
RUN apt-get update -y \
 && apt-get install --no-install-recommends libgstreamer1.0-0=1.22.0-2 gstreamer1.0-plugins-base=1.22.0-3 gstreamer1.0-plugins-good=1.22.0-4ubuntu1 gstreamer1.0-plugins-bad=1.22.0-3ubuntu2 gstreamer1.0-plugins-ugly=1.22.0-2 gstreamer1.0-libav=1.22.0-2 gstreamer1.0-doc gstreamer1.0-tools=1.22.0-2 libgstreamer1.0-dev=1.22.0-2 libgstreamer-plugins-base1.0-dev=1.22.0-3 -y
RUN apt-get update -y \
 && apt-get install --no-install-recommends pkg-config=1.8.1-1ubuntu2 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libwebp-dev=1.2.4-0.1build1 libtbb2 libtbb-dev=2021.8.0-1ubuntu2 libgtk2.0-dev=2.24.33-2ubuntu2 pkg-config=1.8.1-1ubuntu2 libavcodec-dev=7:5.1.2-3ubuntu1 libavformat-dev=7:5.1.2-3ubuntu1 libswscale-dev=7:5.1.2-3ubuntu1 cmake=3.25.1-1 -y
RUN apt-get install --no-install-recommends autoconf=2.71-3 autotools-dev=20220109.1 build-essential=12.9ubuntu3 gcc=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 -y
ENV OPENCV_RELEASE_TAG="3.4.5"
RUN git clone --depth 1 -b ${OPENCV_RELEASE_TAG} https://github.com/opencv/opencv.git /var/local/git/opencv
RUN cd /var/local/git/opencv
RUN mkdir -p /var/local/git/opencv/build \
 && cd /var/local/git/opencv/build $$ \
 && cmake -D CMAKE_BUILD_TYPE=Release -D BUILD_PNG=OFF -D BUILD_TIFF=OFF -D BUILD_TBB=OFF -D BUILD_JPEG=ON -D BUILD_JASPER=OFF -D BUILD_ZLIB=ON -D BUILD_EXAMPLES=OFF -D BUILD_opencv_java=OFF -D BUILD_opencv_python2=ON -D BUILD_opencv_python3=OFF -D ENABLE_NEON=OFF -D WITH_OPENCL=OFF -D WITH_OPENMP=OFF -D WITH_FFMPEG=OFF -D WITH_GSTREAMER=ON -D WITH_GSTREAMER_0_10=OFF -D WITH_CUDA=ON -D CUDA_TOOLKIT_ROOT_DIR=/usr/local/cuda/ -D WITH_GTK=ON -D WITH_VTK=OFF -D WITH_TBB=ON -D WITH_1394=OFF -D WITH_OPENEXR=OFF -D CUDA_ARCH_BIN=6.1 -D CUDA_ARCH_PTX="" -D INSTALL_C_EXAMPLES=OFF -D INSTALL_TESTS=OFF ..
RUN cd /var/local/git/opencv/build \
 && make install
#   Checkout and build darknet
#   for debug
#   apt-get install -y --no-install-recommends vim 
RUN git clone --depth 1 -b opendatacam https://github.com/opendatacam/darknet /var/local/darknet
WORKDIR /var/local/darknet
RUN sed -i -e s/GPU=0/GPU=1/ Makefile
#   For some reason no need for a CUDNN=1 on my CUDA_ARCH_BIN=6.1
RUN sed -i -e s/OPENCV=0/OPENCV=1/ Makefile
RUN make
RUN apt-get update ; apt-get install --no-install-recommends vim=2:9.0.1000-4ubuntu2 wget=1.21.3-1ubuntu1 -y
#   Get weights files
RUN wget https://pjreddie.com/media/files/yolov3.weights -O /var/local/darknet/yolov3.weights
RUN wget https://pjreddie.com/media/files/yolov3-tiny.weights -O /var/local/darknet/yolov3-tiny.weights
RUN mkdir /var/local/darknet/opendatacam_videos \
 && wget https://github.com/opendatacam/opendatacam/raw/v2.0.0-beta.2/static/demo/demo.mp4 -O /var/local/darknet/opendatacam_videos/demo.mp4
#   wget -N https://github.com/opendatacam/opendatacam/raw/v2.0.0-beta.2/static/demo/demo.mp4
#   Debug, test darknet : ./darknet detector demo cfg/coco.data cfg/yolov3.cfg yolov3.weights -ext_output -dont_show demo.mp4
#   Debug, test darknet : ./darknet detector demo cfg/coco.data cfg/yolov3-tiny.cfg yolov3-tiny.weights -ext_output -dont_show demo.mp4
#   Install node.js
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get install --no-install-recommends nodejs=18.13.0+dfsg1-1ubuntu2 -y
#   Install mongodb
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 9DA31620334BD75D9DCB49F368818C72E52529D4 \
 && echo "deb [ arch=amd64 ] https://repo.mongodb.org/apt/ubuntu xenial/mongodb-org/4.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-4.0.list
RUN apt-get update \
 && apt-get install --no-install-recommends openssl=3.0.8-1ubuntu1 libcurl3 mongodb-org -y
VOLUME ["/data/db"]
#   Technique to rebuild the docker file from here : https://stackoverflow.com/a/49831094/1228937
#   Build using date > marker && docker build .
#   date > marker && sudo docker build -t opendatacam .
COPY marker /dev/null
RUN git clone --depth 1 https://github.com/opendatacam/opendatacam /var/local/opendatacam
WORKDIR /var/local/opendatacam
RUN sed -i -e s+/darknet+/var/local/darknet+ config.json
RUN sed -i -e s+TO_REPLACE_VIDEO_INPUT+file+ config.json
RUN sed -i -e s+TO_REPLACE_NEURAL_NETWORK+yolov3+ config.json
#   Build
RUN npm install
RUN npm run build
EXPOSE 8080/tcp 8070/tcp 8090/tcp 27017/tcp
RUN wget https://raw.githubusercontent.com/opendatacam/opendatacam/v2/docker/run-jetson/docker-start-mongo-and-opendatacam.sh
RUN chmod 777 docker-start-mongo-and-opendatacam.sh
CMD ./docker-start-mongo-and-opendatacam.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
