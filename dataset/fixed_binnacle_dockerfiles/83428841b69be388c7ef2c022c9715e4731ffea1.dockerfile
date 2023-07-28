#   Copyright 2018 The SAF Authors. All Rights Reserved.
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#   Dokerfile for building SAF with OpenVINO (CVSDK)
#
#     Usage: sudo docker build -f Dockerfile . -t saf \
#                              --build-arg USE_VDMS=ON \
#                              --build-arg GITHUB_TOKEN=<OAuth-string> \
#                              --build-arg GITHUB_URL=<GitHub-URL>
#                              --build-arg SAF_HASH=<commit-id>
#
#     When building under within VPN, add the following to your build command:
#       --build-arg http_proxy=<protocol>://<hostname>:<port>
#       --build-arg https_proxy=<protocol>://<hostname>:<port>
#
#     To run, use `docker run -it saf`
#              or `sudo docker run -it --device /dev/dri:/dev/dri saf`
#
#     Prebuilt Docker images are available upon requests.
FROM ubuntu:16.04
MAINTAINER Shao-Wen Yang <shao-wen.yang@intel.com>
ENV DEBIAN_FRONTEND="noninteractive"
ARG DEFAULT_WORKDIR=/vcs
ARG USE_VDMS=OFF
ARG GITHUB_URL=github.com/viscloud/saf.git
ARG GITHUB_TOKEN
ARG SAF_HASH
#   Prepare toolchain
RUN apt-get update \
 && apt-get install --no-install-recommends dialog=1.3-20160209-1 apt-utils=1.2.35 --yes
RUN apt-get install --no-install-recommends build-essential=12.1ubuntu2 --yes
RUN apt-get install --no-install-recommends pkg-config=0.29.1-0ubuntu1 --yes
RUN apt-get install --no-install-recommends g++=4:5.3.1-1ubuntu1 wget=1.17.1-1ubuntu1.5 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 vim=2:7.4.1689-3ubuntu1.5 --yes
RUN apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 --yes
RUN apt-get install --no-install-recommends autoconf=2.69-9 automake=1:1.15-4ubuntu1 libtool=2.4.6-0.1 --yes
RUN apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 unzip=6.0-20ubuntu1.1 --yes
RUN apt-get install --no-install-recommends libavcodec-dev:amd64 libavformat-dev:amd64 libswscale-dev:amd64 libtbb2:amd64 libtbb-dev:amd64 libjpeg-dev:amd64 libpng12-dev:amd64 libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libdc1394-22-dev:amd64 --yes
RUN apt-get install --no-install-recommends libeigen3-dev=3.3~beta1-2 --yes
RUN apt-get install --no-install-recommends libboost-all-dev=1.58.0.1ubuntu1 --yes
RUN apt-get install --no-install-recommends libgstreamer1.0-dev=1.8.3-1~ubuntu0.1 libgstreamer-plugins-base1.0-dev=1.8.3-1ubuntu0.3 libgstreamer-plugins-good1.0-dev=1.8.3-1ubuntu0.5 libgstreamer-plugins-bad1.0-dev=1.8.3-1ubuntu0.2 gstreamer1.0:amd64 --yes
RUN apt-get install --no-install-recommends python-scipy=0.17.0-1 python-numpy=1:1.11.0-1ubuntu1 python-yaml=3.11-3build1 --yes
RUN apt-get install --no-install-recommends libleveldb-dev:amd64 libsnappy-dev:amd64 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libgflags-dev=2.1.2-3 libgoogle-glog-dev=0.3.4-0.1 liblmdb-dev:amd64 libjemalloc-dev=3.6.0-9ubuntu1 libzmq3-dev:amd64 --yes
RUN apt-get install --no-install-recommends libblas-dev=3.6.0-2ubuntu2 --yes
RUN apt-get install --no-install-recommends libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 --yes
RUN apt-get install --no-install-recommends libcpprest-dev=2.8.0-2 --yes
RUN apt-get install --no-install-recommends libopenblas-dev=0.2.18-1ubuntu1 --yes
RUN apt-get install --no-install-recommends alien=8.95 clinfo=2.1.16.01.12-1 opencl-headers=2.0~svn32091-2 --yes
RUN apt-get install --no-install-recommends libjsoncpp-dev=1.7.2-1 --yes
#   Prepare the environment
RUN mkdir -p $DEFAULT_WORKDIR
ENV PATH="${PATH}:/usr/local/bin"
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"
#   OpenVINO (CVSDK)
WORKDIR $DEFAULT_WORKDIR
RUN wget http://registrationcenter-download.intel.com/akdlm/irc_nas/13131/l_openvino_toolkit_p_2018.1.265.tgz
RUN tar zxvf l_openvino_toolkit_p_2018.1.265.tgz \
 && cd l_openvino_toolkit_p_2018.1.265 \
 && sed -i 's/ACCEPT_EULA=decline/ACCEPT_EULA=accept/g' silent.cfg \
 && ./install.sh -s silent.cfg
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/opt/intel/opencl"
RUN apt-get install --no-install-recommends lsb-release=9.20160110ubuntu0.2 --yes
RUN /bin/bash -c "source /opt/intel/computer_vision_sdk/bin/setupvars.sh"
ENV PATH="/opt/intel/computer_vision_sdk/deployment_tools/model_optimizer:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/lib:/opt/intel/computer_vision_sdk/opencv/share/OpenCV/3rdparty/lib:/opt/intel/computer_vision_sdk/opencv/lib:/opt/intel/opencl:/opt/intel/computer_vision_sdk/deployment_tools/inference_engine/external/cldnn/lib:/opt/intel/computer_vision_sdk/deployment_tools/inference_engine/external/mkltiny_lnx/lib:/opt/intel/computer_vision_sdk/deployment_tools/inference_engine/lib/ubuntu_16.04/intel64:/opt/intel/computer_vision_sdk/deployment_tools/model_optimizer/model_optimizer_caffe/bin:/opt/intel/computer_vision_sdk/openvx/lib::/opt/intel/opencl"
ENV PYTHONPATH="/opt/intel/computer_vision_sdk/deployment_tools/model_optimizer:${PYTHONPATH}"
ENV OpenCV_DIR="/opt/intel/computer_vision_sdk/opencv/share/OpenCV"
RUN cd /opt/intel/computer_vision_sdk/install_dependencies \
 && ./install_NEO_OCL_driver.sh
RUN cd /opt/intel/computer_vision_sdk/deployment_tools/inference_engine/samples \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make cpu_extension -j4
WORKDIR $DEFAULT_WORKDIR
RUN rm -rf l_openvino_toolkit_p_2018.1.265 l_openvino_toolkit_p_2018.1.265.tgz
#   Protobuf
WORKDIR $DEFAULT_WORKDIR
RUN git clone https://github.com/google/protobuf.git
RUN cd protobuf \
 && git reset --hard 072431452a365450c607e9503f51786be44ecf7f \
 && ./autogen.sh \
 && ./configure --disable-shared --with-pic \
 && make -j4 \
 && make install \
 && cd .. \
 && rm -rf protobuf
#   VDMS
WORKDIR $DEFAULT_WORKDIR
RUN if [ ON = "$USE_VDMS" ] ; then apt-get install --no-install-recommends scons=2.4.1-1 flex=2.6.0-11 --yes \
 && apt-get install --no-install-recommends javacc=5.0-5 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 --yes \
 && apt-get install --no-install-recommends bison=2:3.0.4.dfsg-1 libbison-dev=2:3.0.4.dfsg-1 --yes \
 && apt-get install --no-install-recommends zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 --yes \
 && apt-get install --no-install-recommends libbz2-dev=1.0.6-8ubuntu0.2 --yes \
 && apt-get install --no-install-recommends libssl-dev=1.0.2g-1ubuntu4.20 --yes \
 && apt-get install --no-install-recommends liblz4-dev=0.0~r131-2ubuntu2 --yes \
 && apt-get install --no-install-recommends mpich=3.2-6build1 --yes \
 && apt-get install --no-install-recommends libopenmpi-dev=1.10.2-8ubuntu1 --yes \
 && apt-get install --no-install-recommends libgtest-dev=1.7.0-4ubuntu1 ed=1.10-2 --yes \
 && apt-get install --no-install-recommends libtbb2=4.4~20151115-0ubuntu3 libtbb-dev=4.4~20151115-0ubuntu3 --yes \
 && apt-get install --no-install-recommends libdc1394-22-dev=2.2.4-1 --yes \
 && git clone https://github.com/Blosc/c-blosc.git \
 && cd $DEFAULT_WORKDIR/c-blosc \
 && mkdir build \
 && cd build \
 && cmake .. \
 && cmake --build . \
 && ctest \
 && cmake --build . --target install \
 && cd ../.. \
 && rm -rf c-blosc \
 && cd $DEFAULT_WORKDIR \
 && wget https://github.com/facebook/zstd/archive/v1.1.0.tar.gz \
 && tar xf v1.1.0.tar.gz \
 && cd zstd-1.1.0 \
 && make -j4 \
 && make install \
 && cd .. \
 && rm -f v1.1.0.tar.gz \
 && rm -rf zstd-1.1.0 \
 && cd /usr/src/gtest \
 && cmake . \
 && make -j4 \
 && mv libgtest* /usr/local/lib/ \
 && cd $DEFAULT_WORKDIR \
 && wget https://github.com/TileDB-Inc/TileDB/archive/0.6.1.tar.gz \
 && tar xf 0.6.1.tar.gz \
 && cd TileDB-0.6.1 \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j4 \
 && make install \
 && cd ../.. \
 && rm -f 0.6.1.tar.gz \
 && rm -rf TileDB-0.6.1 \
 && git clone https://github.com/tristanpenman/valijson.git \
 && cd $DEFAULT_WORKDIR/valijson \
 && cp -r include/* /usr/local/include \
 && cd .. \
 && rm -rf valijson \
 && cd $DEFAULT_WORKDIR \
 && wget https://github.com/intellabs/vcl/archive/v0.1.0.tar.gz \
 && tar xf v0.1.0.tar.gz \
 && mv vcl-0.1.0 vcl \
 && cd vcl \
 && sed -i "s/\(CPPPATH\s*=\s*\[.*\)\(\]\)/\1,\'\/opt\/intel\/computer_vision_sdk\/opencv\/include\'\2/g" SConstruct \
 && sed -i "s/\(LIBPATH\s*=\s*\[.*\)\(\]\)/\1,\'\/opt\/intel\/computer_vision_sdk\/opencv\/lib\'\2/g" SConstruct \
 && scons -j4 \
 && cd .. \
 && rm -f v0.1.0.tar.gz \
 && cd $DEFAULT_WORKDIR \
 && wget https://github.com/intellabs/pmgd/archive/v1.0.0.tar.gz \
 && tar xf v1.0.0.tar.gz \
 && mv pmgd-1.0.0 pmgd \
 && cd pmgd \
 && make -j4 \
 && cd .. \
 && rm -f v1.0.0.tar.gz \
 && cd $DEFAULT_WORKDIR \
 && wget https://github.com/intellabs/vdms/archive/v1.0.0.tar.gz \
 && tar xf v1.0.0.tar.gz \
 && mv vdms-1.0.0 vdms \
 && cd vdms \
 && sed -i "s/CPPPATH\s*.*\[/&\'\/opt\/intel\/computer_vision_sdk\/opencv\/include\',/g" SConstruct \
 && sed -i "s/LIBPATH\s*.*\[/&\'\/opt\/intel\/computer_vision_sdk\/opencv\/lib\',/g" SConstruct \
 && sed -i "s/LIBS\s*.*\[/&\'opencv_core\',\'opencv_imgproc\',\'opencv_imgcodecs\',/g" SConstruct \
 && mkdir db \
 && scons -j4 INTEL_PATH=$DEFAULT_WORKDIR \
 && cd .. \
 && rm -f v1.0.0.tar.gz ; fi
#   SAF
WORKDIR $DEFAULT_WORKDIR
ENV no_proxy="\"github.intel.com localhost\""
RUN if [ ! -z "$GITHUB_TOKEN" ] ; then git clone https://$GITHUB_TOKEN@$GITHUB_URL saf ; else git clone https://$GITHUB_URL saf ; fi
RUN cd saf \
 && if [ ! -z "$SAF_HASH" ] ; then git reset --hard $SAF_HASH ; fi \
 && if [ ON = "$USE_VDMS" ] ; then mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DUSE_CVSDK=ON -DBACKEND=cpu -DBUILD_TESTS=OFF -DUSE_SSD=ON -DUSE_WEBSOCKET=ON -DUSE_KAFKA=ON -DUSE_MQTT=ON -DUSE_PYTHON=ON -DUSE_VDMS=ON -DVDMS_HOME=$DEFAULT_WORKDIR/vdms .. ; else mkdir build \
 && cd build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DUSE_CVSDK=ON -DBACKEND=cpu -DBUILD_TESTS=OFF -DUSE_SSD=ON -DUSE_WEBSOCKET=ON -DUSE_KAFKA=ON -DUSE_MQTT=ON -DUSE_PYTHON=ON .. ; fi \
 && make -j4 \
 && make apps -j4 \
 && make install_python
#   Set up SAF 
WORKDIR $DEFAULT_WORKDIR/saf/config
RUN cp config.toml.example config.toml \
 && cp cameras.toml.example cameras.toml \
 && cp models.toml.example models.toml \
 && echo "\n[[model]]\nname = \"person-detection-retail-0012\"\ntype = \"cvsdk\"\ndesc_path = \"/opt/intel/computer_vision_sdk/deployment_tools/intel_models/person-detection-retail-0012/FP32/person-detection-retail-0012.xml\"\nparams_path = \"/opt/intel/computer_vision_sdk/deployment_tools/intel_models/person-detection-retail-0012/FP32/person-detection-retail-0012.bin\"\ninput_width = 96\ninput_height = 112\nlabel_file = \"/opt/intel/computer_vision_sdk/deployment_tools/intel_models/person-detection-retail-0012/label.names\"" >> models.toml \
 && echo "person" >> /opt/intel/computer_vision_sdk/deployment_tools/intel_models/person-detection-retail-0012/label.names
#   Prepare sample scripts
WORKDIR $DEFAULT_WORKDIR/saf
RUN echo "#!/bin/sh\nbuild/apps/simple --camera GST_TEST" > run_simple.sh \
 && chmod +x run_simple.sh
RUN echo "#!/bin/sh\nCAMERA=GST_TEST\nHOST=localhost\nif [ -n \"$1\" ]; then CAMERA=$1; fi\nif [ -n \"$2\" ]; then HOST=$2; fi\nbuild/apps/detector -c $CAMERA --detector_type cvsdk-ssd -m person-detection-retail-0012 --detector_targets person --detector_confidence_threshold 0.3 --sender_package_type frame --sender_endpoint \"kafka://$HOST:9092\"" > run_detector.sh \
 && chmod +x run_detector.sh
RUN echo "#!/bin/sh\nCAMERA=GST_TEST\nHOST=localhost\nif [ -n \"$1\" ]; then CAMERA=$1; fi\nif [ -n \"$2\" ]; then HOST=$2; fi\nbuild/apps/tracker -c $CAMERA --detector_type cvsdk-ssd -m person-detection-retail-0012 --detector_targets person --detector_confidence_threshold 0.3 --detector_idle_duration 0.1 --tracker_type kf --sender_package_type frame --sender_endpoint \"kafka://$HOST:9092\"" > run_tracker.sh \
 && chmod +x run_tracker.sh
RUN echo "#!/bin/sh\nCAMERA=GST_TEST\nHOST=localhost\nif [ -n \"$1\" ]; then CAMERA=$1; fi\nif [ -n \"$2\" ]; then HOST=$2; fi\nbuild/apps/visualizer --sender_endpoint \"kafka://$HOST:9092\" -c $CAMERA" > run_visualizer.sh \
 && chmod +x run_visualizer.sh
RUN echo "#!/bin/sh\nCAMERA=GST_TEST\nHOST=localhost\nif [ -n \"$1\" ]; then CAMERA=$1; fi\nif [ -n \"$2\" ]; then HOST=$2; fi\nbuild/apps/writer --sender_endpoint \"kafka://$HOST:9092\" --write_target file --write_uri sample.csv -c $CAMERA" > run_writer.sh \
 && chmod +x run_writer.sh
#   Install OpenCV-Python for better Python support
RUN apt-get install --no-install-recommends python-pip=8.1.1-2ubuntu0.6 --yes \
 && pip install opencv-python==4.7.0.72
#   Resolve ProtoBuf version conflict
#   In this script, ProtoBuf3 is linked against statically.
#   The following removes ProtoBuf3 shared libraries.
#   Do NOT EVER do the following on your native setup.
RUN rm -f /usr/lib/x86_64-linux-gnu/libprotobuf*
#   Clean up
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
WORKDIR $DEFAULT_WORKDIR/saf
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
