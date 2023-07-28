FROM ubuntu:16.04
#   Install system packages
RUN apt-get update -qq \
 && apt-get install --no-install-recommends python3=3.5.1-3 python3-dev=3.5.1-3 python-pil=3.1.2-0ubuntu1.6 python-lxml=3.5.0-1ubuntu0.4 python-tk=2.7.12-1~16.04 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 pkg-config=0.29.1-0ubuntu1 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libswscale-dev=7:2.8.17-0ubuntu0.1 libtbb2=4.4~20151115-0ubuntu3 libtbb-dev=4.4~20151115-0ubuntu3 libjpeg-dev=8c-2ubuntu8 libpng-dev libtiff-dev libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libdc1394-22-dev=2.2.4-1 x11-apps=7.7+5+nmu1ubuntu1 wget=1.17.1-1ubuntu1.5 vim=2:7.4.1689-3ubuntu1.5 ffmpeg=7:2.8.17-0ubuntu0.1 unzip=6.0-20ubuntu1.1 libusb-1.0-0-dev=2:1.0.20-1 python3-setuptools=20.7.0-1 python3-numpy=1:1.11.0-1ubuntu1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libgoogle-glog-dev=0.3.4-0.1 swig=3.0.8-0ubuntu3 libunwind-dev=1.1-4.1 libc++-dev=3.7.0-1ubuntu0.1 libc++abi-dev=3.7.0-1ubuntu0.1 build-essential=12.1ubuntu2 -qq -y \
 && rm -rf /var/lib/apt/lists/*
#   Install core packages 
RUN wget -q -O /tmp/get-pip.py --no-check-certificate https://bootstrap.pypa.io/get-pip.py \
 && python3 /tmp/get-pip.py
RUN pip install pip==23.1 numpy==1.24.2 pillow==9.5.0 matplotlib==3.7.1 notebook==6.5.4 Flask==2.2.3 imutils==0.5.4 paho-mqtt==1.6.1 PyYAML==6.0 -U
#   Install tensorflow models object detection
RUN GIT_SSL_NO_VERIFY=true git clone -q https://github.com/tensorflow/models /usr/local/lib/python3.5/dist-packages/tensorflow/models
RUN wget -q -P /usr/local/src/ --no-check-certificate https://github.com/google/protobuf/releases/download/v3.5.1/protobuf-python-3.5.1.tar.gz
#   Download & build protobuf-python
RUN cd /usr/local/src/ \
 && tar xf protobuf-python-3.5.1.tar.gz \
 && rm protobuf-python-3.5.1.tar.gz \
 && cd /usr/local/src/protobuf-3.5.1/ \
 && ./configure \
 && make \
 && make install \
 && ldconfig \
 && rm -rf /usr/local/src/protobuf-3.5.1/
#   Download & build OpenCV
RUN wget -q -P /usr/local/src/ --no-check-certificate https://github.com/opencv/opencv/archive/4.0.1.zip
RUN cd /usr/local/src/ \
 && unzip 4.0.1.zip \
 && rm 4.0.1.zip \
 && cd /usr/local/src/opencv-4.0.1/ \
 && mkdir build \
 && cd /usr/local/src/opencv-4.0.1/build \
 && cmake -D CMAKE_INSTALL_TYPE=Release -D CMAKE_INSTALL_PREFIX=/usr/local/ .. \
 && make -j4 \
 && make install \
 && rm -rf /usr/local/src/opencv-4.0.1
#   Download and install EdgeTPU libraries
RUN wget -q -O edgetpu_api.tar.gz --no-check-certificate http://storage.googleapis.com/cloud-iot-edge-pretrained-models/edgetpu_api.tar.gz
RUN tar xzf edgetpu_api.tar.gz \
 && cd python-tflite-source \
 && cp -p libedgetpu/libedgetpu_x86_64.so /lib/x86_64-linux-gnu/libedgetpu.so \
 && cp edgetpu/swig/compiled_so/_edgetpu_cpp_wrapper_x86_64.so edgetpu/swig/_edgetpu_cpp_wrapper.so \
 && cp edgetpu/swig/compiled_so/edgetpu_cpp_wrapper.py edgetpu/swig/ \
 && python3 setup.py develop --user
#   Minimize image size 
RUN (apt-get autoremove -y ;apt-get autoclean -y )
#   symlink the model and labels
RUN ln -s /python-tflite-source/edgetpu/test_data/mobilenet_ssd_v2_coco_quant_postprocess_edgetpu.tflite /frozen_inference_graph.pb
RUN ln -s /python-tflite-source/edgetpu/test_data/coco_labels.txt /label_map.pbtext
#   Set TF object detection available
ENV PYTHONPATH="\"$PYTHONPATH:/usr/local/lib/python3.5/dist-packages/tensorflow/models/research:/usr/local/lib/python3.5/dist-packages/tensorflow/models/research/slim\""
RUN cd /usr/local/lib/python3.5/dist-packages/tensorflow/models/research \
 && protoc object_detection/protos/*.proto --python_out=.
WORKDIR /opt/frigate/
COPY frigate frigate/
COPY detect_objects.py .
CMD ["python3", "-u", "detect_objects.py"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
