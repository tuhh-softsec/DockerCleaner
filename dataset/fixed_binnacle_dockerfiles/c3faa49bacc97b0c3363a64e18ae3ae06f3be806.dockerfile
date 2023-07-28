FROM ubuntu:16.04
RUN echo 'building CPU DeepDetect image'
MAINTAINER Emmanuel Benazera "beniz@droidnik.fr"
LABEL description="DeepDetect deep learning server & API / CPU version"
RUN ln -sf /dev/stdout /var/log/deepdetect.log
RUN ln -sf /dev/stderr /var/log/deepdetect.log
RUN useradd -ms /bin/bash dd
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 cmake=3.5.1-1ubuntu3 automake=1:1.15-4ubuntu1 build-essential=12.1ubuntu2 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 pkg-config=0.29.1-0ubuntu1 zip=3.0-11 g++=4:5.3.1-1ubuntu1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libgoogle-glog-dev=0.3.4-0.1 libgflags-dev=2.1.2-3 libeigen3-dev=3.3~beta1-2 libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 libcppnetlib-dev=0.11.2+dfsg1-2 libboost-dev=1.58.0.1ubuntu1 libboost-iostreams-dev=1.58.0.1ubuntu1 libcurlpp-dev=0.7.3-6 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 protobuf-compiler=2.6.1-1.3 libopenblas-dev=0.2.18-1ubuntu1 libhdf5-dev=1.8.16+docs-4ubuntu1.1 libprotobuf-dev=2.6.1-1.3 libleveldb-dev=1.18-5 libsnappy-dev=1.1.3-2 liblmdb-dev=0.9.17-3 libutfcpp-dev=2.3.4-1 wget=1.17.1-1ubuntu1.5 autoconf=2.69-9 libtool-bin=2.4.6-0.1 python-numpy=1:1.11.0-1ubuntu1 swig=3.0.8-0ubuntu3 python-dev=2.7.12-1~16.04 python-setuptools=20.7.0-1 python-wheel=0.29.0-1 unzip=6.0-20ubuntu1.1 libgoogle-perftools-dev=2.4-0ubuntu5.16.04.1 screen=4.3.1-2ubuntu0.1 vim=2:7.4.1689-3ubuntu1.5 strace=4.11-1ubuntu3 curl=7.47.0-1ubuntu2.19 libarchive-dev=3.1.2-11ubuntu0.16.04.8 bash-completion=1:2.1-4.2ubuntu1.1 libspdlog-dev=1.6-1 ca-certificates=20210119~16.04.1 -y \
 && wget -O /tmp/bazel.deb https://github.com/bazelbuild/bazel/releases/download/0.8.1/bazel_0.8.1-linux-x86_64.deb \
 && dpkg -i /tmp/bazel.deb \
 && apt-get remove -y libcurlpp0 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
WORKDIR /opt
RUN wget -O /opt/curlpp-v0.8.1.zip https://github.com/jpbarrette/curlpp/archive/v0.8.1.zip \
 && unzip curlpp-v0.8.1.zip \
 && cd curlpp-0.8.1 \
 && cmake . \
 && make install \
 && cp /usr/local/lib/libcurlpp.* /usr/lib/ \
 && cd /opt \
 && rm -rf curlpp-0.8.1 curlpp-v0.8.1.zip
RUN git clone https://github.com/beniz/deepdetect.git \
 && cd deepdetect \
 && mkdir build
WORKDIR /opt/deepdetect/build
RUN cmake .. -DUSE_TF=ON -DUSE_TF_CPU_ONLY=ON -DUSE_SIMSEARCH=ON -DUSE_TSNE=ON -DUSE_NCNN=ON \
 && make
#   external volume to be mapped, e.g. for models or training data
RUN mkdir /data
VOLUME ["/data"]
#   include a few image models within the image
RUN mkdir /opt/models
WORKDIR /opt/models
RUN mkdir ggnet \
 && cd ggnet \
 && wget http://www.deepdetect.com/models/ggnet/bvlc_googlenet.caffemodel
RUN mkdir resnet_50 \
 && cd resnet_50 \
 && wget http://www.deepdetect.com/models/resnet/ResNet-50-model.caffemodel \
 && wget http://www.deepdetect.com/models/resnet/ResNet_mean.binaryproto
RUN mv /opt/models/resnet_50/ResNet_mean.binaryproto /opt/models/resnet_50/mean.binaryproto
RUN cp /opt/deepdetect/datasets/imagenet/corresp_ilsvrc12.txt /opt/models/ggnet/corresp.txt
RUN cp /opt/deepdetect/datasets/imagenet/corresp_ilsvrc12.txt /opt/models/resnet_50/corresp.txt
RUN cp /opt/deepdetect/templates/caffe/googlenet/*prototxt /opt/models/ggnet/
RUN cp /opt/deepdetect/templates/caffe/resnet_50/*prototxt /opt/models/resnet_50/
#   permissions
RUN chown -R dd:dd /opt/deepdetect
RUN chown -R dd:dd /opt/models
USER dd
WORKDIR /opt/deepdetect/build/main
CMD ./dede -host 0.0.0.0
EXPOSE 8080/tcp
# Please add your HEALTHCHECK here!!!
