FROM ubuntu:16.04
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 libatlas-base-dev=3.10.2-9 libboost-all-dev=1.58.0.1ubuntu1 libgflags-dev=2.1.2-3 libgoogle-glog-dev=0.3.4-0.1 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libleveldb-dev=1.18-5 liblmdb-dev=0.9.17-3 libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 libprotobuf-dev=2.6.1-1.3 libsnappy-dev=1.1.3-2 protobuf-compiler=2.6.1-1.3 python-dev=2.7.12-1~16.04 python-numpy=1:1.11.0-1ubuntu1 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 python-scipy=0.17.0-1 -y \
 && rm -rf /var/lib/apt/lists/*
ENV CAFFE_ROOT="/opt/caffe"
WORKDIR $CAFFE_ROOT
#   FIXME: use ARG instead of ENV once DockerHub supports this
ENV CLONE_TAG="master"
RUN git clone -b ${CLONE_TAG} --depth 1 https://github.com/jaketaylorpro/caffe.git . \
 && pip install pip==23.1 --upgrade \
 && cd python \
 && for req in $( cat requirements.txt ;) pydot; do pip install $req ; done \
 && cd .. \
 && mkdir build \
 && cd build \
 && cmake -DCPU_ONLY=1 .. \
 && make -j"$( nproc ;)"
RUN /opt/caffe/scripts/download_model_binary.py models/bvlc_reference_caffenet
RUN /opt/caffe/data/ilsvrc12/get_ilsvrc_aux.sh
ENV PYCAFFE_ROOT="$CAFFE_ROOT/python"
ENV PYTHONPATH="$PYCAFFE_ROOT:$PYTHONPATH"
ENV PATH="$CAFFE_ROOT/build/tools:$PYCAFFE_ROOT:$PATH"
RUN echo "$CAFFE_ROOT/build/lib" >> /etc/ld.so.conf.d/caffe.conf \
 && ldconfig
ENTRYPOINT ["./build/examples/cpp_classification/classification.bin", "models/bvlc_reference_caffenet/deploy.prototxt", "models/bvlc_reference_caffenet/bvlc_reference_caffenet.caffemodel", "data/ilsvrc12/imagenet_mean.binaryproto", "data/ilsvrc12/synset_words.txt", "/envoyai/input/image.jpg"]
LABEL com.envoyai.metadata-version="2"
LABEL com.envoyai.nvidia="false"
LABEL com.envoyai.info="name: Caffe Example C++ Classification\ntitle: Caffe C++ Classification Example adapted for the EnvoyAI platform.\nauthor: Caffe adapted by EnvoyAI Staff\norganization: Caffe and EnvoyAI\ndata-source: ilsvrc training set\nalgorithm: Neural Net\nlinks: you can download a frog picture <a href='http://zso3597.cias.rit.edu/256/week4/project1/images/frog.jpg'>here</a>"
LABEL com.envoyai.schema-in="image.jpg:\n title: Input Image\n mime-type: image/jpeg"
LABEL com.envoyai.schema-out="guess0:\n title: First Guess\n type: string \nconf0:\n title: First Guess Confidence\n type: percentage\nguess1:\n title: Second Guess\n type: string \nconf1:\n title: Second Guess Confidence\n type: percentage\nguess3:\n title: Third Guess\n type: string \nconf3:\n title: Third Guess Confidence\n type: percentage"
LABEL com.envoyai.display="source-display-group:\n display-elements:\n - title: Input Image\n id: input-image\n content:\n pointer:\n source: input\n property: image.jpg\nresults-display-group:\n presentation: tile\n display-elements:\n - title: First Guess\n id: guess-0\n content:\n pointer:\n source: output\n property: guess0\n associated-values:\n - value:\n pointer:\n source: output\n property: conf0\n unit: percentage\n label: Confidence\n - title: Second Guess\n id: guess-1\n content:\n pointer:\n source: output\n property: guess1\n associated-values:\n - value:\n pointer:\n source: output\n property: conf1\n unit: percentage\n label: Confidence\n - title: Third Guess\n id: guess-2\n content:\n pointer:\n source: output\n property: guess2\n associated-values:\n - value:\n pointer:\n source: output\n property: conf2\n unit: percentage\n label: Confidence"
LABEL com.envoyai.selector="selector-type: choose-one-display-element\nselector-config:\n choose-options:\n - guess-0\n - guess-1\n - guess-2\n default-option: guess-0"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
