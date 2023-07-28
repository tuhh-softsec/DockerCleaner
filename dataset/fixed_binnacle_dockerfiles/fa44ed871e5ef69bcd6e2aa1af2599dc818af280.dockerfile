FROM alpine:3.7
ENV JAVA_HOME="/usr/lib/jvm/default-jvm"
ENV BAZEL_VERSION="0.5.4"
WORKDIR /tmp
#   Build directory trees
RUN mkdir -p /opt
#   Build and install Tensorflow/SyntaxNet
RUN set -x \
 && apk add bash=4.4.19-r1 build-base=0.5-r0 linux-headers=4.4.6-r2 python openjdk8=8.275.01-r0 wget=1.20.3-r0 zip=3.0-r4 --no-cache --update --virtual .builddeps.bazel \
 && mkdir /tmp/bazel \
 && wget -q -O /tmp/bazel-dist.zip https://github.com/bazelbuild/bazel/releases/download/${BAZEL_VERSION}/bazel-${BAZEL_VERSION}-dist.zip \
 && unzip -q -d /tmp/bazel /tmp/bazel-dist.zip \
 && cd /tmp/bazel \
 && sed -i -e '/"-std=c++0x"/{h;s//"-fpermissive"/;x;G}' tools/cpp/cc_configure.bzl \
 && sed -i -e '/#endif \/\/ COMPILER_MSVC/{h;s//#else/;G;s//#include <sys\/stat.h>/;G;}' third_party/ijar/common.h \
 && export EXTRA_BAZEL_ARGS="--jobs $( grep -c ^processor /proc/cpuinfo ;)" \
 && bash compile.sh \
 && cp output/bazel /usr/local/bin/ \
 && cd /tmp \
 && apk add python py-pip jemalloc=5.0.1-r0 libc6-compat=1.1.18-r4 --no-cache --update \
 && apk add hdf5 --no-cache --repository http://dl-cdn.alpinelinux.org/alpine/edge/testing/ \
 && apk add hdf5-dev --no-cache --virtual .builddeps.edge --repository http://dl-cdn.alpinelinux.org/alpine/edge/testing/ \
 && apk add git=2.15.4-r0 bash=4.4.19-r1 patch=2.7.6-r0 perl=5.26.3-r0 sed=4.4-r1 swig=3.0.12-r1 graphviz=2.40.1-r0 python-dev graphviz-dev=2.40.1-r0 --no-cache --virtual .builddeps.tensorflow \
 && pip install wheel==0.40.0 mock==5.0.2 asciitree==0.3.3 numpy==1.24.2 h5py==3.8.0 autograd==1.1.13 protobuf==3.3.0 pygraphviz==1.10 backports.weakref==1.0.post1 -U \
 && git clone --recursive https://github.com/tensorflow/models.git \
 && cd /tmp/models/research/syntaxnet/tensorflow \
 && echo | CC_OPT_FLAGS=-march=native PYTHON_BIN_PATH=$( which python ;) TF_NEED_MKL=0 TF_NEED_VERBS=0 TF_NEED_CUDA=0 TF_NEED_GCP=0 TF_NEED_JEMALLOC=0 TF_NEED_HDFS=0 TF_NEED_OPENCL=0 TF_ENABLE_XLA=0 ./configure \
 && cd .. \
 && CPU_COUNT=$( grep -c ^processor /proc/cpuinfo ;) \
 && bazel test --local_resources 4096,$CPU_COUNT,1.0 ... \
 && mkdir /tmp/syntaxnet_pkg \
 && bazel-bin/dragnn/tools/build_pip_package --output-dir=/tmp/syntaxnet_pkg --include-tensorflow \
 && pip install /tmp/syntaxnet_pkg/* \
 && cd /opt \
 && apk del .builddeps.bazel .builddeps.edge .builddeps.tensorflow \
 && rm -rf /tmp/* \
 && rm -rf /root/.cache/*
#   Build directory trees for models and dragnn wrapper
RUN set -x \
 && mkdir -p /usr/local/tfmodels \
 && mkdir -p /usr/lib/python2.7/site-packages/dragnn/wrapper
COPY wrapper.py /usr/lib/python2.7/site-packages/dragnn/wrapper/__init__.py
WORKDIR /opt
CMD ["/bin/sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
