FROM ubuntu:18.04
MAINTAINER Remi Cresson <remi.cresson[at]irstea[dot]fr>
RUN apt-get update -y \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 make=4.1-9.1ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 g++=4:7.4.0-1ubuntu2.3 gcc=4:7.4.0-1ubuntu2.3 git=1:2.17.1-1ubuntu0.17 libtool=2.4.6-2 swig=3.0.12-1 xvfb=2:1.19.6-1ubuntu4.14 wget=1.19.4-1ubuntu2.2 autoconf=2.69-11 automake=1:1.15.1-3ubuntu2 pkg-config=0.29.1-0ubuntu2 zip=3.0-11build1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 unzip=6.0-21ubuntu1.2 -y \
 && rm -rf /var/lib/apt/lists/*
#   ----------------------------------------------------------------------------
#   OTB and TensorFlow dependencies
#   ----------------------------------------------------------------------------
RUN apt-get update -y \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends freeglut3-dev=2.8.1-3 libboost-date-time-dev=1.65.1.0ubuntu1 libboost-filesystem-dev=1.65.1.0ubuntu1 libboost-graph-dev=1.65.1.0ubuntu1 libboost-program-options-dev=1.65.1.0ubuntu1 libboost-system-dev=1.65.1.0ubuntu1 libboost-thread-dev=1.65.1.0ubuntu1 libcurl4-gnutls-dev=7.58.0-2ubuntu3.24 libexpat1-dev=2.2.5-3ubuntu0.9 libfftw3-dev=3.3.7-1 libgdal-dev=2.2.3+dfsg-2 libgeotiff-dev=1.4.2-2build1 libglew-dev=2.0.0-5 libglfw3-dev=3.2.1-1 libgsl-dev=2.4+dfsg-6 libinsighttoolkit4-dev=4.12.2-dfsg1-1ubuntu1 libkml-dev=1.3.0-5 libmuparser-dev=2.2.3-6 libmuparserx-dev=4.0.7+dfsg-3 libopencv-core-dev=3.2.0+dfsg-4ubuntu0.1 libopencv-ml-dev=3.2.0+dfsg-4ubuntu0.1 libopenthreads-dev=3.2.3+dfsg1-2ubuntu8 libossim-dev=2.2.2-1 libpng-dev=1.6.34-1ubuntu0.18.04.2 libqt5opengl5-dev=5.9.5+dfsg-0ubuntu2.6 libqwt-qt5-dev=6.1.3-1 libsvm-dev=3.21+ds-1.1 libtinyxml-dev=2.6.2-4 qtbase5-dev=5.9.5+dfsg-0ubuntu2.6 qttools5-dev=5.9.5-0ubuntu1 default-jdk=2:1.11-68ubuntu1~18.04.1 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3.6-dev=3.6.9-1~18.04ubuntu1.12 python3.6-gdal python3-setuptools=39.0.1-2ubuntu0.1 libxmu-dev=2:1.1.2-2 libxi-dev=2:1.7.9-1 qttools5-dev-tools=5.9.5-0ubuntu1 -y \
 && rm -rf /var/lib/apt/lists/*
#   ----------------------------------------------------------------------------
#   Python packages
#   ----------------------------------------------------------------------------
RUN ln -s /usr/bin/python3 /usr/bin/python \
 && python3 -m pip install --upgrade pip \
 && python3 -m pip install pip six numpy wheel mock keras future
#   ----------------------------------------------------------------------------
#   Build TensorFlow
#   ----------------------------------------------------------------------------
RUN export TF_ROOT=/work/tf \
 && mkdir -p ${TF_ROOT}/bazel \
 && cd ${TF_ROOT}/bazel \
 && wget https://github.com/bazelbuild/bazel/releases/download/0.24.1/bazel-0.24.1-installer-linux-x86_64.sh \
 && chmod +x bazel-0.24.1-installer-linux-x86_64.sh \
 && ./bazel-0.24.1-installer-linux-x86_64.sh
RUN export TF_ROOT=/work/tf \
 && export PATH="$PATH:$HOME/bin" \
 && cd $TF_ROOT \
 && git clone https://github.com/tensorflow/tensorflow.git \
 && cd tensorflow \
 && git checkout r1.14 \
 && echo "\n\n\n\n\n\n\n\n\n" | ./configure \
 && bazel build //tensorflow:libtensorflow_framework.so //tensorflow:libtensorflow_cc.so //tensorflow/tools/pip_package:build_pip_package
RUN export TF_ROOT=/work/tf \
 && cd $TF_ROOT/tensorflow \
 && bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/tensorflow_pkg \
 && pip3 install $( find /tmp/tensorflow_pkg/ -type f -iname "tensorflow*.whl" ;) \
 && ./tensorflow/contrib/makefile/build_all_linux.sh \
 && mkdir -p /work/tf/installdir/lib \
 && mkdir -p /work/tf/installdir/include \
 && cp bazel-bin/tensorflow/libtensorflow_cc.so /work/tf/installdir/lib \
 && cp bazel-bin/tensorflow/libtensorflow_framework.so /work/tf/installdir/lib \
 && cp tensorflow/contrib/makefile/gen/protobuf/lib/libprotobuf.a /work/tf/installdir/lib \
 && cp tensorflow/contrib/makefile/downloads/nsync/builds/default.linux.c++11/*.a /work/tf/installdir/lib \
 && cp -r bazel-genfiles/* /work/tf/installdir/include/ \
 && cp -r tensorflow/cc /work/tf/installdir/include/tensorflow/ \
 && cp -r tensorflow/core /work/tf/installdir/include/tensorflow/ \
 && cp -r third_party /work/tf/installdir/include/ \
 && cp -r tensorflow/contrib/makefile/gen/protobuf/include/* /work/tf/installdir/include/ \
 && cp -r tensorflow/contrib/makefile/downloads/eigen/Eigen /work/tf/installdir/include/ \
 && cp -r tensorflow/contrib/makefile/downloads/eigen/unsupported /work/tf/installdir/include/ \
 && cp -r tensorflow/contrib/makefile/downloads/eigen/signature_of_eigen3_matrix_library /work/tf/installdir/include/ \
 && cd ${TF_ROOT}/tensorflow/tensorflow/contrib/makefile/downloads/absl \
 && find absl/ -name '*.h' -exec cp --parents {} /work/tf/installdir/include/
RUN echo "Create symlinks for tensorflow libs" \
 && ln -s /work/tf/installdir/lib/libtensorflow_cc.so /work/tf/installdir/lib/libtensorflow_cc.so.1 \
 && ln -s /work/tf/installdir/lib/libtensorflow_framework.so /work/tf/installdir/lib/libtensorflow_framework.so.1
#   ----------------------------------------------------------------------------
#   Build OTB
#   ----------------------------------------------------------------------------
RUN mkdir -p /work/otb/build \
 && cd /work/otb \
 && git clone https://gitlab.orfeo-toolbox.org/orfeotoolbox/otb.git otb \
 && cd otb \
 && git checkout 0df44b312d64d6c3890b65d3790d4a17d0fd5f23 \
 && cd /work/otb/build \
 && cmake /work/otb/otb/SuperBuild -DUSE_SYSTEM_BOOST=ON -DUSE_SYSTEM_CURL=ON -DUSE_SYSTEM_EXPAT=ON -DUSE_SYSTEM_FFTW=ON -DUSE_SYSTEM_FREETYPE=ON -DUSE_SYSTEM_GDAL=ON -DUSE_SYSTEM_GEOS=ON -DUSE_SYSTEM_GEOTIFF=ON -DUSE_SYSTEM_GLEW=ON -DUSE_SYSTEM_GLFW=ON -DUSE_SYSTEM_GLUT=ON -DUSE_SYSTEM_GSL=ON -DUSE_SYSTEM_ITK=ON -DUSE_SYSTEM_LIBKML=ON -DUSE_SYSTEM_LIBSVM=ON -DUSE_SYSTEM_MUPARSER=ON -DUSE_SYSTEM_MUPARSERX=ON -DUSE_SYSTEM_OPENCV=ON -DUSE_SYSTEM_OPENTHREADS=ON -DUSE_SYSTEM_OSSIM=ON -DUSE_SYSTEM_PNG=ON -DUSE_SYSTEM_QT5=ON -DUSE_SYSTEM_QWT=ON -DUSE_SYSTEM_TINYXML=ON -DUSE_SYSTEM_ZLIB=ON \
 && cd /work/otb/otb/Modules/Remote \
 && git clone https://github.com/remicres/otbtf.git \
 && cd /work/otb/build/OTB/build \
 && cmake /work/otb/otb -DModule_Mosaic=ON -DModule_OTBTensorflow=ON -DOTB_USE_TENSORFLOW=ON -Dopencv_INCLUDE_DIR=/usr/include -DTENSORFLOW_CC_LIB=/work/tf/installdir/lib/libtensorflow_cc.so -DTENSORFLOW_FRAMEWORK_LIB=/work/tf/installdir/lib/libtensorflow_framework.so -Dtensorflow_include_dir=/work/tf/installdir/include/ \
 && cd /work/otb/build/ \
 && make -j $( grep -c ^processor /proc/cpuinfo ;)
ENV PATH="\"$PATH:/work/otb/superbuild_install/bin/\""
ENV LD_LIBRARY_PATH="\"$LD_LIBRARY_PATH:/work/otb/superbuild_install/lib/:/work/tf/installdir/lib/\""
ENV PYTHONPATH="\"$PYTHONPATH:/work/otb/superbuild_install/lib/otb/python/\""
ENV PATH="\"$PATH:/work/otb/superbuild_install/bin/\""
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
