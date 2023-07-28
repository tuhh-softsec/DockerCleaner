FROM ubuntu:16.04
MAINTAINER MadDevs <rock@maddevs.io>
ENV OPENCV_VER="3.2.0"
ENV SOURCE_URL="https://github.com/opencv/opencv/archive/${OPENCV_VER}.tar.gz"
ENV CONTRIB_URL="https://github.com/opencv/opencv_contrib/archive/${OPENCV_VER}.tar.gz"
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 libtool=2.4.6-0.1 autoconf=2.69-9 automake=1:1.15-4ubuntu1 pkg-config=0.29.1-0ubuntu1 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 libatlas-base-dev=3.10.2-9 libboost-all-dev=1.58.0.1ubuntu1 libgflags-dev=2.1.2-3 libgoogle-glog-dev=0.3.4-0.1 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 libleveldb-dev=1.18-5 liblmdb-dev=0.9.17-3 libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 libprotobuf-dev=2.6.1-1.3 libsnappy-dev=1.1.3-2 protobuf-compiler=2.6.1-1.3 libtbb2=4.4~20151115-0ubuntu3 libdc1394-22-dev=2.2.4-1 libdc1394-22-dev=2.2.4-1 libleptonica-dev=1.73-1 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libswscale-dev=7:2.8.17-0ubuntu0.1 libtbb-dev=4.4~20151115-0ubuntu3 libjpeg-dev=8c-2ubuntu8 libpng-dev libtiff-dev libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libtesseract-dev=3.04.01-4 tesseract-ocr=3.04.01-4 tesseract-ocr-eng=3.04.00-1 tesseract-ocr-rus=3.04.00-1 tesseract-ocr-kir=3.04.00-1 python2.7-dev=2.7.12-1ubuntu0~16.04.18 python-pip=8.1.1-2ubuntu0.6 python-numpy=1:1.11.0-1ubuntu1 python-setuptools=20.7.0-1 python-scipy=0.17.0-1 -y \
 && pip install pip==23.1 --upgrade \
 && pip install Cython==0.25.2 \
 && rm -rf /var/lib/apt/lists/*
#   install opencv
RUN mkdir -p /tmp/opencv/build /tmp/opencv_contrib \
 && curl -Ls ${SOURCE_URL} | tar -xz --strip=1 -C /tmp/opencv \
 && curl -Ls ${CONTRIB_URL} | tar -xz --strip=1 -C /tmp/opencv_contrib \
 && cd /tmp/opencv/build \
 && cmake -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=/usr/local -D BUILD_PYTHON_SUPPORT=ON -D OPENCV_EXTRA_MODULES_PATH=/tmp/opencv_contrib/modules/ .. \
 && make -j "$( nproc ;)" \
 && make install \
 && ldconfig \
 && cd / \
 && rm -rf /tmp/*
#   install caffe
ENV CAFFE_VER="1.0"
ENV CAFFE_SRC="/tmp/caffe"
RUN set -ex \
 && mkdir -p ${CAFFE_SRC}/build \
 && curl -Ls https://github.com/BVLC/caffe/archive/${CAFFE_VER}.tar.gz | tar -xz --strip=1 -C ${CAFFE_SRC} \
 && for req in $( cat ${CAFFE_SRC}/python/requirements.txt ;) pydot; do pip install $req ; done \
 && cd ${CAFFE_SRC}/build \
 && cmake -DCPU_ONLY=1 -DOPENCV_VERSION=3 -D CMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j"$( nproc ;)" \
 && make install \
 && ldconfig \
 && cd / \
 && rm -rf /tmp/*
ENV PYCAFFE_ROOT="/usr/local/python"
ENV PYTHONPATH="$PYCAFFE_ROOT:$PYTHONPATH"
ENV UWSGI_CPU_AFFINITY="2"
ENV UWSGI_PROCESSES="4"
ENV UWSGI_HARAKIRI="60"
WORKDIR /webapp
COPY requirements.txt /webapp
RUN pip install -r requirements.txt
COPY . /webapp
RUN mkdir -p /webapp/web/uploads
WORKDIR /webapp/web
EXPOSE 8080/tcp
CMD ["uwsgi", "--ini", "uwsgi.ini"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
