FROM ubuntu:16.04
MAINTAINER Waleed Abdulla <waleed.abdulla@gmail.com>
#   Supress warnings about missing front-end. As recommended at:
#   http://stackoverflow.com/questions/22466255/is-it-possibe-to-answer-dialog-questions-when-installing-under-docker
ARG DEBIAN_FRONTEND=noninteractive
#   Essentials: developer tools, build tools, OpenBLAS
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils=1.2.35 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 vim=2:7.4.1689-3ubuntu1.5 unzip=6.0-20ubuntu1.1 openssh-client=1:7.2p2-4ubuntu2.10 wget=1.17.1-1ubuntu1.5 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 libopenblas-dev=0.2.18-1ubuntu1 -y
#
#   Python 3.5
#
#   For convenience, alias (but don't sym-link) python & pip to python3 & pip3 as recommended in:
#   http://askubuntu.com/questions/351318/changing-symlink-python-to-python3-causes-problems
RUN apt-get install --no-install-recommends python3.5=3.5.2-2ubuntu0~16.04.13 python3.5-dev=3.5.2-2ubuntu0~16.04.13 python3-pip=8.1.1-2ubuntu0.6 python3-tk=3.5.1-1 -y \
 && pip3 install --no-cache-dir --upgrade pip setuptools \
 && echo "alias python='python3'" >> /root/.bash_aliases \
 && echo "alias pip='pip3'" >> /root/.bash_aliases
#   Pillow and it's dependencies
RUN apt-get install --no-install-recommends libjpeg-dev=8c-2ubuntu8 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y \
 && pip3 --no-cache-dir install Pillow
#   Science libraries and other common packages
RUN pip3 --no-cache-dir install numpy scipy sklearn scikit-image pandas matplotlib Cython requests
#
#   Jupyter Notebook
#
#   Allow access from outside the container, and skip trying to open a browser.
#   NOTE: disable authentication token for convenience. DON'T DO THIS ON A PUBLIC SERVER.
RUN pip3 --no-cache-dir install jupyter \
 && mkdir /root/.jupyter \
 && echo "c.NotebookApp.ip = '*'" "\nc.NotebookApp.open_browser = False" "\nc.NotebookApp.token = ''" > /root/.jupyter/jupyter_notebook_config.py
EXPOSE 8888/tcp
#
#   Tensorflow 1.6.0 - CPU
#
RUN pip3 install --no-cache-dir --upgrade tensorflow
#   Expose port for TensorBoard
EXPOSE 6006/tcp
#
#   OpenCV 3.4.1
#
#   Dependencies
RUN apt-get install --no-install-recommends libjpeg8-dev=8c-2ubuntu8 libtiff5-dev=4.0.6-1ubuntu0.8 libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libpng12-dev=1.2.54-1ubuntu1.1 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libswscale-dev=7:2.8.17-0ubuntu0.1 libv4l-dev=1.10.0-1 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 liblapacke-dev=3.6.0-2ubuntu2 checkinstall=1.6.2-4ubuntu1 -y
#   Get source from github
RUN git clone -b 3.4.1 --depth 1 https://github.com/opencv/opencv.git /usr/local/src/opencv
#   Compile
RUN cd /usr/local/src/opencv \
 && mkdir build \
 && cd build \
 && cmake -D CMAKE_INSTALL_PREFIX=/usr/local -D BUILD_TESTS=OFF -D BUILD_PERF_TESTS=OFF -D PYTHON_DEFAULT_EXECUTABLE=$( which python3 ;) .. \
 && make -j"$( nproc ;)" \
 && make install
#
#   Caffe
#
#   Dependencies
RUN apt-get install --no-install-recommends cmake=3.5.1-1ubuntu3 libprotobuf-dev=2.6.1-1.3 libleveldb-dev=1.18-5 libsnappy-dev=1.1.3-2 libopencv-dev=2.4.9.1+dfsg-1.5ubuntu1.1 libhdf5-serial-dev=1.8.16+docs-4ubuntu1.1 protobuf-compiler=2.6.1-1.3 liblmdb-dev=0.9.17-3 libgoogle-glog-dev=0.3.4-0.1 libboost-all-dev=1.58.0.1ubuntu1 -y \
 && pip3 install lmdb
#   Get source. Use master branch because the latest stable release (rc3) misses critical fixes.
RUN git clone -b master --depth 1 https://github.com/BVLC/caffe.git /usr/local/src/caffe
#   Python dependencies
RUN pip3 --no-cache-dir install -r /usr/local/src/caffe/python/requirements.txt
#   Compile
RUN cd /usr/local/src/caffe \
 && mkdir build \
 && cd build \
 && cmake -D CPU_ONLY=ON -D python_version=3 -D BLAS=open -D USE_OPENCV=ON .. \
 && make -j"$( nproc ;)" all \
 && make install
#   Enivronment variables
ENV PYTHONPATH="/usr/local/src/caffe/python:$PYTHONPATH" \
    PATH="/usr/local/src/caffe/build/tools:$PATH"
#   Fix: old version of python-dateutil breaks caffe. Update it.
RUN pip3 install --no-cache-dir python-dateutil --upgrade
#
#   Java
#
#   Install JDK (Java Development Kit), which includes JRE (Java Runtime
#   Environment). Or, if you just want to run Java apps, you can install
#   JRE only using: apt install default-jre
RUN apt-get install --no-install-recommends default-jdk=2:1.8-56ubuntu2 -y
#
#   Keras 2.1.5
#
RUN pip3 install --no-cache-dir --upgrade h5py pydot_ng keras
#
#   PyTorch 0.3.1
#
RUN pip3 install http://download.pytorch.org/whl/cpu/torch-0.3.1-cp35-cp35m-linux_x86_64.whl \
 && pip3 install torchvision
#
#   PyCocoTools
#
#   Using a fork of the original that has a fix for Python 3.
#   I submitted a PR to the original repo (https://github.com/cocodataset/cocoapi/pull/50)
#   but it doesn't seem to be active anymore.
RUN pip3 install --no-cache-dir git+https://github.com/waleedka/coco.git#subdirectory=PythonAPI
WORKDIR "/root"
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
