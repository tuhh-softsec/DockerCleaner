FROM ubuntu:14.04
MAINTAINER Datmo devs <dev@datmo.io>
RUN apt-get update ; apt-get install --no-install-recommends python=2.7.5-5ubuntu3 python-pip=1.5.4-1ubuntu4 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 build-essential=11.6ubuntu6 python-dev=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 libatlas-dev=3.10.1-4 libatlas3gf-base=3.10.1-4 -y
RUN update-alternatives --set libblas.so.3 /usr/lib/atlas-base/atlas/libblas.so.3 ; update-alternatives --set liblapack.so.3 /usr/lib/atlas-base/atlas/liblapack.so.3
RUN pip install scikit-learn==1.2.2 -U
#   All other dependencies are more or less needed by building phase of OpenCV.
#   The last "apt-get clean" command is needed to reduce Docker image size.
RUN apt-get upgrade -y \
 && apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y \
 && add-apt-repository ppa:saiarcot895/myppa \
 && apt-get update \
 && apt-get install --no-install-recommends apt-fast -y \
 && apt-fast install -y build-essential cmake git pkg-config libgtk2.0-dev libavcodec-dev libavformat-dev libswscale-dev libtbb2 libtbb-dev libjpeg-dev libpng-dev libtiff-dev libjasper-dev libdc1394-22-dev libdc1394-22 libdc1394-utils libv4l-0 libv4l-dev libgl1-mesa-dev libgles1-mesa-dev libgles2-mesa-dev libopenvg1-mesa-dev libglu1-mesa-dev libgtkglext1 libgtkglext1-dev openjdk-7-jdk ant vtk6 libvtk6-dev \
 && apt-get clean
#   Install pip
RUN curl -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
#   Install other useful Python packages using pip
RUN pip install ipython==8.12.0 --upgrade \
 && pip install Cython==0.29.34 ipykernel==6.22.0 jupyter==1.0.0 path.py==12.5.0 Pillow==9.5.0 pygments==2.15.0 six==1.16.0 sphinx==6.1.3 wheel==0.40.0 zmq==0.0.0 \
 && python -m ipykernel.kernelspec
#   git clone the repo from OpenCV official repository on GitHub.
RUN mkdir /opt/opencv-build \
 && cd /opt/opencv-build \
 && git clone https://github.com/Itseez/opencv \
 && cd opencv \
 && git checkout master \
 && mkdir build
WORKDIR /opt/opencv-build/opencv/build
ENV JAVA_HOME="/usr/lib/jvm/java-1.7.0-openjdk-amd64"
#   OpenCV repository is kept but all building intermediate files are removed.
#   All other dependencies is using the default settings from CMake file of OpenCV.
RUN cmake -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=/opt .. \
 && make -j2 \
 && make install \
 && make clean \
 && cd .. \
 && rm -rf build
#   Let python can find the newly install OpenCV modules.
RUN echo '/opt/lib/python2.7/dist-packages/' > /usr/lib/python2.7/dist-packages/cv2.pth
RUN echo 'ln /dev/null /dev/raw1394' >> ~/.bashrc
#  Jupyter notebook related configs
COPY jupyter_notebook_config.py /root/.jupyter/
EXPOSE 8888/tcp
#   Jupyter has issues with being run directly: https://github.com/ipython/ipython/issues/7062
COPY run_jupyter.sh /home/
#  Adding flask
RUN pip install flask==2.2.3
EXPOSE 5000/tcp
WORKDIR /workspace
RUN chmod -R a+w /workspace
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
