FROM ubuntu:16.04
MAINTAINER Moch. Ainun Najib <ec2ainun@gmail.com>
ARG TENSORFLOW_VERSION=1.2.0
ARG TENSORFLOW_ARCH=cpu
#   Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends bc=1.06.95-9build1 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 g++=4:5.3.1-1ubuntu1 gfortran=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 libffi-dev=3.2.1-4 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libhdf5-dev=1.8.16+docs-4ubuntu1.1 libjpeg-dev=8c-2ubuntu8 liblcms2-dev=2.6-3ubuntu2.1 libopenblas-dev=0.2.18-1ubuntu1 liblapack-dev=3.6.0-2ubuntu2 libopenjpeg5=1:1.5.2-3.1 libpng12-dev=1.2.54-1ubuntu1.1 libssl-dev=1.0.2g-1ubuntu4.20 libtiff5-dev=4.0.6-1ubuntu0.8 libwebp-dev=0.4.4-1 libzmq3-dev=4.1.4-7ubuntu0.1 nano=2.5.3-2ubuntu2 pkg-config=0.29.1-0ubuntu1 python-dev=2.7.12-1~16.04 software-properties-common=0.96.20.10 unzip=6.0-20ubuntu1.1 vim=2:7.4.1689-3ubuntu1.5 wget=1.17.1-1ubuntu1.5 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 qt5-default=5.5.1+dfsg-16ubuntu7.7 libvtk6-dev=6.2.0+dfsg1-10ubuntu0.1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libjpeg-dev=8c-2ubuntu8 libwebp-dev=0.4.4-1 libpng-dev libtiff5-dev=4.0.6-1ubuntu0.8 libjasper-dev=1.900.1-debian1-2.4ubuntu1.3 libopenexr-dev=2.2.0-10ubuntu2.6 libgdal-dev=1.11.3+dfsg-3build2 libdc1394-22-dev=2.2.4-1 libavcodec-dev=7:2.8.17-0ubuntu0.1 libavformat-dev=7:2.8.17-0ubuntu0.1 libswscale-dev=7:2.8.17-0ubuntu0.1 libtheora-dev=1.1.1+dfsg.1-8 libvorbis-dev=1.3.5-3ubuntu0.2 libxvidcore-dev=2:1.3.4-1 libx264-dev=2:0.148.2643+git5c65704-1 yasm=1.3.0-2 libopencore-amrnb-dev=0.1.3-2.1 libopencore-amrwb-dev=0.1.3-2.1 libv4l-dev=1.10.0-1 libxine2-dev=1.2.6-1build5 libtbb-dev=4.4~20151115-0ubuntu3 libeigen3-dev=3.3~beta1-2 python3-dev=3.5.1-3 python3-tk=3.5.1-1 python3-numpy=1:1.11.0-1ubuntu1 python3-pip=8.1.1-2ubuntu0.6 ant=1.9.6-1ubuntu1.1 default-jdk=2:1.8-56ubuntu2 doxygen=1.8.11-1ubuntu0.1 -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
RUN pip3 install --upgrade pip
#   Add SNI support to Python
RUN pip3 --no-cache-dir install pyopenssl ndg-httpsclient pyasn1
#   Install other useful Python packages using pip3
RUN pip3 --no-cache-dir install --upgrade ipython \
 && pip3 --no-cache-dir install Cython ipykernel jupyter path.py Pillow h5py pygments six sphinx wheel zmq matplotlib numpy pandas scipy scikit-learn \
 && python3 -m ipykernel.kernelspec
RUN apt-get update \
 && apt-get install --no-install-recommends python3-nose=1.3.7-1 python3-skimage=0.10.1-2build1 python3-sympy=0.7.6.1-1 -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
#   Install TensorFlow
RUN pip3 --no-cache-dir install https://storage.googleapis.com/tensorflow/linux/${TENSORFLOW_ARCH}/tensorflow-${TENSORFLOW_VERSION}-cp35-cp35m-linux_x86_64.whl
COPY *.sh /
#   Install Opencv
RUN bash Opencv.sh
#   Set up notebook config
COPY jupyter_notebook_config.py /root/.jupyter/
COPY jalankan.sh /root/
EXPOSE 6006/tcp 8888/tcp
COPY *.ipynb /notebooks/
WORKDIR /notebooks
RUN chmod +x /root/jalankan.sh
CMD ["/root/jalankan.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
