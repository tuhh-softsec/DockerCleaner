FROM ubuntu:14.04
ARG THEANO_VERSION=rel-0.8.2
ARG TENSORFLOW_VERSION=0.12.1
ARG TENSORFLOW_ARCH=cpu
RUN apt-get update \
 && apt-get install git -y
#  Install some dependencies
RUN apt-get update \
 && apt-get install bc build-essential cmake curl g++ gfortran git libffi-dev libfreetype6-dev libhdf5-dev libjpeg-dev liblcms2-dev libopenblas-dev liblapack-dev libopenjpeg2 libpng12-dev libssl-dev libtiff5-dev libwebp-dev libzmq3-dev nano pkg-config python-dev software-properties-common unzip vim wget zlib1g-dev qt5-default libvtk6-dev zlib1g-dev libjpeg-dev libwebp-dev libpng-dev libtiff5-dev libjasper-dev libopenexr-dev libgdal-dev libdc1394-22-dev libavcodec-dev libavformat-dev libswscale-dev libtheora-dev libvorbis-dev libxvidcore-dev libx264-dev yasm libopencore-amrnb-dev libopencore-amrwb-dev libv4l-dev libxine2-dev libtbb-dev libeigen3-dev python-dev python-tk python-numpy python3-dev python3-tk python3-numpy ant default-jdk doxygen -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* \
 && update-alternatives --set libblas.so.3 /usr/lib/openblas-base/libblas.so.3
#  Install pip
RUN curl -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
#  Add SNI support to Python
RUN pip install pyopenssl ndg-httpsclient pyasn1 --no-cache-dir
#  Install useful Python packages using apt-get to avoid version incompatibilities with Tensorflow binary
#  especially numpy, scipy, skimage and sklearn (see https://github.com/tensorflow/tensorflow/issues/2034)
RUN apt-get update \
 && apt-get install python-numpy python-scipy python-nose python-h5py python-skimage python-matplotlib python-pandas python-sklearn python-sympy -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
#  Install other useful Python packages using pip
RUN pip install ipython --no-cache-dir --upgrade \
 && pip install Cython ipykernel jupyter path.py Pillow pygments six sphinx wheel zmq --no-cache-dir \
 && python -m ipykernel.kernelspec
#  install Tensorflow and Theano
RUN pip install https://storage.googleapis.com/tensorflow/linux/${TENSORFLOW_ARCH}/tensorflow-${TENSORFLOW_VERSION}-cp27-none-linux_x86_64.whl --no-cache-dir
#  Install Theano and set up Theano config (.theanorc) OpenBLAS
RUN pip install git+git://github.com/Theano/Theano.git@${THEANO_VERSION} --no-cache-dir \
 && echo "[global]\ndevice=cpu\nfloatX=float32\nmode=FAST_RUN \n[lib]\ncnmem=0.95 \n[nvcc]\nfastmath=True \n[blas]\nldflag = -L/usr/lib/openblas-base -lopenblas \n[DebugMode]\ncheck_finite=1" > /root/.theanorc
#  Install Pytorch
RUN pip install http://download.pytorch.org/whl/cu75/torch-0.1.12.post2-cp27-none-linux_x86_64.whl --no-cache-dir
RUN pip install torchvision --no-cache-dir
#  copy the current repository
COPY . /root/convai
#  Install dependencies
RUN pip install -r /root/convai/requirements.txt
#  clone the models
RUN cd /root/convai/models \
 && ./setup
#  download the data
RUN cd /root/convai/data \
 && ./setup
EXPOSE 8000/tcp
#  run the bot
CMD python /root/convai/bot.py
