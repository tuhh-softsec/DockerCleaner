ARG CUDA_DOCKER_VERSION=9.0-devel-ubuntu16.04
FROM nvidia/cuda:${CUDA_DOCKER_VERSION}
#  Arguments for the build. CUDA_DOCKER_VERSION needs to be repeated becaus
#  the first usage only applies to the FROM tag.
ARG CUDA_DOCKER_VERSION=9.0-devel-ubuntu16.04
ARG CUDNN_VERSION=7.4.1.5-1+cuda9.0
ARG NCCL_VERSION_OVERRIDE=2.3.7-1+cuda9.0
ARG MPI_KIND=OpenMPI
ARG PYTHON_VERSION=2.7
ARG TENSORFLOW_PACKAGE=tensorflow-gpu==1.12.0
ARG KERAS_PACKAGE=keras==2.2.2
ARG PYTORCH_PACKAGE=torch==1.0.0
ARG TORCHVISION_PACKAGE=torchvision==0.2.2.post3
ARG MXNET_PACKAGE=mxnet-cu90==1.4.1
ARG PYSPARK_PACKAGE=pyspark==2.4.0
ARG HOROVOD_BUILD_FLAGS=HOROVOD_GPU_ALLREDUCE=NCCL
ARG HOROVOD_MIXED_INSTALL=0
#  Set default shell to /bin/bash
SHELL ["/bin/bash", "-cu"]
#  Install essential packages.
RUN apt-get update -qq
RUN apt-get install --no-install-recommends wget ca-certificates openssh-client git build-essential gcc-4.9 g++-4.9 gcc-4.9-base software-properties-common libcudnn7=${CUDNN_VERSION} libnccl2=${NCCL_VERSION_OVERRIDE} libnccl-dev=${NCCL_VERSION_OVERRIDE} -y --allow-downgrades --allow-change-held-packages
#  Install Python.
RUN if [[ "${PYTHON_VERSION}" == "3.6" ]] ; then add-apt-repository ppa:deadsnakes/ppa \
 && apt-get update -qq ; fi
RUN apt-get install python${PYTHON_VERSION} python${PYTHON_VERSION}-dev -y
RUN ln -s -f /usr/bin/python${PYTHON_VERSION} /usr/bin/python
RUN wget https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN pip install pip setuptools requests pytest -U --force
#  Install PySpark.
RUN apt-get install -y openjdk-8-jdk-headless
RUN pip install ${PYSPARK_PACKAGE}
#  Install MPI.
RUN if [[ ${MPI_KIND} == "OpenMPI" ]] ; then wget -O /tmp/openmpi-3.0.0-bin.tar.gz https://github.com/horovod/horovod/files/1596799/openmpi-3.0.0-bin.tar.gz \
 && cd /usr/local \
 && tar -zxf /tmp/openmpi-3.0.0-bin.tar.gz \
 && ldconfig \
 && echo "mpirun -allow-run-as-root -np 2 -H localhost:2 -bind-to none -map-by slot -mca mpi_abort_print_stack 1" > /mpirun_command; else apt-get install mpich -y \
 && echo "mpirun -np 2" > /mpirun_command; fi
#  Set default NCCL parameters
RUN echo NCCL_DEBUG=INFO >> /etc/nccl.conf
#  Install mpi4py.
RUN pip install mpi4py
# ## END OF CACHE ###
COPY . /horovod
#  Install TensorFlow.
RUN pip install ${TENSORFLOW_PACKAGE}
#  Install Keras.
RUN pip install h5py scipy pandas ${KERAS_PACKAGE}
RUN mkdir -p ~/.keras
RUN ldconfig /usr/local/cuda/targets/x86_64-linux/lib/stubs \
 && python -c "from keras.datasets import mnist; mnist.load_data()" \
 && ldconfig
#  Install PyTorch.
RUN if [[ ${PYTORCH_PACKAGE} == "torch-nightly" ]] ; then PYTORCH_CUDA=$( echo ${CUDA_DOCKER_VERSION} | awk -F- '{print $1}' | sed 's/\.//' ;) ;pip install torch_nightly -v -f https://download.pytorch.org/whl/nightly/cu${PYTORCH_CUDA}/torch_nightly.html ; else pip install ${PYTORCH_PACKAGE} ; fi
RUN pip install Pillow ${TORCHVISION_PACKAGE} --no-deps
RUN pip install future typing
#  Install MXNet.
RUN pip install ${MXNET_PACKAGE}
#  Pin GCC to 4.9 (priority 200) to compile correctly against TensorFlow, PyTorch, and MXNet.
#  Backup existing GCC installation as priority 100, so that it can be recovered later.
RUN update-alternatives --install /usr/bin/gcc gcc $( readlink -f $( which gcc ;) ;) 100 \
 && update-alternatives --install /usr/bin/x86_64-linux-gnu-gcc x86_64-linux-gnu-gcc $( readlink -f $( which gcc ;) ;) 100 \
 && update-alternatives --install /usr/bin/g++ g++ $( readlink -f $( which g++ ;) ;) 100 \
 && update-alternatives --install /usr/bin/x86_64-linux-gnu-g++ x86_64-linux-gnu-g++ $( readlink -f $( which g++ ;) ;) 100
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.9 200 \
 && update-alternatives --install /usr/bin/x86_64-linux-gnu-gcc x86_64-linux-gnu-gcc /usr/bin/gcc-4.9 200 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 200 \
 && update-alternatives --install /usr/bin/x86_64-linux-gnu-g++ x86_64-linux-gnu-g++ /usr/bin/g++-4.9 200
#  Install Horovod.
RUN cd /horovod \
 && python setup.py sdist
RUN ldconfig /usr/local/cuda/targets/x86_64-linux/lib/stubs \
 && bash -c "${HOROVOD_BUILD_FLAGS} pip install -v /horovod/dist/horovod-*.tar.gz" \
 && ldconfig
#  Remove GCC pinning
RUN update-alternatives --remove gcc /usr/bin/gcc-4.9 \
 && update-alternatives --remove x86_64-linux-gnu-gcc /usr/bin/gcc-4.9 \
 && update-alternatives --remove g++ /usr/bin/g++-4.9 \
 && update-alternatives --remove x86_64-linux-gnu-g++ /usr/bin/g++-4.9
#  Hack for compatibility of MNIST example with TensorFlow 1.1.0.
RUN if [[ ${TENSORFLOW_PACKAGE} == "tensorflow-gpu==1.1.0" ]] ; then sed -i "s/from tensorflow import keras/from tensorflow.contrib import keras/" /horovod/examples/tensorflow_mnist.py ; fi
#  Hack TensorFlow MNIST example to be smaller.
RUN sed -i "s/last_step=20000/last_step=100/" /horovod/examples/tensorflow_mnist.py
#  Hack TensorFlow Eager MNIST example to be smaller.
RUN sed -i "s/dataset.take(20000/dataset.take(100/" /horovod/examples/tensorflow_mnist_eager.py
#  Hack Keras MNIST advanced example to be smaller.
RUN sed -i "s/epochs = .*/epochs = 9/" /horovod/examples/keras_mnist_advanced.py
#  Hack PyTorch MNIST example to be smaller.
RUN sed -i "s/'--epochs', type=int, default=10,/'--epochs', type=int, default=2,/" /horovod/examples/pytorch_mnist.py
#  Export HOROVOD_MIXED_INSTALL
ENV HOROVOD_MIXED_INSTALL="${HOROVOD_MIXED_INSTALL}"
