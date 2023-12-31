ARG UBUNTU_VERSION=16.04
FROM ubuntu:${UBUNTU_VERSION}
#  Arguments for the build. UBUNTU_VERSION needs to be repeated because
#  the first usage only applies to the FROM tag.
ARG UBUNTU_VERSION=16.04
ARG MPI_KIND=OpenMPI
ARG PYTHON_VERSION=2.7
ARG TENSORFLOW_PACKAGE=tensorflow==1.12.0
ARG KERAS_PACKAGE=keras==2.2.2
ARG PYTORCH_PACKAGE=torch==1.0.0
ARG TORCHVISION_PACKAGE=torchvision==0.2.2.post3
ARG MXNET_PACKAGE=mxnet==1.4.1
ARG PYSPARK_PACKAGE=pyspark==2.4.0
#  Set default shell to /bin/bash
SHELL ["/bin/bash", "-cu"]
#  Install essential packages.
RUN apt-get update -qq
RUN apt-get install --no-install-recommends wget ca-certificates openssh-client git build-essential gcc-4.9 g++-4.9 gcc-4.9-base software-properties-common -y
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
 && echo "mpirun -allow-run-as-root -np 2 -H localhost:2 -bind-to none -map-by slot -mca mpi_abort_print_stack 1" > /mpirun_command; elif [[ ${MPI_KIND} == "MLSL" ]] ; then wget -O /tmp/l_mlsl_2018.3.008.tgz https://github.com/intel/MLSL/releases/download/IntelMLSL-v2018.3-Preview/l_mlsl_2018.3.008.tgz \
 && cd /tmp \
 && tar -zxf /tmp/l_mlsl_2018.3.008.tgz \
 && /tmp/l_mlsl_2018.3.008/install.sh -s -d /usr/local/mlsl \
 && wget https://raw.githubusercontent.com/intel/MLSL/master/mpirt_2019/include/mpi.h -P /usr/local/mlsl/intel64/include \
 && wget https://raw.githubusercontent.com/intel/MLSL/master/mpirt_2019/include/mpio.h -P /usr/local/mlsl/intel64/include \
 && wget https://raw.githubusercontent.com/intel/MLSL/master/mpirt_2019/include/mpicxx.h -P /usr/local/mlsl/intel64/include \
 && wget https://raw.githubusercontent.com/AlekseyMarchuk/MLSL/master/mpirt_2019/bin/mpicc -P /usr/local/mlsl/intel64/bin \
 && chmod +x /usr/local/mlsl/intel64/bin/mpicc \
 && wget https://raw.githubusercontent.com/AlekseyMarchuk/MLSL/master/mpirt_2019/bin/mpicxx -P /usr/local/mlsl/intel64/bin \
 && chmod +x /usr/local/mlsl/intel64/bin/mpicxx \
 && wget https://raw.githubusercontent.com/AlekseyMarchuk/MLSL/master/mpirt_2019/bin/mpigcc -P /usr/local/mlsl/intel64/bin \
 && chmod +x /usr/local/mlsl/intel64/bin/mpigcc \
 && wget https://raw.githubusercontent.com/AlekseyMarchuk/MLSL/master/mpirt_2019/bin/mpigxx -P /usr/local/mlsl/intel64/bin \
 && chmod +x /usr/local/mlsl/intel64/bin/mpigxx \
 && wget https://raw.githubusercontent.com/AlekseyMarchuk/MLSL/master/mpirt_2019/lib/libmpicxx.so -P /usr/local/mlsl/intel64/lib \
 && chmod +x /usr/local/mlsl/intel64/lib/libmpicxx.so \
 && echo ". /usr/local/mlsl/intel64/bin/mlslvars.sh \"thread\"; echo \"mpirun is $(which mpirun)\"; echo \"this file is $(cat /mpirun_command_script)\"; echo \"LD_LIBRARY_PATH is $(echo $LD_LIBRARY_PATH)\"; echo \"mlsl links with $(ldd /usr/local/mlsl/intel64/lib/libmlsl.so)\"; mpirun -np 2 -ppn 2 -hosts localhost $@" > /mpirun_command_script \
 && chmod +x /mpirun_command_script \
 && echo "-L/usr/local/mlsl/intel64/lib/thread -lmpi -I/usr/local/mlsl/intel64/include" > /mpicc_mlsl \
 && chmod +x /mpicc_mlsl \
 && echo "/mpirun_command_script" > /mpirun_command; else apt-get install mpich -y \
 && echo "mpirun -np 2" > /mpirun_command; fi
#  Install mpi4py.
RUN if [[ ${MPI_KIND} == "MLSL" ]] ; then export I_MPI_ROOT=/usr/local/mlsl ;export MPICC=/usr/local/mlsl/intel64/bin/mpicc ; fi ; pip install mpi4py
# ## END OF CACHE ###
COPY . /horovod
#  Install TensorFlow.
RUN pip install ${TENSORFLOW_PACKAGE}
#  Install Keras.
RUN pip install h5py scipy pandas ${KERAS_PACKAGE}
RUN mkdir -p ~/.keras
RUN python -c "from keras.datasets import mnist; mnist.load_data()"
#  Install PyTorch.
RUN if [[ ${PYTORCH_PACKAGE} == "torch-nightly" ]] ; then pip install torch_nightly -v -f https://download.pytorch.org/whl/nightly/cpu/torch_nightly.html ; else pip install ${PYTORCH_PACKAGE} ; fi
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
RUN if [[ ${MPI_KIND} == "MLSL" ]] ; then if [ -z "${LD_LIBRARY_PATH:-}" ] ; then export LD_LIBRARY_PATH="" ; fi ;if [ -z "${PYTHONPATH:-}" ] ; then export PYTHONPATH="" ; fi ;source /usr/local/mlsl/intel64/bin/mlslvars.sh "thread" ;export I_MPI_ROOT=/usr/local/mlsl ;export PIP_HOROVOD_MPICXX_SHOW=/usr/local/mlsl/intel64/bin/mpicxx ;echo "horovod python setup.py sdist, mpicxx is $( which mpicxx ;)" ;cd /horovod \
 && python setup.py sdist ; else cd /horovod \
 && python setup.py sdist ; fi
# RUN if [[ ${MPI_KIND} == "MLSL" ]]; then \
#         source /usr/local/mlsl/intel64/bin/mlslvars.sh "thread"; \
#     fi; \
#     pip install -v /horovod/dist/horovod-*.tar.gz
RUN if [[ ${MPI_KIND} == "MLSL" ]] ; then if [ -z "${LD_LIBRARY_PATH:-}" ] ; then export LD_LIBRARY_PATH="" ; fi ;if [ -z "${PYTHONPATH:-}" ] ; then export PYTHONPATH="" ; fi ;source /usr/local/mlsl/intel64/bin/mlslvars.sh "thread" ;echo "pip install horovod, mpicxx is $( which mpicxx ;)" ;pip install /horovod/dist/horovod-*.tar.gz -v ; else pip install /horovod/dist/horovod-*.tar.gz -v ; fi
#  Remove GCC pinning
RUN update-alternatives --remove gcc /usr/bin/gcc-4.9 \
 && update-alternatives --remove x86_64-linux-gnu-gcc /usr/bin/gcc-4.9 \
 && update-alternatives --remove g++ /usr/bin/g++-4.9 \
 && update-alternatives --remove x86_64-linux-gnu-g++ /usr/bin/g++-4.9
#  Hack for compatibility of MNIST example with TensorFlow 1.1.0.
RUN if [[ ${TENSORFLOW_PACKAGE} == "tensorflow==1.1.0" ]] ; then sed -i "s/from tensorflow import keras/from tensorflow.contrib import keras/" /horovod/examples/tensorflow_mnist.py ; fi
#  Hack TensorFlow MNIST example to be smaller.
RUN sed -i "s/last_step=20000/last_step=100/" /horovod/examples/tensorflow_mnist.py
#  Hack TensorFlow Eager MNIST example to be smaller.
RUN sed -i "s/dataset.take(20000/dataset.take(100/" /horovod/examples/tensorflow_mnist_eager.py
#  Hack Keras MNIST advanced example to be smaller.
RUN sed -i "s/epochs = .*/epochs = 9/" /horovod/examples/keras_mnist_advanced.py
RUN sed -i "s/model.add(Conv2D(32, kernel_size=(3, 3),/model.add(Conv2D(1, kernel_size=(3, 3),/" /horovod/examples/keras_mnist_advanced.py
RUN sed -i "s/model.add(Conv2D(64, (3, 3), activation='relu'))//" /horovod/examples/keras_mnist_advanced.py
#  Hack PyTorch MNIST example to be smaller.
RUN sed -i "s/'--epochs', type=int, default=10,/'--epochs', type=int, default=2,/" /horovod/examples/pytorch_mnist.py
RUN sed -i "s/self.fc1 = nn.Linear(320, 50)/self.fc1 = nn.Linear(784, 50)/" /horovod/examples/pytorch_mnist.py
RUN sed -i "s/x = F.relu(F.max_pool2d(self.conv1(x), 2))//" /horovod/examples/pytorch_mnist.py
RUN sed -i "s/x = F.relu(F.max_pool2d(self.conv2_drop(self.conv2(x)), 2))//" /horovod/examples/pytorch_mnist.py
RUN sed -i "s/x = x.view(-1, 320)/x = x.view(-1, 784)/" /horovod/examples/pytorch_mnist.py
