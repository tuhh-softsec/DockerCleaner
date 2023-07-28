FROM nvidia/cuda:8.0-cudnn6-devel-ubuntu16.04
RUN :
#   disable interactive functions
ENV DEBIAN_FRONTEND="noninteractive"
#  ################Install MiniConda and other dependencies##########
ENV CONDA_DIR="/opt/conda"
ENV PATH="$CONDA_DIR/bin:$PATH"
ENV OPENBLAS_NUM_THREADS="$(nproc)"
RUN mkdir -p $CONDA_DIR \
 && echo export PATH=$CONDA_DIR/bin:'$PATH' > /etc/profile.d/conda.sh \
 && apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends wget vim git g++ graphviz software-properties-common python-software-properties python3-dev libhdf5-dev libopenblas-dev liblapack-dev libblas-dev gfortran cpio libmlx4-1 libmlx5-1 librdmacm1 libibverbs1 libmthca1 libdapl2 dapl2-utils -y ) \
 && rm -rf /var/lib/apt/lists/* \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh \
 && /bin/bash /Miniconda3-latest-Linux-x86_64.sh -f -b -p $CONDA_DIR \
 && rm Miniconda3-latest-Linux-x86_64.sh
#  ######################## INTEL MPI ###########################
RUN cd /tmp \
 && wget -q 'http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/11595/l_mpi_2017.3.196.tgz' \
 && tar zxvf l_mpi_2017.3.196.tgz \
 && sed -i -e 's/^ACCEPT_EULA=decline/ACCEPT_EULA=accept/g' /tmp/l_mpi_2017.3.196/silent.cfg \
 && sed -i -e 's|^#ACTIVATION_LICENSE_FILE=|ACTIVATION_LICENSE_FILE=/tmp/l_mpi_2017.3.196/USE_SERVER.lic|g' /tmp/l_mpi_2017.3.196/silent.cfg \
 && sed -i -e 's/^ACTIVATION_TYPE=exist_lic/ACTIVATION_TYPE=license_server/g' /tmp/l_mpi_2017.3.196/silent.cfg \
 && cd /tmp/l_mpi_2017.3.196 \
 && ./install.sh -s silent.cfg \
 && echo "source /opt/intel/compilers_and_libraries_2017.4.196/linux/mpi/intel64/bin/mpivars.sh" >> ~/.bashrc
#  ######################NCCL###########################
ENV CPATH="/usr/local/cuda/include:/usr/local/include:$CPATH"
RUN cd /usr/local \
 && git clone https://github.com/NVIDIA/nccl.git \
 && cd nccl \
 && make CUDA_HOME=/usr/local/cuda -j"$( nproc ;)" \
 && make install \
 && ldconfig
#  ###################Python 3#########################
ARG python_version=3.5.2
RUN conda install -y python=${python_version} \
 && pip install pip==23.1 -U \
 && . /opt/intel/compilers_and_libraries_2017.4.196/linux/mpi/intel64/bin/mpivars.sh \
 && conda install Pillow scikit-learn notebook pandas matplotlib mkl nose pyyaml six h5py \
 && pip install cupy==12.0.0 \
 && pip install mpi4py==3.1.4 \
 && pip install cython==0.29.34 \
 && pip install chainer==7.8.1 \
 && pip install chainercv==0.13.1 \
 && pip install chainermn==1.3.1 \
 && conda clean -yt
ENV PYTHONPATH="$CONDA_DIR/lib/python3.5/site-packages/:$PYTHONPATH"
#  #####################################################
ENV PYTHONPATH="/src/:$PYTHONPATH"
#  ################ remove Intel components #####################
RUN rm -rf /opt/intel
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
