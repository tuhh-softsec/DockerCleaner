#   Tag: nvidia/cuda:9.0-cudnn7-devel-ubuntu16.04
#   Label: com.nvidia.cuda.version: 9.0.176
#   Label: com.nvidia.cudnn.version: 7.1.2.21
#   Label: com.nvidia.volumes.needed: nvidia_driver
#   Label: maintainer: NVIDIA CORPORATION <cudatools@nvidia.com>
#   Ubuntu 16.04
FROM nvidia/cuda@sha256:40db1c98b66e133f54197ba1a66312b9c29842635c8cba5ae66fb56ded695b7c
ENV TENSORFLOW_VERSION="1.12.0"
ENV HADOOP_VERSION="2.7.2"
LABEL HADOOP_VERSION="2.7.2"
RUN apt-get update -y \
 && apt-get install --no-install-recommends nano=7.2-1 vim=2:9.0.1000-4ubuntu2 joe=4.6-1build2 wget=1.21.3-1ubuntu1 curl=7.88.1-7ubuntu1 jq=1.6-2.1ubuntu3 gawk=1:5.2.1-2 psmisc=23.6-1 python python-dev python-pip python3=3.11.2-1 python3-dev=3.11.2-1 python3-pip=23.0.1+dfsg-1 python-six build-essential=12.9ubuntu3 automake=1:1.16.5-1.3 openjdk-8-jdk=8u362-ga-0ubuntu2 lsof=4.95.0-1 libcupti-dev=11.8.87~11.8.0-2ubuntu1 openssh-server=1:9.0p1-1ubuntu8 openssh-client=1:9.0p1-1ubuntu8 build-essential=12.9ubuntu3 autotools-dev=20220109.1 cmake=3.25.1-1 git=1:2.39.2-1ubuntu1 bash-completion=1:2.11-6ubuntu1 ca-certificates=20230311 inotify-tools=3.22.6.0-4 rsync=3.2.7-1 realpath libjpeg-dev=8c-2ubuntu11 libpng-dev=1.6.39-2 net-tools=2.10-0.1ubuntu3 libsm6=2:1.2.3-1build2 libxext6=2:1.3.4-1build1 rpm=4.18.0+dfsg-1build1 cpio=2.13+dfsg-7.1 net-tools=2.10-0.1ubuntu3 libdapl2=2.1.10.1.f1e05b7a-3 dapl2-utils=2.1.10.1.f1e05b7a-3 libmlx4-1 libmlx5-1 ibutils=1.5.7+0.2.gbd7e502-3 librdmacm1=44.0-2 libibverbs1=44.0-2 libmthca1 ibverbs-utils=44.0-2 rdmacm-utils=44.0-2 perftest=4.5+0.17-1 kmod=30+20221128-1ubuntu1 -y
#   Prepare Hadoop 2.7.2
RUN wget -qO- http://archive.apache.org/dist/hadoop/common/hadoop-${HADOOP_VERSION}/hadoop-${HADOOP_VERSION}.tar.gz | tar xz -C /usr/local \
 && mv /usr/local/hadoop-${HADOOP_VERSION} /usr/local/hadoop
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64" \
    HADOOP_INSTALL="/usr/local/hadoop" \
    NVIDIA_VISIBLE_DEVICES="all"
ENV HADOOP_PREFIX="${HADOOP_INSTALL}" \
    HADOOP_BIN_DIR="${HADOOP_INSTALL}/bin" \
    HADOOP_SBIN_DIR="${HADOOP_INSTALL}/sbin" \
    HADOOP_HDFS_HOME="${HADOOP_INSTALL}" \
    HADOOP_COMMON_LIB_NATIVE_DIR="${HADOOP_INSTALL}/lib/native" \
    HADOOP_OPTS="-Djava.library.path=${HADOOP_INSTALL}/lib/native"
ENV PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:${HADOOP_BIN_DIR}:${HADOOP_SBIN_DIR}" \
    LD_LIBRARY_PATH="/usr/local/cuda/extras/CUPTI/lib:/usr/local/cuda/extras/CUPTI/lib64:/usr/local/nvidia/lib:/usr/local/nvidia/lib64:/usr/local/cuda/lib64:/usr/local/cuda/targets/x86_64-linux/lib/stubs:${JAVA_HOME}/jre/lib/amd64/server"
#   Install NCCL v2.3.7, for CUDA 9.0
RUN wget https://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu1604/x86_64/nvidia-machine-learning-repo-ubuntu1604_1.0.0-1_amd64.deb \
 && dpkg -i nvidia-machine-learning-repo-ubuntu1604_1.0.0-1_amd64.deb \
 && apt-get install --no-install-recommends libnccl2=2.3.7-1+cuda9.0 libnccl-dev=2.3.7-1+cuda9.0
#   Install intel MPI with the version which azure suggests.
COPY silent.cfg /silent.cfg
ENV MANPATH="/usr/share/man:/usr/local/man" \
    COMPILERVARS_ARCHITECTURE="intel64" \
    COMPILERVARS_PLATFORM="linux" \
    INTEL_MPI_PATH="/opt/intel/compilers_and_libraries/linux/mpi"
#   Install Intel MPI in the Docker Image.
#   You should prepare your own intel mpi license to active your intel MPI, and modify the file silent.cfg to set the configuration of activation type.
RUN wget http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/9278/l_mpi_p_5.1.3.223.tgz \
 && tar -xvf /l_mpi_p_5.1.3.223.tgz \
 && cd /l_mpi_p_5.1.3.223 \
 && ./install.sh -s /silent.cfg \
 && . /opt/intel/bin/compilervars.sh \
 && . /opt/intel/compilers_and_libraries/linux/mpi/bin64/mpivars.sh \
 && echo "source /opt/intel/compilers_and_libraries/linux/mpi/bin64/mpivars.sh" >> /root/.bashrc \
 && echo LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:'$LD_LIBRARY_PATH' >> /root/.bashrc
ENV PATH="$PATH:/opt/intel/compilers_and_libraries/linux/mpi/bin64"
#   Install TensorFlow
RUN pip3 install tensorflow-gpu==${TENSORFLOW_VERSION} h5py \
 && pip install h5py==3.8.0 tensorflow-gpu==${TENSORFLOW_VERSION}
#   Install Dependencies
RUN pip3 install --no-cache-dir scipy jupyter ipykernel numpy toolz pandas scikit-learn pillow \
 && pip install scipy==1.10.1 numpy==1.24.2 toolz==0.12.0 pandas==2.0.0 scikit-learn==1.2.2 pillow==9.5.0 --no-cache-dir
#   Install Horovod, temporarily using CUDA stubs
RUN ldconfig /usr/local/cuda-9.0/targets/x86_64-linux/lib/stubs \
 && /bin/bash -c "source /opt/intel/compilers_and_libraries/linux/mpi/intel64/bin/mpivars.sh" \
 && pip3 install --no-cache-dir horovod==0.15.2 \
 && pip install horovod==0.15.2 --no-cache-dir \
 && ldconfig
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
