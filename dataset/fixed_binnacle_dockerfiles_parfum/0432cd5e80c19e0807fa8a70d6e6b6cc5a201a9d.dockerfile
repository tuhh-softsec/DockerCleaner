FROM ubuntu:16.04
LABEL maintainer="Cong Xu <cong.xu@intel.com>"
#  This parameter MUST be set by parameterized_docker_build.sh
ARG TF_WHL_URL
#  Optional parameters
ARG TF_BUILD_VERSION=r1.11
ARG PYTHON="python"
ARG PYTHON_DEV="python-dev"
ARG PIP="pip"
#  Pick up some TF dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential curl libfreetype6-dev libhdf5-serial-dev libpng12-dev libzmq3-dev pkg-config python rsync software-properties-common unzip wget libnuma-dev openssh-client openssh-server ${PYTHON_DEV} -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN curl -O https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && rm get-pip.py
RUN ${PIP} --no-cache-dir install Pillow h5py ipykernel jupyter keras_applications keras_preprocessing matplotlib numpy pandas scipy sklearn \
 && python -m ipykernel.kernelspec
COPY ${TF_WHL_URL} /
RUN ${PIP} install --no-cache-dir --force-reinstall /${TF_WHL_URL} \
 && rm -rf /${TF_WHL_URL}
RUN if [ "${PYTHON}" = "python3" ] ; then ln -s -f /usr/bin/python3 /usr/bin/python ; fi
#  Set up our notebook config.
COPY jupyter_notebook_config.py /root/.jupyter/
#  Copy sample notebooks.
COPY notebooks /notebooks
#  Jupyter has issues with being run directly:
#    https://github.com/ipython/ipython/issues/7062
#  We just add a little wrapper script.
COPY run_jupyter.sh /
WORKDIR /root
#  Install Open MPI
RUN mkdir /tmp/openmpi \
 && cd /tmp/openmpi \
 && wget https://www.open-mpi.org/software/ompi/v3.0/downloads/openmpi-3.0.0.tar.gz \
 && tar zxf openmpi-3.0.0.tar.gz \
 && cd openmpi-3.0.0 \
 && ./configure --enable-orterun-prefix-by-default \
 && make -j $( nproc ;) all \
 && make install \
 && ldconfig \
 && rm -rf /tmp/openmpi
#  Create a wrapper for OpenMPI to allow running as root by default
RUN mv /usr/local/bin/mpirun /usr/local/bin/mpirun.real \
 && echo '#!/bin/bash' > /usr/local/bin/mpirun \
 && echo 'mpirun.real --allow-run-as-root "$@"' >> /usr/local/bin/mpirun \
 && chmod a+x /usr/local/bin/mpirun
#  Configure OpenMPI to run good defaults:
RUN echo "btl_tcp_if_exclude = lo,docker0" >> /usr/local/etc/openmpi-mca-params.conf
#  Install Horovod
RUN ${PIP} install --no-cache-dir horovod
#  Install OpenSSH for MPI to communicate between containers
RUN mkdir -p /var/run/sshd
#  Allow OpenSSH to talk to containers without asking for confirmation
RUN cat /etc/ssh/ssh_config | grep -v StrictHostKeyChecking > /etc/ssh/ssh_config.new \
 && echo " StrictHostKeyChecking no" >> /etc/ssh/ssh_config.new \
 && mv /etc/ssh/ssh_config.new /etc/ssh/ssh_config
#  TensorBoard
EXPOSE 6006/tcp
#  IPython
EXPOSE 8888/tcp
WORKDIR "/notebooks"
CMD ["/run_jupyter.sh", "--allow-root"]
