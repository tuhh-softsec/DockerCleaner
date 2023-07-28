FROM gzynda/tacc-ubuntu18-mvapich2.3psm2:stampede2
MAINTAINER https://github.com/underworldcode/
#   install things
RUN : \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq --no-install-recommends build-essential git python-dev python-pip libxml2-dev xorg-dev libfreetype6-dev libpng-dev libxft-dev xvfb freeglut3 freeglut3-dev libgl1-mesa-dri libgl1-mesa-glx xauth swig wget cmake gdb \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
ENV LANG="C.UTF-8"
#   Install setuptools and wheel first, needed by plotly
RUN pip install setuptools==67.6.1 -U \
 && pip install wheel==0.40.0 -U \
 && pip install packaging==23.1 appdirs==1.4.4 numpy==1.24.2 jupyter==1.0.0 matplotlib==3.7.1 pillow==9.5.0 pyvirtualdisplay==3.0 ipython==8.12.0 pint==0.20.1 scipy==1.10.1 tabulate==0.9.0 mpi4py==3.1.4 wget==3.2 --no-cache-dir
WORKDIR /tmp/petsc-build
RUN wget http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/petsc-lite-3.9.3.tar.gz \
 && tar zxf petsc-lite-3.9.3.tar.gz \
 && cd petsc-3.9.3 \
 && ./configure --with-debugging=0 --prefix=/usr --COPTFLAGS="-g -O3" --CXXOPTFLAGS="-g -O3" --FOPTFLAGS="-g -O3" --with-zlib=1 --download-fblaslapack=1 --download-hdf5=1 --download-mumps=1 --download-parmetis=1 --download-metis=1 --download-superlu=1 --download-hypre=1 --download-scalapack=1 --download-superlu_dist=1 --download-superlu=1 \
 && make PETSC_DIR=/tmp/petsc-build/petsc-3.9.3 PETSC_ARCH=arch-linux2-c-opt all \
 && make PETSC_DIR=/tmp/petsc-build/petsc-3.9.3 PETSC_ARCH=arch-linux2-c-opt install \
 && cd /tmp \
 && rm -fr *
ENV PYTHONPATH="$PYTHONPATH:/usr/lib"
RUN CC=h5pcc HDF5_MPI="ON" pip install --no-cache-dir --no-binary=h5py h5py
ENV NB_WORK="/workspace"
#   create a volume
VOLUME $NB_WORK/user_data
WORKDIR $NB_WORK
WORKDIR /opt
ENV UW2_DIR="/opt/underworld2"
RUN mkdir $UW2_DIR
ENV PYTHONPATH="$PYTHONPATH:$UW2_DIR"
COPY . $UW2_DIR/
#   get underworld, compile, delete some unnecessary files, trust notebooks, copy to workspace
RUN cd /opt/underworld2/libUnderworld \
 && ./configure.py --with-debugging=0 \
 && ./compile.py \
 && rm -fr h5py_ext \
 && rm .sconsign.dblite \
 && rm -fr .sconf_temp \
 && cd build \
 && rm -fr libUnderworldPy \
 && rm -fr StGermain \
 && rm -fr gLucifer \
 && rm -fr Underworld \
 && rm -fr StgFEM \
 && rm -fr StgDomain \
 && rm -fr PICellerator \
 && rm -fr Solvers
RUN pip install lavavu==1.8.45 --no-cache-dir
ENV GLUCIFER_USE_XVFB="1"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
