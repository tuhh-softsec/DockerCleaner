#   Authors:
#   Guilherme Caminha <gpkc@cin.ufpe.br>
FROM phusion/baseimage
MAINTAINER Guilherme Caminha <gpkc@cin.ufpe.br>
ENV HOME="/home/scientific"
ENV MOAB_DIR="/usr"
ENV VISIT_DIR="/opt/visit"
ENV MPI_HOME="/usr"
ENV PYTHONPATH="/usr/lib/python3.6/site-packages"
ENV PATH="/usr/local/bin:$PATH"
ENV LANG="C.UTF-8"
RUN set -x \
 && apt-get update \
 && apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 pkg-config=1.8.1-1ubuntu2 git=1:2.39.2-1ubuntu1 libopenblas-dev=0.3.21+ds-4 liblapack-dev=3.11.0-2 make=4.3-4.1build1 cmake=3.25.1-1 autoconf=2.71-3 automake=1:1.16.5-1.3 libtool=2.4.7-5 clang=1:15.0-56~exp2 gcc=4:12.2.0-3ubuntu1 g++=4:12.2.0-3ubuntu1 gfortran=4:12.2.0-3ubuntu1 libhdf5-mpich-dev=1.10.8+repack1-1ubuntu1 libnetcdf-c++4=4.2-13 libeigen3-dev=3.4.0-4 libmetis-dev=5.1.0.dfsg-7build2 doxygen=1.9.4-4 liboce-foundation-dev=0.18.3-2 liboce-modeling-dev=0.18.3-2 liboce-ocaf-dev=0.18.3-2 liboce-visualization-dev=0.18.3-2 oce-draw=0.18.3-2 netgen=6.2.2006+really6.2.1905+dfsg-5.1build1 libnglib-dev=6.2.2006+really6.2.1905+dfsg-5.1build1 build-essential=12.9ubuntu3 bzip2=1.0.8-5build1 tar=1.34+dfsg-1.1 m4=1.4.19-3 file=1:5.44-3 swig=4.1.0-0.2 tcl=8.6.13 tk=8.6.13 libssl-dev=3.0.8-1ubuntu1 -qy \
 && apt-get clean
ENV GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D"
ENV PYTHON_VERSION="3.6.3"
#   Install Python3
RUN set -ex \
 && buildDeps=' dpkg-dev tcl-dev tk-dev ' \
 && apt-get update \
 && apt-get install --no-install-recommends $buildDeps -y \
 && rm -rf /var/lib/apt/lists/* \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && rm -rf "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --enable-loadable-sqlite-extensions --enable-shared --with-system-expat --with-system-ffi --without-ensurepip \
 && make -j "$( nproc ;)" \
 && make install \
 && ldconfig \
 && apt-get purge -y --auto-remove $buildDeps \
 && find /usr/local -depth
#   make some useful symlinks that are expected to exist
RUN cd /usr/local/bin \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="9.0.1"
RUN set -ex ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
CMD ["/sbin/my_init"]
RUN pip install cython==0.29.34 numpy==1.24.2 pytest==7.3.1 colorlog==6.7.0 configobj==5.0.8 pytest-runner==6.0.0
#   Install mpi4py
WORKDIR $HOME
RUN cd $HOME \
 && git clone https://bitbucket.org/mpi4py/mpi4py \
 && cd $HOME/mpi4py \
 && python setup.py build \
 && python setup.py install \
 && cd $HOME \
 && rm -rf mpi4py
#   Install MOAB
WORKDIR $HOME
RUN cd $HOME \
 && git clone https://bitbucket.org/fathomteam/moab.git \
 && cd $HOME/moab \
 && autoreconf -fi \
 && mkdir build \
 && cd $HOME/moab/build \
 && ../configure --prefix=$MOAB_DIR --with-mpi=/usr --with-hdf5=/usr/lib/x86_64-linux-gnu/hdf5/mpich --with-netcdf=/usr --with-metis=/usr --enable-optimize --enable-debug --enable-tools --enable-pymoab --enable-shared CFLAGS='-O2 -fPIC -DPIC' CXXFLAGS='-O2 -fPIC -DPIC' FCFLAGS='-O2 -fPIC' FFLAGS='-O2 -fPIC' \
 && make -j24 \
 && make install \
 && cd $HOME/moab/build/pymoab \
 && python setup.py build \
 && python setup.py install \
 && cd $HOME \
 && rm -rf moab \
 && echo "export MOAB_DIR=$MOAB_DIR" >> $HOME/.bashrc \
 && echo "export PATH=$PATH:$MOAB_DIR/bin" >> $HOME/.bashrc \
 && echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MOAB_DIR/lib" >> $HOME/.bashrc
#   Install Trilinos
ENV TRILINOS_VER="12.12.1"
ENV CPATH="/usr/local/include/python3.6m:$CPATH"
RUN cd $HOME \
 && wget http://www.cmake.org/files/v3.2/cmake-3.2.2.tar.gz \
 && tar xzvf cmake-3.2.2.tar.gz \
 && cd $HOME/cmake-3.2.2 \
 && ./configure --prefix=/usr/local/cmake \
 && make -j24 \
 && make install \
 && cd $HOME \
 && rm -rf cmake-3.2.2 \
 && rm -f cmake-3.2.2.tar.gz \
 && wget http://trilinos.csbsju.edu/download/files/trilinos-$TRILINOS_VER-Source.tar.bz2 \
 && tar xjvf trilinos-$TRILINOS_VER-Source.tar.bz2 \
 && mkdir trilinos-$TRILINOS_VER-Source/build \
 && cd $HOME/trilinos-$TRILINOS_VER-Source/build/ \
 && /usr/local/cmake/bin/cmake -D CMAKE_INSTALL_PREFIX:PATH=/usr -D MPI_BASE_DIR:PATH=/usr -D CMAKE_BUILD_TYPE:STRING=Release -D CMAKE_Fortran_COMPILER:FILEPATH=/usr/bin/mpif90 -D BUILD_SHARED_LIBS:BOOL=ON -D Trilinos_WARNINGS_AS_ERRORS_FLAGS:STRING="" -D Trilinos_ENABLE_ALL_PACKAGES:BOOL=OFF -D Trilinos_ENABLE_ALL_OPTIONAL_PACKAGES:BOOL=OFF -D Trilinos_ENABLE_ALL_FORWARD_DEP_PACKAGES:BOOL=OFF -D Trilinos_ENABLE_Teuchos:BOOL=ON -D Trilinos_ENABLE_Epetra:BOOL=ON -D Trilinos_ENABLE_AztecOO:BOOL=ON -D Trilinos_ENABLE_Amesos:BOOL=ON -D Trilinos_ENABLE_PyTrilinos:BOOL=ON -D PyTrilinos_DISABLE_STRONG_WARN:BOOL=OFF -D PyTrilinos_DOCSTRINGS:BOOL=OFF -D PyTrilinos_ENABLE_EXAMPLES:BOOL=OFF -D PyTrilinos_ENABLE_TESTS:BOOL=OFF -D Trilinos_ENABLE_EXAMPLES:BOOL=OFF -D Trilinos_ENABLE_TESTS:BOOL=OFF -D TPL_ENABLE_MATLAB:BOOL=OFF -D TPL_ENABLE_Matio:BOOL=OFF -D TPL_ENABLE_MPI:BOOL=ON -D TPL_ENABLE_BLAS:BOOL=ON -D TPL_ENABLE_LAPACK:BOOL=ON -D TPL_ENABLE_QT:BOOL=OFF -D TPL_ENABLE_X11:BOOL=OFF -D TPL_ENABLE_MPI4PY:BOOL=ON -D CMAKE_VERBOSE_MAKEFILE:BOOL=OFF -D Trilinos_VERBOSE_CONFIGURE:BOOL=OFF .. \
 && make -j24 \
 && cd $HOME/trilinos-$TRILINOS_VER-Source/build/packages/PyTrilinos/src/PyTrilinos \
 && python -m compileall -b -l -f . \
 && cd $HOME/trilinos-$TRILINOS_VER-Source/build \
 && make install \
 && cd $HOME \
 && rm -rf trilinos-$TRILINOS_VER-Source trilinos-$TRILINOS_VER-Source.tar.bz2
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
