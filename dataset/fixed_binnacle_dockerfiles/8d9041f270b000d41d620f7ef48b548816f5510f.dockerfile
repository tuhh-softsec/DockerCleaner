#   Dockerfile describing development environments and builds of FEniCS-X
#
#   Authors: Jack S. Hale <jack.hale@uni.lu> Lizao Li
#   <lzlarryli@gmail.com> Garth N. Wells <gnw20@cam.ac.uk> Jan Blechta
#   <blechta@karlin.mff.cuni.cz>
#
#   All layers are built bi-weekly on CircleCI and pushed to
#   https://quay.io/repository/fenicsproject/dolfinx
#
#   To build development environment images:
#
#      docker build --target dev-env-complex -t quay.io/fenicsproject/dolfinx:dev-env-complex .
#      docker build --target dev-env-real -t quay.io/fenicsproject/dolfinx:dev-env-real .
#
#   To run a notebook:
#
#      docker run -p 8888:8888 quay.io/fenicsproject/dolfinx:notebook
#
#   To run and share the current host directory with the container:
#
#      docker run -p 8888:8888 -v "$(pwd)":/tmp quay.io/fenicsproject/dolfinx:notebook
#
ARG GMSH_VERSION=4.3.0
ARG PYBIND11_VERSION=2.3.0
ARG PETSC_VERSION=3.11.2
ARG SLEPC_VERSION=3.11.1
ARG PETSC4PY_VERSION=3.11.0
ARG SLEPC4PY_VERSION=3.11.0
ARG TINI_VERSION=v0.18.0
ARG MAKEFLAGS
ARG PETSC_SLEPC_OPTFLAGS="-02 -g"
ARG PETSC_SLEPC_DEBUGGING="yes"
FROM ubuntu:18.04 AS base
LABEL maintainer="fenics-project <fenics-support@googlegroups.org>"
LABEL description="Base image for real and complex FEniCS test environments"
ARG GMSH_VERSION
ARG PYBIND11_VERSION
WORKDIR /tmp
#   Environment variables
ENV OPENBLAS_NUM_THREADS="1" \
    OPENBLAS_VERBOSE="0"
#   Install dependencies available via apt-get.
#   - First set of packages are required to build and run FEniCS.
#   - Second set of packages are recommended and/or required to build
#     documentation or tests.
#   - Third set of packages are optional, but required to run gmsh
#     pre-built binaries.
#   - Fourth set of packages are optional, required for meshio.
RUN export DEBIAN_FRONTEND=noninteractive \
 && apt-get update -qq \
 && apt-get -yq --with-new-pkgs -o Dpkg::Options::="--force-confold" upgrade \
 && apt-get install --no-install-recommends cmake=3.10.2-1ubuntu2.18.04.2 g++=4:7.4.0-1ubuntu2.3 gfortran=4:7.4.0-1ubuntu2.3 libboost-dev=1.65.1.0ubuntu1 libboost-filesystem-dev=1.65.1.0ubuntu1 libboost-iostreams-dev=1.65.1.0ubuntu1 libboost-math-dev=1.65.1.0ubuntu1 libboost-program-options-dev=1.65.1.0ubuntu1 libboost-system-dev=1.65.1.0ubuntu1 libboost-thread-dev=1.65.1.0ubuntu1 libboost-timer-dev=1.65.1.0ubuntu1 libeigen3-dev=3.3.4-4 libhdf5-mpich-dev=1.10.0-patch1+docs-4 liblapack-dev=3.7.1-4ubuntu1 libmpich-dev=3.3~a2-4 libopenblas-dev=0.2.20+ds-4 mpich=3.3~a2-4 ninja-build=1.8.2-1 pkg-config=0.29.1-0ubuntu2 python3-dev=3.6.7-1~18.04 python3-matplotlib=2.1.1-2ubuntu3 python3-numpy=1:1.13.3-2ubuntu1 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-scipy=0.19.1-2ubuntu1 python3-setuptools=39.0.1-2ubuntu0.1 -y \
 && apt-get install --no-install-recommends doxygen=1.8.13-10 git=1:2.17.1-1ubuntu0.17 graphviz=2.40.1-2 valgrind=1:3.13.0-2ubuntu2.3 wget=1.19.4-1ubuntu2.2 -y \
 && apt-get install --no-install-recommends libglu1 libxcursor-dev=1:1.1.15-1 libxinerama1=2:1.1.3-1 -y \
 && apt-get install --no-install-recommends python3-lxml=4.2.1-1ubuntu0.6 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Download Install Gmsh SDK
RUN cd /usr/local \
 && wget -nc --quiet http://gmsh.info/bin/Linux/gmsh-${GMSH_VERSION}-Linux64-sdk.tgz \
 && tar -xf gmsh-${GMSH_VERSION}-Linux64-sdk.tgz
ENV PATH="/usr/local/gmsh-${GMSH_VERSION}-Linux64-sdk/bin:$PATH"
#   Add gmsh python API
ENV PYTHONPATH="/usr/local/gmsh-${GMSH_VERSION}-Linux64-sdk/lib"
#   Install Python packages (via pip)
#   - First set of packages are required to build and run FEniCS.
#   - Second set of packages are recommended and/or required to build
#     documentation or run tests.
#   - Third set of packages are optional but required for
#     pygmsh/meshio/DOLFIN mesh pipeline.
RUN pip3 install --no-cache-dir mpi4py numba \
 && pip3 install --no-cache-dir cffi decorator flake8 pytest pytest-xdist sphinx sphinx_rtd_theme \
 && export HDF5_MPI="ON" \
 && pip3 install --no-cache-dir --no-binary=h5py h5py meshio pygmsh
#   Install pybind11
RUN wget -nc --quiet https://github.com/pybind/pybind11/archive/v${PYBIND11_VERSION}.tar.gz \
 && tar -xf v${PYBIND11_VERSION}.tar.gz \
 && cd pybind11-${PYBIND11_VERSION} \
 && mkdir build \
 && cd build \
 && cmake -DPYBIND11_TEST=False ../ \
 && make install \
 && rm -rf /tmp/*
WORKDIR /root
#  #######################################
FROM base AS dev-env-real
LABEL maintainer="fenics-project <fenics-support@googlegroups.org>"
LABEL description="FEniCS development environment with PETSc real mode"
ARG PETSC_VERSION
ARG PETSC4PY_VERSION
ARG SLEPC_VERSION
ARG SLEPC4PY_VERSION
ARG MAKEFLAGS
ARG PETSC_SLEPC_OPTFLAGS
ARG PETSC_SLEPC_DEBUGGING
WORKDIR /tmp
#   Install PETSc and SLEPc with real types.
RUN apt-get update -qq \
 && apt-get install --no-install-recommends bison=2:3.0.4.dfsg-1build1 flex=2.6.4-6 python=2.7.15~rc1-1 -y \
 && wget -nc --quiet https://bitbucket.org/petsc/petsc/get/v${PETSC_VERSION}.tar.gz -O petsc-${PETSC_VERSION}.tar.gz \
 && mkdir -p petsc-src \
 && tar -xf petsc-${PETSC_VERSION}.tar.gz -C petsc-src --strip-components 1 \
 && cd petsc-src \
 && ./configure --COPTFLAGS=${PETSC_SLEPC_OPTFLAGS} --CXXOPTFLAGS=${PETSC_SLEPC_OPTFLAGS} --FOPTFLAGS=${PETSC_SLEPC_OPTFLAGS} --with-debugging=${PETSC_SLEPC_DEBUGGING} --with-fortran-bindings=no --download-blacs --download-hypre --download-metis --download-mumps --download-ptscotch --download-scalapack --download-spai --download-suitesparse --download-superlu --with-scalar-type=real --prefix=/usr/local/petsc \
 && make ${MAKEFLAGS} \
 && make install \
 && export PETSC_DIR=/usr/local/petsc \
 && cd /tmp \
 && wget -nc --quiet http://slepc.upv.es/download/distrib/slepc-${SLEPC_VERSION}.tar.gz -O slepc-${SLEPC_VERSION}.tar.gz \
 && mkdir -p slepc-src \
 && tar -xf slepc-${SLEPC_VERSION}.tar.gz -C slepc-src --strip-components 1 \
 && cd slepc-src \
 && ./configure --prefix=/usr/local/slepc \
 && make ${MAKEFLAGS} \
 && make install \
 && apt-get -y purge bison flex python \
 && apt-get -y autoremove \
 && apt-get clean \
 && rm -rf /tmp/* \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
ENV PETSC_DIR="/usr/local/petsc" \
    SLEPC_DIR="/usr/local/slepc"
#   Install petsc4py and slepc4py
RUN pip3 install --no-cache-dir petsc4py==${PETSC4PY_VERSION} \
 && pip3 install --no-cache-dir slepc4py==${SLEPC4PY_VERSION}
WORKDIR /root
#  #######################################
FROM base AS dev-env-complex
LABEL description="FEniCS development environment with PETSc complex mode"
ARG PETSC_VERSION
ARG PETSC4PY_VERSION
ARG SLEPC_VERSION
ARG SLEPC4PY_VERSION
ARG MAKEFLAGS
ARG PETSC_SLEPC_OPTFLAGS
ARG PETSC_SLEPC_DEBUGGING
WORKDIR /tmp
#   Install PETSc and SLEPc with complex scalar types
RUN apt-get update -qq \
 && apt-get install --no-install-recommends bison=2:3.0.4.dfsg-1build1 flex=2.6.4-6 python=2.7.15~rc1-1 -y \
 && wget -nc --quiet https://bitbucket.org/petsc/petsc/get/v${PETSC_VERSION}.tar.gz -O petsc-${PETSC_VERSION}.tar.gz \
 && mkdir -p petsc-src \
 && tar -xf petsc-${PETSC_VERSION}.tar.gz -C petsc-src --strip-components 1 \
 && cd petsc-src \
 && ./configure --COPTFLAGS=${PETSC_SLEPC_OPTFLAGS} --CXXOPTFLAGS=${PETSC_SLEPC_OPTFLAGS} --FOPTFLAGS=${PETSC_SLEPC_OPTFLAGS} --with-debugging=${PETSC_SLEPC_DEBUGGING} --with-fortran-bindings=no --download-blacs --download-metis --download-mumps --download-ptscotch --download-scalapack --download-suitesparse --download-superlu --with-scalar-type=complex --prefix=/usr/local/petsc \
 && make ${MAKEFLAGS} \
 && make install \
 && export PETSC_DIR=/usr/local/petsc \
 && cd /tmp \
 && wget -nc --quiet http://slepc.upv.es/download/distrib/slepc-${SLEPC_VERSION}.tar.gz -O slepc-${SLEPC_VERSION}.tar.gz \
 && mkdir -p slepc-src \
 && tar -xf slepc-${SLEPC_VERSION}.tar.gz -C slepc-src --strip-components 1 \
 && cd slepc-src \
 && ./configure --prefix=/usr/local/slepc \
 && make ${MAKEFLAGS} \
 && make install \
 && apt-get -y purge bison flex python \
 && apt-get -y autoremove \
 && apt-get clean \
 && rm -rf /tmp/* \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
ENV PETSC_DIR="/usr/local/petsc" \
    SLEPC_DIR="/usr/local/slepc"
#   Install complex petsc4py and slepc4py
RUN pip3 install --no-cache-dir petsc4py==${PETSC4PY_VERSION} \
 && pip3 install --no-cache-dir slepc4py==${SLEPC4PY_VERSION}
WORKDIR /root
#  #######################################
FROM dev-env-real AS real
LABEL description="DOLFIN-X in real mode"
ARG MAKEFLAGS
WORKDIR /tmp
#   Install ipython (optional), FIAT, UFL and ffcX (development
#   versions, master branch)
RUN pip3 install --no-cache-dir ipython \
 && pip3 install --no-cache-dir git+https://bitbucket.org/fenics-project/fiat.git \
 && pip3 install --no-cache-dir git+https://bitbucket.org/fenics-project/ufl.git \
 && pip3 install --no-cache-dir git+https://github.com/fenics/ffcX
#   Install dolfinx
RUN git clone https://github.com/fenics/dolfinx.git \
 && cd dolfinx \
 && mkdir build \
 && cd build \
 && cmake -G Ninja ../cpp \
 && ninja ${MAKEFLAGS} install \
 && cd ../python \
 && pip3 install . \
 && rm -rf /tmp/*
WORKDIR /root
#  #######################################
FROM dev-env-complex AS complex
LABEL description="DOLFIN-X in complex mode"
ARG MAKEFLAGS
WORKDIR /tmp
#   Install ipython (optional), FIAT, UFL and ffcX (development versions,
#   master branch)
RUN pip3 install --no-cache-dir ipython \
 && pip3 install --no-cache-dir git+https://bitbucket.org/fenics-project/fiat.git \
 && pip3 install --no-cache-dir git+https://bitbucket.org/fenics-project/ufl.git \
 && pip3 install --no-cache-dir git+https://github.com/fenics/ffcX
#   Install dolfinx
RUN git clone https://github.com/fenics/dolfinx.git \
 && cd dolfinx \
 && mkdir build \
 && cd build \
 && cmake -G Ninja ../cpp \
 && ninja ${MAKEFLAGS} install \
 && cd ../python \
 && pip3 install . \
 && rm -rf /tmp/*
WORKDIR /root
#  #######################################
FROM real AS notebook
LABEL description="DOLFIN-X Jupyter Notebook"
WORKDIR /root
ARG TINI_VERSION
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tini https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini
RUN chmod +x /tini \
 && pip3 install --no-cache-dir jupyter jupyterlab
ENTRYPOINT ["/tini", "--", "jupyter", "notebook", "--ip", "0.0.0.0", "--no-browser", "--allow-root"]
#  #######################################
FROM complex AS notebook-complex
LABEL description="DOLFIN-X (complex mode) Jupyter Notebook"
ARG TINI_VERSION
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tini https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini
RUN chmod +x /tini \
 && pip3 install --no-cache-dir jupyter jupyterlab
WORKDIR /root
ENTRYPOINT ["/tini", "--", "jupyter", "notebook", "--ip", "0.0.0.0", "--no-browser", "--allow-root"]
#  #######################################
FROM notebook AS lab
LABEL description="DOLFIN-X Jupyter Lab"
WORKDIR /root
ENTRYPOINT ["/tini", "--", "jupyter", "lab", "--ip", "0.0.0.0", "--no-browser", "--allow-root"]
#  #######################################
FROM notebook-complex AS lab-complex
LABEL description="DOLFIN-X (complex mode) Jupyter Lab"
WORKDIR /root
ENTRYPOINT ["/tini", "--", "jupyter", "lab", "--ip", "0.0.0.0", "--no-browser", "--allow-root"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
