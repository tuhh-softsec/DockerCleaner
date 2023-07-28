#   This Dockerfile has been adapted from the original work here
#   https://github.com/gpuopenanalytics/pygdf
#   An integration test & dev container which builds and installs libgdf & pygdf from master
FROM nvidia/cuda-ppc64le:9.2-cudnn7-devel-ubuntu16.04
MAINTAINER "Priya Seth <sethp@us.ibm.com>"
ARG CC_VERSION=5
ARG CXX_VERSION=5
ARG PYTHON_VERSION=3.6
ARG NUMBA_VERSION=0.40.0
ARG NUMPY_VERSION=1.14.5
#   Locked to Pandas 0.20.3 by https://github.com/gpuopenanalytics/pygdf/issues/118
ARG PANDAS_VERSION=0.20.3
ARG LIBGDF_REPO=https://github.com/gpuopenanalytics/libgdf
ARG HASH_JOIN=ON
ARG ARROW_REPO=https://github.com/apache/arrow.git
ARG PYGDF_REPO=https://github.com/gpuopenanalytics/pygdf
#   Needed for pygdf.concat(), avoids "OSError: library nvvm not found"
ENV NUMBAPRO_NVVM="/usr/local/cuda/nvvm/lib64/libnvvm.so"
ENV NUMBAPRO_LIBDEVICE="/usr/local/cuda/nvvm/libdevice/"
ENV PATH="${PATH}:/conda/bin"
ENV ARROW_BUILD_TYPE="release"
ENV ARROW_HOME="/repos/dist"
ENV LD_LIBRARY_PATH="/repos/dist/lib:$LD_LIBRARY_PATH:/usr/local/cuda/lib64:/usr/local/lib"
ENV CC="/usr/bin/gcc-${CC_VERSION}"
ENV CXX="/usr/bin/g++-${CXX_VERSION}"
#   Install conda
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /miniconda.sh https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-ppc64le.sh
RUN sh /miniconda.sh -b -p /conda \
 && /conda/bin/conda update -n base conda
#   Enables "source activate conda"
SHELL ["/bin/bash", "-c"]
RUN apt-get update -y --fix-missing \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends git libboost-all-dev libffi-dev gfortran libjemalloc-dev flex bison pkg-config autoconf gcc-${CC_VERSION} g++-${CXX_VERSION} -y \
 && conda create -n gdf python=${PYTHON_VERSION} \
 && conda install -n gdf -y -c numba -c conda-forge -c defaults numba=${NUMBA_VERSION} numpy=${NUMPY_VERSION} pandas=${PANDAS_VERSION} cmake cython pytest \
 && git clone --recurse-submodules ${LIBGDF_REPO} -b v0.1.0a3 /libgdf \
 && source activate gdf \
 && mkdir -p /libgdf/build \
 && cd /libgdf/ \
 && cd build \
 && cmake .. \
 && cmake .. -DHASH_JOIN=${HASH_JOIN} \
 && make -j install \
 && make copy_python \
 && python setup.py install \
 && mkdir -p /repos \
 && git clone --recurse-submodules ${ARROW_REPO} /repos/arrow \
 && mkdir -p /repos/dist \
 && source activate gdf \
 && mkdir -p /repos/arrow/cpp/build \
 && cd /repos/arrow/cpp/build \
 && cmake -DCMAKE_BUILD_TYPE=$ARROW_BUILD_TYPE -DCMAKE_INSTALL_PREFIX=$ARROW_HOME -DARROW_PYTHON=on -DARROW_BUILD_TESTS=OFF .. \
 && make -j4 \
 && make install \
 && source activate gdf \
 && cd /repos/arrow/python \
 && python setup.py build_ext --build-type=$ARROW_BUILD_TYPE --inplace \
 && python setup.py install \
 && git clone --recurse-submodules ${PYGDF_REPO} -b v0.1.0a3 /pygdf \
 && source activate gdf \
 && cd /pygdf \
 && python setup.py install \
 && apt-get remove -y git libffi-dev libjemalloc-dev flex bison pkg-config \
 && apt-get autoremove -y
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
