ARG docker_name
FROM $docker_name
MAINTAINER H2o.ai <ops@h2o.ai>
#
#   Env variables for CUDA. Necessary because certain systems don't support nvidia-docker so we should use plain docker as much as possible.
#
ENV HOME="/root"
ENV CUDA_HOME="/usr/local/cuda"
ENV CUDADIR="/usr/local/cuda/include/"
ENV PATH="/usr/local/cuda/bin:$PATH"
ENV LD_LIBRARY_PATH_CUDA="$CUDA_HOME/lib64/:$CUDA_HOME/lib/:/usr/local/cuda/lib64:/usr/local/cuda/extras/CUPTI/lib64:/usr/local/nvidia/lib:/usr/local/nvidia/lib64"
ENV LD_LIBRARY_PATH_BUILD="/lib64:/usr/local/lib64:/home/$USER/lib/"
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH_CUDA:$LD_LIBRARY_PATH_BUILD:$LD_LIBRARY_PATH"
#
#   Env variables used by the codebase.
#
ENV OMP_NUM_THREADS="32"
ENV MKL_NUM_THREADS="32"
ENV VECLIB_MAXIMUM_THREADS="32"
#
#   Library versions
#
ENV MINICONDA_VERSION="4.4.10"
ENV SWIG_VERSION="3.0.12"
ENV PILLOW_VERSION="4.2.1"
ENV GIT_VERSION="2.17.0"
#
#   Install necessary libraries and dependencies
#
RUN yum install -y epel-release
#   Setup gcc etc.
RUN yum install -y gcc gcc-c++ libgcc libstdc++ libgomp glibc
#   Git requirements
RUN yum install -y libcurl-devel zlib-devel asciidoc xmlto wget make autoconf gettext
#   Compile from source because yum's latest version is 1.8.3
#   --depth for submodule update which we use was added in 1.8.4
RUN wget https://www.kernel.org/pub/software/scm/git/git-${GIT_VERSION}.tar.xz \
 && tar xf git-${GIT_VERSION}.tar.xz \
 && cd git-${GIT_VERSION} \
 && make configure \
 && ./configure --prefix=/usr \
 && make all \
 && make install
#   H2O4GPU requirements + util programs
RUN yum install -y ncurses-devel bzip2 which axel cmake3 openssl-devel libpng-devel freetype-devel blas-devel epel-release zeromq-devel openblas-devel \
 && wget https://repo.continuum.io/miniconda/Miniconda3-${MINICONDA_VERSION}-Linux-`arch `.sh \
 && bash Miniconda3-${MINICONDA_VERSION}-Linux-`arch `.sh -b -p /opt/h2oai/h2o4gpu/python \
 && /opt/h2oai/h2o4gpu/python/bin/conda install -y conda-build six=1.11.0 \
 && wget https://s3.amazonaws.com/artifacts.h2o.ai/releases/ai/h2o/dai-thirdparty-deps/1.0-master-2/`arch `-centos7/llvm.tar.bz2 \
 && tar xvf llvm.tar.bz2 \
 && cp -r llvm/* /opt/h2oai/h2o4gpu/ \
 && rm -rf llvm*
ENV LLVM4="/opt/h2oai/h2o4gpu"
ENV PATH="/opt/h2oai/h2o4gpu/python/bin:$PATH"
ENV PATH="/usr/local/bin:$PATH"
ENV PATH="$LLVM4/bin:$PATH"
ENV LD_LIBRARY_PATH="$LLVM4/lib:$LD_LIBRARY_PATH"
#
#   Symlinks
#
#   AR for conda
RUN ln /usr/bin/ar $LLVM4/bin/`arch `-conda_cos6-linux-gnu-ar
#   CentOS' yum install cmake has only 2.X so need to install cmake3 and make a symlink
RUN ln -s /usr/bin/cmake3 /usr/bin/cmake
#   Symlinks for Python libs used by SWIG in CMake - it does not recognize Miniconda paths otherwise
RUN mkdir -p /usr/lib64/ \
 && ln -sf /opt/h2oai/h2o4gpu/python/lib/libpython* /usr/lib64/ \
 && mkdir -p /usr/include/python3.6m \
 && ln -s /opt/h2oai/h2o4gpu/python/include/python3.6m/* /usr/include/python3.6m
#   Yumming openblas puts some files in a not-so-standard locations
RUN ln -s /usr/include/openblas/* /usr/local/include/
#   Symlinks for NVML
RUN mkdir -p /usr/lib64/nvidia/ \
 && ln -s /usr/local/cuda-`nvcc --version | tail -n 1 | cut -f 5 -d' ' | cut -f 1 -d ',' `/targets/`arch `-linux/lib/stubs/libnvidia-ml.so /usr/lib64/nvidia/libnvidia-ml.so
#
#   Builds from source due to too old versions in yum
#
WORKDIR $HOME
#   SWIG
RUN wget https://sourceforge.net/projects/swig/files/swig/swig-${SWIG_VERSION}/swig-${SWIG_VERSION}.tar.gz \
 && tar -zxvf swig-${SWIG_VERSION}.tar.gz \
 && cd swig-${SWIG_VERSION} \
 && ./configure --prefix=/usr \
 && make -j $( nproc ;) \
 && make install \
 && cd $HOME \
 && rm -rf swig-3*
#   TODO Install DAAL
#
#   PPC64 specific - certain libs/whl don't support PPC64LE
#
#   Arrow
RUN bash -c 'if [ `arch` = "ppc64le" ]; then git clone https://github.com/apache/arrow.git \
 && cd $HOME/arrow/cpp \
 && git checkout tags/apache-arrow-0.12.0 \
 && yum install -y boost-devel \
 && pip install numpy==1.16.1 cython==0.29.3 \
 && cmake -DARROW_CXXFLAGS="-lutil" -DARROW_PYTHON=on \
 && make -j \
 && make install \
 && cd $HOME/arrow/python \
 && ARROW_HOME=/usr/local python setup.py install \
 && yum install -y libjpeg-devel; fi'
#   Pillow
RUN bash -c 'if [ `arch` = "ppc64le" ]; then wget https://files.pythonhosted.org/packages/55/aa/f7f983fb72710a9daa4b3374b7c160091d3f94f5c09221f9336ade9027f3/Pillow-${PILLOW_VERSION}.tar.gz \
 && tar xvf Pillow-${PILLOW_VERSION}.tar.gz \
 && cd $HOME/Pillow-${PILLOW_VERSION} \
 && sed -i "s/'ppc64'/'ppc64le'/g" setup.py \
 && python setup.py install \
 && cd $HOME \
 && rm -rf Pillow-${PILLOW_VERSION}*; fi'
#
#   Install Python requirements
#
RUN pip install numpy==1.16.1 scipy==1.2.1 setuptools==39.0.1
RUN git clone https://github.com/NVIDIA/nccl.git \
 && cd nccl \
 && git checkout tags/v2.4.7-1 \
 && make -j src.build
COPY src/interface_py/requirements_buildonly.txt requirements_buildonly.txt
COPY src/interface_py/requirements_runtime.txt requirements_runtime.txt
COPY src/interface_py/requirements_runtime_demos.txt requirements_runtime_demos.txt
RUN pip install -r requirements_buildonly.txt
RUN pip install -r requirements_runtime.txt
RUN pip install -r requirements_runtime_demos.txt
RUN mkdir -p /etc/OpenCL/vendors \
 && echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd
RUN (localedef -v -c -i en_US -f UTF-8 en_US.UTF-8 || true )
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
#   See https://github.com/Microsoft/LightGBM/wiki/Installation-Guide#with-gpu-support for details
#   https://github.com/Microsoft/LightGBM/pull/929/files
#   Could compile with these as well: -DBOOST_COMPUTE_USE_OFFLINE_CACHE=OFF -DBOOST_COMPUTE_THREAD_SAFE=ON
RUN export CUDA_HOME=/usr/local/cuda/ \
 && yum install -y opencl-headers icu libicu-devel bzip2 bzip2-devel zlib-devel python-devel \
 && wget https://s3.amazonaws.com/0xdata-public/boost/boost_1_69_0.tar.bz2 \
 && tar xjf boost_1_69_0.tar.bz2 \
 && cd boost_1_69_0 \
 && export PYTHONPATH=/opt/h2oai/h2o4gpu/python/ \
 && ./bootstrap.sh --prefix=/opt/boost/ --with-python=python3 \
 && export CPPFLAGS="-I/opt/h2oai/h2o4gpu/python/include/python3.6m/ -fPIC" \
 && export C_INCLUDE_PATH="/opt/h2oai/h2o4gpu/python/include/python3.6m/" ; export CPLUS_INCLUDE_PATH="/opt/h2oai/h2o4gpu/python/include/python3.6m/" \
 && ./b2 link=static -a -d0 install --prefix=/opt/boost/ --with=all -j 20 cxxflags="-fPIC -I /opt/h2oai/h2o4gpu/python/include/python3.6m/" \
 && cd /usr/include ; rm -rf boost ; ln -s /opt/boost/include/boost . \
 && cd /usr/lib64/ ; rm -rf libboost* ; cp -a /opt/boost/lib/* . \
 && cd /
#  	yum install -y boost boost-devel boost-system boost-filesystem boost-thread
ENV LD_LIBRARY_PATH="/opt/boost/lib/:$LD_LIBRARY_PATH"
RUN chmod -R o+rwx /opt/h2oai/h2o4gpu/python
RUN chmod -R o+rwx /root
RUN yum install -y hdf5-devel
RUN bash -c 'if [ `arch` == "ppc64le" ]; then yum install -y ocl-icd; fi'
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/lib64/nvidia"
ENV CUDA_TOOLKIT_ROOT_DIR="/usr/local/cuda-9.2"
WORKDIR $HOME
ENV GIT_AUTHOR_NAME="anonymous"
ENV GIT_AUTHOR_EMAIL="anonymous@h2o.ai"
ENV GIT_COMMITTER_NAME="anonymous"
ENV GIT_COMMITTER_EMAIL="anonymous@h2o.ai"
ENV EMAIL="anonymous@h2o.ai"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
