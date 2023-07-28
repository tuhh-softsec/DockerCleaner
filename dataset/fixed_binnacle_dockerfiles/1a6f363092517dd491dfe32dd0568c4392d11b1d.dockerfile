FROM ubuntu:16.04
RUN apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.1ubuntu2 libopenblas-dev=0.2.18-1ubuntu1 wget=1.17.1-1ubuntu1.5 git=1:2.7.4-0ubuntu1.10 automake=1:1.15-4ubuntu1 cmake=3.5.1-1ubuntu3 pkg-config=0.29.1-0ubuntu1 python3-numpy=1:1.11.0-1ubuntu1 python3-wheel=0.29.0-1 unzip=6.0-20ubuntu1.1 curl=7.47.0-1ubuntu2.19 sudo=1.8.16-0ubuntu1.10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libcurl3-dev libfreetype6-dev=2.6.1-0.1ubuntu2.5 libpng12-dev=1.2.54-1ubuntu1.1 libzmq3-dev=4.1.4-7ubuntu0.1 rsync=3.1.1-3ubuntu1.3 zip=3.0-11 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 -y )
RUN : \
 && DEBIAN_FRONTEND=noninteractive apt-get -y install gcc g++ gfortran wget cpio \
 && cd /tmp \
 && wget -q http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/12070/l_mkl_2018.0.128.tgz \
 && tar -xzf l_mkl_2018.0.128.tgz \
 && cd l_mkl_2018.0.128 \
 && sed -i 's/ACCEPT_EULA=decline/ACCEPT_EULA=accept/g' silent.cfg \
 && sed -i 's/ARCH_SELECTED=ALL/ARCH_SELECTED=INTEL64/g' silent.cfg \
 && sed -i 's/COMPONENTS=DEFAULTS/COMPONENTS=;intel-comp-l-all-vars__noarch;intel-comp-nomcu-vars__noarch;intel-openmp__x86_64;intel-tbb-libs__x86_64;intel-mkl-common__noarch;intel-mkl-installer-license__noarch;intel-mkl-core__x86_64;intel-mkl-core-rt__x86_64;intel-mkl-doc__noarch;intel-mkl-doc-ps__noarch;intel-mkl-gnu__x86_64;intel-mkl-gnu-rt__x86_64;intel-mkl-common-ps__noarch;intel-mkl-core-ps__x86_64;intel-mkl-common-c__noarch;intel-mkl-core-c__x86_64;intel-mkl-common-c-ps__noarch;intel-mkl-tbb__x86_64;intel-mkl-tbb-rt__x86_64;intel-mkl-gnu-c__x86_64;intel-mkl-common-f__noarch;intel-mkl-core-f__x86_64;intel-mkl-gnu-f-rt__x86_64;intel-mkl-gnu-f__x86_64;intel-mkl-f95-common__noarch;intel-mkl-f__x86_64;intel-mkl-psxe__noarch;intel-psxe-common__noarch;intel-psxe-common-doc__noarch;intel-compxe-pset/g' silent.cfg \
 && ./install.sh -s silent.cfg \
 && cd .. \
 && rm -rf * \
 && rm -rf /opt/intel/.*.log /opt/intel/compilers_and_libraries_2018.0.128/licensing \
 && echo "/opt/intel/mkl/lib/intel64" >> /etc/ld.so.conf.d/intel.conf \
 && ldconfig \
 && echo "source /opt/intel/mkl/bin/mklvars.sh intel64" >> /etc/bash.bashrc
#   Configure environment
ENV CONDA_DIR="/opt/conda"
ENV PATH="$CONDA_DIR/bin:$PATH"
RUN cd /tmp \
 && mkdir -p $CONDA_DIR \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda3-4.3.21-Linux-x86_64.sh \
 && /bin/bash Miniconda3-4.3.21-Linux-x86_64.sh -f -b -p $CONDA_DIR \
 && rm Miniconda3-4.3.21-Linux-x86_64.sh \
 && $CONDA_DIR/bin/conda install --quiet --yes conda==4.3.21 \
 && $CONDA_DIR/bin/conda config --system --add channels conda-forge \
 && $CONDA_DIR/bin/conda config --system --set auto_update_conda false \
 && conda clean -tipsy
RUN pip install setuptools==67.6.1
RUN pip install cython==0.29.34 pillow==9.5.0 scipy==1.10.1 matplotlib==3.7.1 pandas==2.0.0 h5py==3.8.0 tqdm==4.65.0
#   Set up Bazel.
#   Running bazel inside a `docker build` command causes trouble, cf:
#     https://github.com/bazelbuild/bazel/issues/134
#   The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /etc/bazel.bazelrc
#   Similarly, we need to workaround sandboxing issues:
#     https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /etc/bazel.bazelrc
#   Install the most recent bazel release.
ENV BAZEL_VERSION="0.4.5"
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
WORKDIR /
RUN git clone https://github.com/tensorflow/tensorflow.git \
 && cd tensorflow \
 && git checkout r1.0
WORKDIR /tensorflow
#   Configure the build for our CUDA configuration.
ENV CI_BUILD_PYTHON="python"
ENV TF_NEED_CUDA="0"
ENV TF_BUILD_ENABLE_XLA="1"
RUN tensorflow/tools/ci_build/builds/configured CPU bazel build -c opt --copt=-march=native --cxxopt="-D_GLIBCXX_USE_CXX11_ABI=0" --config=mkl --copt="-DEIGEN_USE_VML" tensorflow/tools/pip_package:build_pip_package
RUN bazel-bin/tensorflow/tools/pip_package/build_pip_package /tmp/pip \
 && pip install setuptools==67.6.1 --upgrade -I \
 && pip install /tmp/pip/tensorflow-*.whl --no-cache-dir --upgrade \
 && rm -rf /tmp/pip \
 && rm -rf /root/.cache
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
