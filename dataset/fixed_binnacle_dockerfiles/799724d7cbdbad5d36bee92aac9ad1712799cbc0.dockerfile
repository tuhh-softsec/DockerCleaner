ARG docker_name
FROM $docker_name
MAINTAINER H2o.ai <ops@h2o.ai>
ENV HOME="/root"
ENV CUDA_HOME="/usr/local/cuda"
ENV PATH="/usr/local/cuda/bin:$PATH"
ENV CUDADIR="/usr/local/cuda/include/"
ENV LD_LIBRARY_PATH="/usr/lib64:/usr/local/lib:$LD_LIBRARY_PATH"
ENV MINICONDA_VERSION="4.4.10"
#   Setup gcc etc.
RUN yum install -y epel-release
RUN yum install -y gcc gcc-c++ libgcc libstdc++ libgomp glibc
RUN yum install -y make ncurses-devel zlib-devel epel-release zeromq-devel wget blas-devel openblas-devel libpng-devel freetype-devel bzip2 \
 && wget https://repo.continuum.io/miniconda/Miniconda3-${MINICONDA_VERSION}-Linux-`arch `.sh \
 && bash Miniconda3-${MINICONDA_VERSION}-Linux-`arch `.sh -b -p /opt/h2oai/h2o4gpu/python
ENV PATH="/opt/h2oai/h2o4gpu/python/bin:$PATH"
#
#   PPC64 specific - certain libs/whl don't support PPC64LE
#
WORKDIR $HOME
#   Arrow
RUN bash -c 'if [ `arch` = "ppc64le" ]; then yum install -y git boost-devel cmake3 autoconf libjpeg-devel \
 && ln -s /usr/bin/cmake3 /usr/bin/cmake \
 && git clone https://github.com/apache/arrow.git \
 && cd $HOME/arrow/cpp \
 && git checkout tags/apache-arrow-0.12.0 \
 && pip install numpy==1.16.1 cython==0.29.3 \
 && export ARROW_BUILD_TYPE=release \
 && export ARROW_HOME=/usr/local \
 && cmake -DARROW_CXXFLAGS="-lutil" -DCMAKE_INSTALL_PREFIX=$ARROW_HOME -DARROW_PYTHON=on -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=$ARROW_BUILD_TYPE -DARROW_BUILD_TESTS=OFF \
 && make -j \
 && make install \
 && cd $HOME/arrow/python \
 && python setup.py install; fi'
WORKDIR /
#   Add requirements
COPY src/interface_py/requirements_runtime.txt requirements.txt
COPY src/interface_py/requirements_runtime_demos.txt requirements_runtime_demos.txt
RUN chmod a+rwx requirements*.txt \
 && pip install --no-cache-dir -r requirements.txt \
 && pip install --no-cache-dir -r requirements_runtime_demos.txt
RUN mkdir -p /etc/OpenCL/vendors \
 && echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd
RUN (localedef -v -c -i en_US -f UTF-8 en_US.UTF-8 || true )
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LD_LIBRARY_PATH_CUDA="$CUDA_HOME/lib64/:$CUDA_HOME/lib/:/usr/local/cuda/lib64:/usr/local/cuda/extras/CUPTI/lib64:/usr/local/nvidia/lib:/usr/local/nvidia/lib64"
#   Add a canned jupyter notebook demo to the container
RUN mkdir -p jupyter/demos
COPY examples/py/demos/H2O4GPU_Ridge.ipynb /jupyter/demos/H2O4GPU_Ridge.ipynb
COPY examples/py/demos/H2O4GPU_LinearRegression.ipynb /jupyter/demos/H2O4GPU_LinearRegression.ipynb
COPY examples/py/demos/H2O4GPU_GBM.ipynb /jupyter/demos/H2O4GPU_GBM.ipynb
COPY examples/py/demos/H2O4GPU_GLM.ipynb /jupyter/demos/H2O4GPU_GLM.ipynb
COPY examples/py/demos/H2O4GPU_Lasso.ipynb /jupyter/demos/H2O4GPU_Lasso.ipynb
COPY examples/py/demos/H2O4GPU_KMeans_Images.ipynb /jupyter/demos/H2O4GPU_KMeans_Images.ipynb
COPY examples/py/demos/H2O4GPU_KMeans_Quantization.ipynb /jupyter/demos/H2O4GPU_KMeans_Quantization.ipynb
COPY examples/py/demos/Multi-GPU-H2O-GLM-simple.ipynb /jupyter/demos/Multi-GPU-H2O-GLM-simple.ipynb
COPY examples/py/demos/H2O4GPU_TruncatedSVD.ipynb /jupyter/demos/H2O4GPU_TruncatedSVD.ipynb
COPY examples/py/demos/H2O4GPU_PCA.ipynb /jupyter/demos/H2O4GPU_PCA.ipynb
COPY examples/py/demos/H2O4GPU_Daal_LinearRegression.ipynb /jupyter/demos/H2O4GPU_Daal_LinearRegression.ipynb
COPY examples/py/demos/figures /jupyter/demos/figures
RUN cd /jupyter/demos \
 && chmod -R a+rwx /jupyter \
 && mkdir /scikit_learn_data \
 && chmod -R a+rwx /scikit_learn_data \
 && mkdir -p /jupyter/scikit_learn_data/covertype \
 && chmod -R a+rwx /jupyter/scikit_learn_data/covertype \
 && mkdir -p /jupyter/scikit_learn_data/lfw_home \
 && chmod -R a+rwx /jupyter/scikit_learn_data/lfw_home \
 && HOME=/jupyter jupyter notebook --generate-config \
 && sed -i "s/#c.NotebookApp.token = '<generated>'/c.NotebookApp.token = 'h2o'/" /jupyter/.jupyter/jupyter_notebook_config.py \
 && chmod -R a+rwx /jupyter/.jupyter
#   Add shell wrapper
COPY scripts/run.sh /run.sh
RUN chmod a+rwx run.sh
RUN wget https://s3.amazonaws.com/artifacts.h2o.ai/releases/ai/h2o/dai-thirdparty-deps/1.0-master-2/`arch `-centos7/llvm.tar.bz2 \
 && tar xvf llvm.tar.bz2 \
 && cp -r llvm/* /opt/h2oai/h2o4gpu/ \
 && rm -rf llvm*
ENV LLVM4="/opt/h2oai/h2o4gpu"
ENV PATH="$LLVM4/bin:$PATH"
ENV LD_LIBRARY_PATH="$LLVM4/lib:$LD_LIBRARY_PATH"
ARG h2o4gpu_VERSION
ARG h2o4gpu_COMMIT
ARG DOCKER_VERSION_TAG
LABEL h2o4gpu_commit="$h2o4gpu_COMMIT" \
      docker_version_tag="$DOCKER_VERSION_TAG"
ENTRYPOINT ["./run.sh"]
EXPOSE 8888/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
