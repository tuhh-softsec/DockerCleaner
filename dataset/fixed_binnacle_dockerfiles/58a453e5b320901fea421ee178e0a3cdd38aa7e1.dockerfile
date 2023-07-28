FROM intelpython/intelpython3_core AS TENSORFLOW
RUN apt-get update \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u362-ga-0ubuntu2 openjdk-8-jre-headless=8u362-ga-0ubuntu2 build-essential=12.9ubuntu3 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 libcurl3-dev libfreetype6-dev=2.12.1+dfsg-4 libhdf5-serial-dev libpng-dev=1.6.39-2 libzmq3-dev=4.3.4-6 pkg-config=1.8.1-1ubuntu2 rsync=3.2.7-1 software-properties-common=0.99.35 unzip=6.0-27ubuntu1 zip=3.0-13 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 -y \
 && apt-get clean
RUN git clone -b r1.14 --depth 1 https://github.com/tensorflow/tensorflow
RUN conda create --name myenv -y
ENV PATH="/opt/conda/envs/myenv/bin:$PATH"
#   Set up Bazel.
#   Running bazel inside a `docker build` command causes trouble, cf:
#     https://github.com/bazelbuild/bazel/issues/134
#   The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /etc/bazel.bazelrc
#   Similarly, we need to workaround sandboxing issues:
#     https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /etc/bazel.bazelrc
#   Install the most recent bazel release.
ENV BAZEL_VERSION="0.24.1"
WORKDIR /
RUN mkdir /bazel \
 && cd /bazel \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -O https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && curl -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36" -fSsL -o /bazel/LICENSE.txt https://raw.githubusercontent.com/bazelbuild/bazel/master/LICENSE \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -f /bazel/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh
RUN cd tensorflow \
 && bazel build tensorflow/tools/graph_transforms:summarize_graph
FROM intelpython/intelpython3_core AS IE
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.71-3 automake=1:1.16.5-1.3 build-essential=12.9ubuntu3 ca-certificates=20230311 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 gstreamer1.0-plugins-base=1.22.0-3 libavcodec-dev=7:5.1.2-3ubuntu1 libavformat-dev=7:5.1.2-3ubuntu1 libboost-regex-dev=1.74.0.3ubuntu7 libcairo2-dev=1.16.0-7 libgfortran3 libglib2.0-dev=2.76.0-1ubuntu1 libgstreamer1.0-0=1.22.0-2 libgtk2.0-dev=2.24.33-2ubuntu2 libopenblas-dev=0.3.21+ds-4 libpango1.0-dev=1.50.12+ds-1 libpng-dev=1.6.39-2 libssl-dev=3.0.8-1ubuntu1 libswscale-dev=7:5.1.2-3ubuntu1 libtool=2.4.7-5 libusb-1.0-0-dev=2:1.0.26-1 pkg-config=1.8.1-1ubuntu2 unzip=6.0-27ubuntu1 vim=2:9.0.1000-4ubuntu2 wget=1.21.3-1ubuntu1 -y
RUN wget https://cmake.org/files/v3.14/cmake-3.14.3.tar.gz \
 && tar -xvzf cmake-3.14.3.tar.gz \
 && cd cmake-3.14.3/ \
 && ./configure \
 && make -j$( nproc ;) \
 && make install
RUN echo "deb http://ftp.us.debian.org/debian/ jessie main contrib non-free" >> /etc/apt/sources.list \
 && echo "deb-src http://ftp.us.debian.org/debian/ jessie main contrib non-free" >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends g++-4.9 -y
ENV CXX="/usr/bin/g++-4.9"
RUN pip install cython==0.29.34 numpy==1.24.2
ARG DLDT_DIR=/dldt-2019_R1.0.1
RUN git clone --depth=1 -b 2019_R1.0.1 https://github.com/opencv/dldt.git ${DLDT_DIR} \
 && cd ${DLDT_DIR} \
 && git submodule init \
 && git submodule update --recursive \
 && rm -Rf .git \
 && rm -Rf model-optimizer
WORKDIR ${DLDT_DIR}
RUN curl -L https://github.com/intel/mkl-dnn/releases/download/v0.18/mklml_lnx_2019.0.3.20190220.tgz | tar -xz
WORKDIR ${DLDT_DIR}/inference-engine/build
RUN cmake -DGEMM=MKL -DMKLROOT=${DLDT_DIR}/mklml_lnx_2019.0.3.20190220 -DENABLE_MKL_DNN=ON -DTHREADING=OMP -DCMAKE_BUILD_TYPE=Release ..
RUN make -j$( nproc ;)
WORKDIR ${DLDT_DIR}/inference-engine/ie_bridges/python/build
RUN cmake -DInferenceEngine_DIR=${DLDT_DIR}/inference-engine/build -DPYTHON_EXECUTABLE=$( which python ;) -DPYTHON_LIBRARY=/opt/conda/lib/libpython3.6m.so -DPYTHON_INCLUDE_DIR=/opt/conda/include/python3.6m ${DLDT_DIR}/inference-engine/ie_bridges/python \
 && make -j$( nproc ;)
FROM intelpython/intelpython3_core AS FINAL
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 ca-certificates=20230311 python3-pip=23.0.1+dfsg-1 gcc=4:12.2.0-3ubuntu1 python-setuptools python3-setuptools=66.1.1-1 libgfortran3 unzip=6.0-27ubuntu1 vim=2:9.0.1000-4ubuntu2 -y \
 && apt-get clean
RUN curl -L -o 2019_R1.0.1.tar.gz https://github.com/opencv/dldt/archive/2019_R1.0.1.tar.gz \
 && tar -zxf 2019_R1.0.1.tar.gz \
 && rm 2019_R1.0.1.tar.gz \
 && rm -Rf dldt-2019_R1.0.1/inference-engine
WORKDIR dldt-2019_R1.0.1/model-optimizer
RUN conda create --name myenv -y
ENV PATH="/opt/conda/envs/myenv/bin:$PATH"
RUN pip install pip==23.1 setuptools==67.6.1 --upgrade
RUN pip install -r requirements.txt
RUN curl -L -o google-cloud-sdk.zip https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.zip \
 && unzip -qq google-cloud-sdk.zip -d tools \
 && rm google-cloud-sdk.zip \
 && tools/google-cloud-sdk/install.sh --usage-reporting=false --path-update=true --bash-completion=false --disable-installation-options \
 && tools/google-cloud-sdk/bin/gcloud -q components update gcloud core gsutil \
 && tools/google-cloud-sdk/bin/gcloud config set component_manager/disable_update_check true \
 && touch tools/google-cloud-sdk/lib/third_party/google.py \
 && pip install crcmod==1.7 -U
ENV PATH="${PATH}:/dldt-2019_R1.0.1/model-optimizer:/dldt-2019_R1.0.1/model-optimizer/tools/google-cloud-sdk/bin"
COPY --from=IE /dldt-2019_R1.0.1/inference-engine/bin/intel64/Release/lib/*.so /usr/local/lib/
COPY --from=IE /dldt-2019_R1.0.1/inference-engine/ie_bridges/python/bin/intel64/Release/python_api/python3.6/openvino/ /usr/local/lib/openvino/
COPY --from=IE /dldt-2019_R1.0.1/mklml_lnx_2019.0.3.20190220/lib/lib*.so /usr/local/lib/
ENV LD_LIBRARY_PATH="/usr/local/lib"
WORKDIR /slim
RUN git clone --depth 1 https://github.com/tensorflow/models \
 && rm -Rf models/.git \
 && git clone --depth 1 -b r1.14 https://github.com/tensorflow/tensorflow \
 && rm -Rf tensorflow/.git
ENV PYTHONPATH="/usr/local/lib:/slim/models/research/slim:/slim/tensorflow/python/tools"
COPY --from=TENSORFLOW /tensorflow/bazel-bin/tensorflow/tools/graph_transforms/summarize_graph /usr/bin/summarize_graph
COPY --from=TENSORFLOW /root/.cache/bazel/_bazel_root/*/execroot/org_tensorflow/bazel-out/k8-opt/bin/_solib_k8/_U_S_Stensorflow_Stools_Sgraph_Utransforms_Csummarize_Ugraph___Utensorflow/libtensorflow_framework.so.1 /usr/local/lib/libtensorflow_framework.so.1
WORKDIR /scripts
COPY classes.py convert_model.py predict.py slim_model.py requirements.txt ./
RUN pip install -r requirements.txt
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
