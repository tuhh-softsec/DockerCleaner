#  Copyright (c) 2017 Sony Corporation. All Rights Reserved.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#  for nnabla>=1.0.17
FROM multiarch/ubuntu-core:armhf-xenial
ENV LC_ALL="C"
ENV LANG="C"
ENV LANGUAGE="C"
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2 ca-certificates ccache clang-format-3.8 cmake curl g++ git libarchive-dev libatlas-dev libhdf5-dev liblapack-dev make pkg-config python python-dev python-pip python-setuptools python-wheel python3 python3-dev python3-pip python3-setuptools python3-wheel unzip wget zip -y
RUN mkdir /tmp/deps \
 && cd /tmp/deps \
 && curl -L https://github.com/google/protobuf/archive/v3.1.0.tar.gz -o protobuf-v3.1.0.tar.gz \
 && tar xvf protobuf-v3.1.0.tar.gz \
 && cd protobuf-3.1.0 \
 && mkdir build \
 && cd build \
 && cmake -DCMAKE_POSITION_INDEPENDENT_CODE=ON -Dprotobuf_BUILD_TESTS=OFF ../cmake \
 && make \
 && make install \
 && cd / \
 && rm -rf /tmp/*
RUN python3 -m pip install --upgrade pip
RUN python2 -m pip install --upgrade pip
RUN pip install autopep8 boto3 cython h5py mako numpy>==.16 onnx==1.4.1 pillow protobuf pytest pyyaml requests scipy tqdm virtualenv --no-cache-dir
RUN pip3 install --no-cache-dir autopep8 boto3 cython h5py mako numpy>=1.16 onnx==1.4.1 pillow protobuf pytest pyyaml requests scipy tqdm virtualenv
RUN pip install ipython==5.0
RUN pip3 install ipython
