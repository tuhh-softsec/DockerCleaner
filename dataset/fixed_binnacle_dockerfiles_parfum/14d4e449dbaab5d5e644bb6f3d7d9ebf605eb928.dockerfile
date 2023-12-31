#  Licensed to the Apache Software Foundation (ASF) under one
#  or more contributor license agreements.  See the NOTICE file
#  distributed with this work for additional information
#  regarding copyright ownership.  The ASF licenses this file
#  to you under the Apache License, Version 2.0 (the
#  "License"); you may not use this file except in compliance
#  with the License.  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing,
#  software distributed under the License is distributed on an
#  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#  KIND, either express or implied.  See the License for the
#  specific language governing permissions and limitations
#  under the License.
FROM quay.io/pypa/manylinux1_x86_64:latest
#  Install dependencies
RUN yum install -y xz ccache flex wget \
 && yum clean all
COPY scripts/build_zlib.sh /
RUN /build_zlib.sh
COPY scripts/build_openssl.sh /
RUN /build_openssl.sh
#  Install cmake manylinux1 package
COPY scripts/install_cmake.sh /
RUN /install_cmake.sh
COPY scripts/build_gflags.sh /
RUN /build_gflags.sh
COPY scripts/build_protobuf.sh /
RUN /build_protobuf.sh
ENV PROTOBUF_HOME="/usr"
COPY scripts/build_cares.sh /
RUN /build_cares.sh
COPY scripts/build_grpc.sh /
RUN /build_grpc.sh
COPY scripts/build_boost.sh /
RUN /build_boost.sh
COPY scripts/build_gtest.sh /
RUN /build_gtest.sh
ENV GTEST_HOME="/usr"
COPY scripts/build_flatbuffers.sh /
RUN /build_flatbuffers.sh
ENV FLATBUFFERS_HOME="/usr"
COPY scripts/build_bison.sh /
RUN /build_bison.sh
COPY scripts/build_thrift.sh /
RUN /build_thrift.sh
ENV THRIFT_HOME="/usr"
COPY scripts/build_brotli.sh /
RUN /build_brotli.sh
ENV BROTLI_HOME="/usr"
COPY scripts/build_snappy.sh /
RUN /build_snappy.sh
ENV SNAPPY_HOME="/usr"
COPY scripts/build_lz4.sh /
RUN /build_lz4.sh
ENV LZ4_HOME="/usr"
COPY scripts/build_zstd.sh /
RUN /build_zstd.sh
ENV ZSTD_HOME="/usr"
COPY scripts/build_ccache.sh /
RUN /build_ccache.sh
COPY scripts/build_glog.sh /
RUN /build_glog.sh
ENV GLOG_HOME="/usr"
WORKDIR /
RUN git clone https://github.com/matthew-brett/multibuild.git \
 && cd multibuild \
 && git checkout ffe59955ad8690c2f8bb74766cb7e9b0d0ee3963
COPY scripts/build_virtualenvs.sh /
RUN /build_virtualenvs.sh
COPY scripts/build_llvm.sh /
RUN /build_llvm.sh
COPY scripts/build_clang.sh /
RUN /build_clang.sh
COPY scripts/build_double_conversion.sh /
RUN /build_double_conversion.sh
COPY scripts/build_rapidjson.sh /
RUN /build_rapidjson.sh
COPY scripts/build_re2.sh /
RUN /build_re2.sh
COPY scripts/build_uriparser.sh /
RUN /build_uriparser.sh
COPY scripts/build_bz2.sh /
RUN /build_bz2.sh
