#   Copyright 2018 Google LLC
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
ARG DISTRO_VERSION=18.04
FROM ubuntu:${DISTRO_VERSION}
RUN apt-get update \
 && apt-get install --no-install-recommends abi-compliance-checker abi-dumper automake build-essential ccache clang clang-format-7 cmake curl doxygen gawk git gcc g++ cmake libcurl4-openssl-dev libssl-dev libtool lsb-release make pkg-config python-pip shellcheck tar unzip wget zlib1g-dev -y
#   By default, Ubuntu 18.04 does not install the alternatives for clang-format
#   and clang-tidy, so we need to manually install those.
RUN if grep -q 18.04 /etc/lsb-release ; then apt update \
 && apt-get install --no-install-recommends clang-tidy clang-format-7 clang-tools -y ;update-alternatives --install /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-6.0 100 ;update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-7 100 ;update-alternatives --install /usr/bin/scan-build scan-build /usr/bin/scan-build-6.0 100 ; fi
#   Install the the buildifier tool, which does not compile with the default
#   golang compiler for Ubuntu 16.04 and Ubuntu 18.04.
RUN wget -q -O /usr/bin/buildifier https://github.com/bazelbuild/buildtools/releases/download/0.17.2/buildifier
RUN chmod 755 /usr/bin/buildifier
#   Install cmake_format to automatically format the CMake list files.
#       https://github.com/cheshirekow/cmake_format
#   Pin this to an specific version because the formatting changes when the
#   "latest" version is updated, and we do not want the builds to break just
#   because some third party changed something.
RUN pip install pip==23.1 --upgrade
RUN pip install numpy==1.24.2 cmake_format==0.4.0
#   Install Python packages used in the integration tests.
RUN pip install flask==2.2.3 httpbin==0.7.0 gevent==22.10.2 gunicorn==20.1.0 crc32c==2.3.post0
#   Parallelize the builds if possible. The default is chosen to work well on
#   Travis, but developers may want to override this value when building on their
#   workstations.
ARG NCPU=2
#   Install Crc32c library.
WORKDIR /var/tmp/build
RUN wget -q https://github.com/google/crc32c/archive/1.0.6.tar.gz
RUN tar -xf 1.0.6.tar.gz
WORKDIR /var/tmp/build/crc32c-1.0.6
RUN cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=yes -DCRC32C_BUILD_TESTS=OFF -DCRC32C_BUILD_BENCHMARKS=OFF -DCRC32C_USE_GLOG=OFF -H. -Bcmake-out/crc32c
RUN cmake --build cmake-out/crc32c --target install -- -j ${NCPU}
RUN ldconfig
#   Install protobuf using CMake. Some distributions include protobuf, but gRPC
#   requires 3.4.x or newer, and many of those distribution use older versions.
#   We need to install both the debug and Release version because:
#   - When using pkg-config, only the release version works, the pkg-config
#     file is hard-coded to search for `libprotobuf.so` (or `.a`)
#   - When using CMake, only the version compiled with the same CMAKE_BUILD_TYPE
#     as the dependent (gRPC or google-cloud-cpp) works.
WORKDIR /var/tmp/build
RUN wget -q https://github.com/google/protobuf/archive/v3.6.1.tar.gz
RUN tar -xf v3.6.1.tar.gz
WORKDIR /var/tmp/build/protobuf-3.6.1/cmake
RUN for build_type in "Debug" "Release"; do cmake -DCMAKE_BUILD_TYPE="${build_type}" -DBUILD_SHARED_LIBS=yes -Dprotobuf_BUILD_TESTS=OFF -H. -Bcmake-out-${build_type} ;cmake --build cmake-out-${build_type} --target install -- -j ${NCPU} ; done
RUN ldconfig
#   Many distributions include c-ares, but they do not include the CMake support
#   files for the library, so manually install it.  c-ares requires two install
#   steps because (1) the CMake-based build does not install pkg-config files,
#   and (2) the Makefile-based build does not install CMake config files.
WORKDIR /var/tmp/build
RUN apt-get remove -y libc-ares-dev libc-ares2
RUN wget -q https://github.com/c-ares/c-ares/archive/cares-1_14_0.tar.gz
RUN tar -xf cares-1_14_0.tar.gz
WORKDIR /var/tmp/build/c-ares-cares-1_14_0
RUN cmake -DCMAKE_BUILD_TYPE="Release" -DBUILD_SHARED_LIBS=yes -H. -Bcmake-out
RUN cmake --build cmake-out --target install -- -j ${NCPU}
RUN ./buildconf
RUN ./configure
RUN install -m 644 -D -t /usr/local/lib/pkgconfig libcares.pc
RUN ldconfig
#   Install gRPC. Note that we use the system's zlib and ssl libraries.
#   For similar reasons to c-ares (see above), we need two install steps.
WORKDIR /var/tmp/build
RUN wget -q https://github.com/grpc/grpc/archive/v1.19.1.tar.gz
RUN tar -xf v1.19.1.tar.gz
RUN ls -l
WORKDIR /var/tmp/build/grpc-1.19.1
RUN ls -l
RUN cmake -DCMAKE_BUILD_TYPE="Release" -DBUILD_SHARED_LIBS=yes -DgRPC_BUILD_TESTS=OFF -DgRPC_ZLIB_PROVIDER=package -DgRPC_SSL_PROVIDER=package -DgRPC_CARES_PROVIDER=package -DgRPC_PROTOBUF_PROVIDER=package -H. -Bcmake-out/grpc
RUN cmake --build cmake-out/grpc --target install -- -j ${NCPU}
RUN make install-pkg-config_c install-pkg-config_cxx install-certs
RUN ldconfig
#   Install googletest.
WORKDIR /var/tmp/build
RUN wget -q https://github.com/google/googletest/archive/b6cd405286ed8635ece71c72f118e659f4ade3fb.tar.gz
RUN tar -xf b6cd405286ed8635ece71c72f118e659f4ade3fb.tar.gz
WORKDIR /var/tmp/build/googletest-b6cd405286ed8635ece71c72f118e659f4ade3fb
RUN cmake -DCMAKE_BUILD_TYPE="Release" -DBUILD_SHARED_LIBS=yes -H. -Bcmake-out/googletest
RUN cmake --build cmake-out/googletest --target install -- -j ${NCPU}
RUN ldconfig
RUN find /usr/local -type d | xargs chmod 777
#   Install the Cloud Bigtable emulator and the Cloud Bigtable command-line
#   client.  They are used in the integration tests.
COPY . /var/tmp/ci
WORKDIR /var/tmp/downloads
RUN /var/tmp/ci/install-cloud-sdk.sh
#   Install Bazel because some of the builds need it.
RUN /var/tmp/ci/install-bazel.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
