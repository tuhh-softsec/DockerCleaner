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
ARG DISTRO_VERSION=xenial
FROM ubuntu:${DISTRO_VERSION} AS devtools
#   Please keep the formatting in these commands, it is optimized to cut & paste
#   into the README.md file.
#  # [START INSTALL.md]
#   Install the minimal development tools:
#  # [START README.md]
#   ```bash
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential cmake git gcc g++ cmake libcurl4-openssl-dev libssl-dev make pkg-config tar wget zlib1g-dev -y
#   ```
#  # [END README.md]
FROM devtools
COPY ci/install-retry.sh /retry3
#  # [START IGNORED]
#   Verify that the tools above are enough to compile google-cloud-cpp when using
#   external projects.
WORKDIR /home/build/external
COPY . /home/build/external
RUN cmake -H. -Bcmake-out -DCMAKE_BUILD_TYPE=Debug
RUN cmake --build cmake-out -- -j $( nproc ;)
RUN (cd cmake-out \
 && ctest --output-on-failure )
#  # [END IGNORED]
#   #### crc32c
#   There is no Ubuntu-16.04 package for this library. To install it use:
#   ```bash
WORKDIR /var/tmp/build
RUN /retry3 wget -q https://github.com/google/crc32c/archive/1.0.6.tar.gz
RUN tar -xf 1.0.6.tar.gz
WORKDIR /var/tmp/build/crc32c-1.0.6
RUN cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=yes -DCRC32C_BUILD_TESTS=OFF -DCRC32C_BUILD_BENCHMARKS=OFF -DCRC32C_USE_GLOG=OFF -H. -Bcmake-out/crc32c
RUN cmake --build cmake-out/crc32c --target install -- -j $( nproc ;)
RUN ldconfig
#   ```
#   #### Protobuf
#   While protobuf-2.6 is distributed with Ubuntu-16.04, the Google Cloud Plaform
#   proto files require more recent versions (circa 3.4.x). To manually install a
#   more recent version use:
#   ```bash
WORKDIR /var/tmp/build
RUN /retry3 wget -q https://github.com/google/protobuf/archive/v3.6.1.tar.gz
RUN tar -xf v3.6.1.tar.gz
WORKDIR /var/tmp/build/protobuf-3.6.1/cmake
RUN cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=yes -Dprotobuf_BUILD_TESTS=OFF -H. -Bcmake-out
RUN cmake --build cmake-out --target install -- -j $( nproc ;)
RUN ldconfig
#   ```
#   #### c-ares
#   Recent versions of gRPC require c-ares >= 1.11, while Ubuntu-16.04
#   distributes c-ares-1.10. We need some additional development tools to compile
#   this library:
#   ```bash
RUN /retry3 apt update \
 && /retry3 apt install -y automake libtool
#   ```
#   After installing these tools we can manually install a newer version
#   of c-ares:
#   ```bash
WORKDIR /var/tmp/build
RUN /retry3 wget -q https://github.com/c-ares/c-ares/archive/cares-1_14_0.tar.gz
RUN tar -xf cares-1_14_0.tar.gz
WORKDIR /var/tmp/build/c-ares-cares-1_14_0
RUN ./buildconf \
 && ./configure \
 && make -j $( nproc ;)
RUN make install
RUN ldconfig
#   ```
#   #### gRPC
#   Ubuntu-16.04 does not provide packages for the C++ gRPC bindings, install the
#   library manually:
#   ```bash
WORKDIR /var/tmp/build
RUN /retry3 wget -q https://github.com/grpc/grpc/archive/v1.19.1.tar.gz
RUN tar -xf v1.19.1.tar.gz
WORKDIR /var/tmp/build/grpc-1.19.1
RUN make -j $( nproc ;)
RUN make install
RUN ldconfig
#   ```
#   #### google-cloud-cpp
#   Finally we can install `google-cloud-cpp`.
#   ```bash
WORKDIR /home/build/google-cloud-cpp
COPY . /home/build/google-cloud-cpp
RUN cmake -H. -Bcmake-out -DGOOGLE_CLOUD_CPP_DEPENDENCY_PROVIDER=package -DGOOGLE_CLOUD_CPP_GMOCK_PROVIDER=external
RUN cmake --build cmake-out -- -j $( nproc ;)
WORKDIR /home/build/google-cloud-cpp/cmake-out
RUN ctest --output-on-failure
RUN cmake --build . --target install
#   ```
#  # [END INSTALL.md]
#   Verify that the installed files are actually usable
WORKDIR /home/build/test-install-plain-make
COPY ci/test-install /home/build/test-install-plain-make
RUN make
WORKDIR /home/build/test-install-cmake-bigtable
COPY ci/test-install/bigtable /home/build/test-install-cmake-bigtable
RUN env -u PKG_CONFIG_PATH cmake -H. -Bcmake-out
RUN cmake --build cmake-out -- -j $( nproc ;)
WORKDIR /home/build/test-install-cmake-storage
COPY ci/test-install/storage /home/build/test-install-cmake-storage
RUN env -u PKG_CONFIG_PATH cmake -H. -Bcmake-out
RUN cmake --build cmake-out -- -j $( nproc ;)
WORKDIR /home/build/test-submodule
COPY ci/test-install /home/build/test-submodule
COPY . /home/build/test-submodule/submodule/google-cloud-cpp
RUN cmake -Hsubmodule -Bcmake-out
RUN cmake --build cmake-out -- -j $( nproc ;)
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
