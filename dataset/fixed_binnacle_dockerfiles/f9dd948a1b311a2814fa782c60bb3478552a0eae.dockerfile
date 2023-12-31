#   Copyright 2015 gRPC authors.
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
FROM debian:jessie
#   Install Git and basic packages.
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf autotools-dev build-essential bzip2 ccache curl dnsutils gcc gcc-multilib git golang gyp lcov libc6 libc6-dbg libc6-dev libgtest-dev libtool make perl strace python-dev python-setuptools python-yaml telnet unzip wget zip -y \
 && apt-get clean
#  ================
#   Build profiling
RUN apt-get update \
 && apt-get install --no-install-recommends time -y \
 && apt-get clean
#   Google Cloud platform API libraries
RUN apt-get update \
 && apt-get install --no-install-recommends python-pip -y \
 && apt-get clean
RUN pip install google-api-python-client==2.85.0 oauth2client==4.1.3 --upgrade
#  ====================
#   Python dependencies
#   Install dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends python-all-dev python3-all-dev python-pip -y
#   Install Python packages from PyPI
RUN pip install pip==10.0.1 --upgrade
RUN pip install virtualenv==20.21.0
RUN pip install futures==2.2.0 enum34==1.0.4 protobuf==3.5.2.post1 six==1.10.0 twisted==17.5.0
#  =================
#   C++ dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends libgflags-dev libgtest-dev libc++-dev clang -y \
 && apt-get clean
#  =================
#   Use cmake 3.6 from jessie-backports
#   should only be used for images based on debian jessie.
RUN echo "deb http://archive.debian.org/debian jessie-backports main" | tee /etc/apt/sources.list.d/jessie-backports.list
RUN echo 'Acquire::Check-Valid-Until "false";' > /etc/apt/apt.conf
RUN sed -i '/deb http:\/\/deb.debian.org\/debian jessie-updates main/d' /etc/apt/sources.list
RUN apt-get update \
 && apt-get install --no-install-recommends jessie-backports cmake -t -y \
 && apt-get clean
#  =================
#   Update clang to a version with improved tsan and fuzzing capabilities
RUN git clone -n -b release_38 http://llvm.org/git/llvm.git \
 && cd llvm \
 && git checkout ad57503 \
 && cd ..
RUN git clone -n -b release_38 http://llvm.org/git/clang.git \
 && cd clang \
 && git checkout ad2c56e \
 && cd ..
RUN git clone -n -b release_38 http://llvm.org/git/compiler-rt.git \
 && cd compiler-rt \
 && git checkout 3176922 \
 && cd ..
RUN git clone -n -b release_38 http://llvm.org/git/clang-tools-extra.git \
 && cd clang-tools-extra \
 && git checkout c288525 \
 && cd ..
RUN git clone -n -b release_38 http://llvm.org/git/libcxx.git \
 && cd libcxx \
 && git checkout fda3549 \
 && cd ..
RUN git clone -n -b release_38 http://llvm.org/git/libcxxabi.git \
 && cd libcxxabi \
 && git checkout 8d4e51d \
 && cd ..
RUN mv clang llvm/tools
RUN mv compiler-rt llvm/projects
RUN mv clang-tools-extra llvm/tools/clang/tools
RUN mv libcxx llvm/projects
RUN mv libcxxabi llvm/projects
RUN mkdir llvm-build
RUN cd llvm-build \
 && cmake -DCMAKE_BUILD_TYPE:STRING=Release -DCMAKE_INSTALL_PREFIX:STRING=/usr -DLLVM_TARGETS_TO_BUILD:STRING=X86 ../llvm
RUN make -C llvm-build -j 12 \
 && make -C llvm-build install \
 && rm -rf llvm-build
RUN mkdir /var/local/jenkins
RUN clang++ -c -g -O2 -std=c++11 llvm/lib/Fuzzer/*.cpp -IFuzzer
RUN ar ruv libFuzzer.a Fuzzer*.o
RUN mv libFuzzer.a /usr/lib
RUN rm -f Fuzzer*.o
#   Define the default command.
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
