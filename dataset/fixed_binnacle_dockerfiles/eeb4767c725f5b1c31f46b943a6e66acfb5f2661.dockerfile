FROM ubuntu:trusty@sha256:ed49036f63459d6e5ed6c0f238f5e94c3a0c70d24727c793c48fded60f70aa96
MAINTAINER Everest Munro-Zeisberger
WORKDIR /root
#  #######################
#   SETUP ENV & VERSIONS #
#  #######################
#   Versions:
ENV AFL_VERSION="2.52b"
ENV RUBY_VERSION="2.3.3"
ENV GO_DEP_VERSION="0.4.1"
#   Environment Variables:
ENV GOPATH="/root/go/"
ENV GOBIN="/root/go/bin"
#  ###############
#   INSTALL DEPS #
#  ###############
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y )
RUN apt-add-repository -y ppa:rael-gc/rvm
RUN apt-add-repository -y ppa:gophers/archive
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends git=1:1.9.1-1ubuntu0.10 wget=1.15-1ubuntu1.14.04.5 gcc=4:4.8.2-1ubuntu6 autoconf=2.69-6 make=3.81-8.2ubuntu3 bison=2:3.0.2.dfsg-2 libssl-dev=1.0.1f-1ubuntu2.27 libreadline-dev=6.3-4ubuntu2 zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 pkg-config=0.26-1ubuntu4 gcc=4:4.8.2-1ubuntu6 clang=1:3.4-0ubuntu1 llvm=1:3.4-0ubuntu1 rvm watch nodejs=0.10.25~dfsg2-2ubuntu1.2 npm=1.3.10~dfsg-1 supervisor=3.0b2-1ubuntu0.1 cmake=2.8.12.2-0ubuntu3 gdb=7.7.1-0ubuntu5~14.04.3 python-virtualenv=1.11.4-1ubuntu1 golang-1.9-go cython=0.20.1+git90-g0e6e38e-1ubuntu2 build-essential=11.6ubuntu6 libgtk2.0-dev=2.24.23-0ubuntu1.4 libtbb-dev=4.2~20130725-1.1ubuntu1 python-dev=2.7.5-5ubuntu3 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 libjasper-dev=1.900.1-14ubuntu3.5 libjpeg-dev=8c-2ubuntu8 libpng-dev libtiff-dev libavcodec-dev=6:9.20-0ubuntu0.14.04.1 libavutil-dev=6:9.20-0ubuntu0.14.04.1 libavformat-dev=6:9.20-0ubuntu0.14.04.1 libswscale-dev=6:9.20-0ubuntu0.14.04.1 libdc1394-22-dev=2.2.1-2ubuntu2 libv4l-dev=1.0.1-1 -y )
#  ##########################
#   AFL Compilation & Setup #
#  ##########################
#   Download AFL and uncompress
RUN wget http://lcamtuf.coredump.cx/afl/releases/afl-$AFL_VERSION.tgz
RUN tar -xvf afl-$AFL_VERSION.tgz
RUN mv afl-$AFL_VERSION afl
#   Inject our own AFL config header file
RUN rm /root/afl/config.h
COPY ./config/afl_config/config.h /root/afl/config.h
#   Compile both standard gcc, clang etc as well as afl-clang-fast, used for
#   faster & persistent test harnesses. Also build the afl-fuzz binary
RUN cd ~/afl \
 && make
RUN cd ~/afl/llvm_mode \
 && make
#   Environment Setup
ENV AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES="1"
#   Install py-afl-fuzz (for fuzzing python libraries)
RUN git clone https://github.com/jwilk/python-afl.git
RUN cd python-afl \
 && python setup.py install
#   Compile ruby from sources with afl, and setup cflags to access instrumented
#   ruby headers (useful for ruby library fuzzing with C harneses)
RUN CC=~/afl/afl-clang-fast /usr/share/rvm/bin/rvm install --disable-binary $RUBY_VERSION
ENV LD_LIBRARY_PATH="LD_LIBRARY_PATH=/usr/share/rvm/rubies/ruby-$RUBY_VERSION/lib"
ENV PATH="/usr/share/rvm/rubies/ruby-$RUBY_VERSION/bin:$PATH"
COPY ./config/ ./config/
RUN PKG_CONFIG_PATH=/usr/share/rvm/rubies/ruby-$RUBY_VERSION/lib/pkgconfig pkg-config --cflags --libs ruby-2.3 > ~/config/afl-ruby-flags
#   File structure setup
RUN mkdir ~/fuzz_out
RUN mkdir ~/fuzz_in
#  ###########################
#  SIDECAR & MONITORING SETUP#
#  ###########################
#   Setup logging and scripts & install Go libs
RUN mkdir /root/logs
WORKDIR /root/go/src/maxfuzz/fuzzer-base
RUN wget https://github.com/golang/dep/releases/download/v$GO_DEP_VERSION/dep-linux-amd64
RUN mv dep-linux-amd64 /usr/local/bin/dep
ENV PATH="/usr/lib/go-1.9/bin/:$PATH"
RUN chmod +x /usr/local/bin/dep
RUN go get -u github.com/dvyukov/go-fuzz/...
#   Copy Go files into container & compile binaries
RUN mkdir -p /root/go/src/maxfuzz/fuzzer-base
WORKDIR /root/go/src/maxfuzz/fuzzer-base
COPY ./cmd /root/go/src/maxfuzz/fuzzer-base/cmd
COPY ./internal /root/go/src/maxfuzz/fuzzer-base/internal
COPY ./Gopkg.lock /root/go/src/maxfuzz/fuzzer-base/Gopkg.lock
COPY ./Gopkg.toml /root/go/src/maxfuzz/fuzzer-base/Gopkg.toml
COPY ./Makefile /root/go/src/maxfuzz/fuzzer-base/Makefile
RUN make \
 && make install
WORKDIR /root
#   Setup sidecar webapp
COPY ./sidecar ./sidecar
RUN curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 -y )
RUN cd sidecar \
 && npm install log@6.3.1
#  ################################
#   FINAL SETUP & COPYING SCRIPTS #
#  ################################
RUN mkdir fuzzer-files
COPY ./scripts ./scripts
COPY ./fuzzer-files/base ./fuzzer-files/base
RUN chmod -R 755 /root/fuzzer-files
RUN chmod 755 /root/scripts/reproduce_stdin
RUN echo "export GIT_SHA=test_environment" >> /root/fuzzer-files/base/environment
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
