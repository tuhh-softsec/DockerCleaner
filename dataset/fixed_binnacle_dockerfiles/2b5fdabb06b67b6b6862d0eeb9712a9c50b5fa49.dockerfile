FROM ubuntu:14.04
MAINTAINER Dan Liew <daniel.liew@imperial.ac.uk>
#   FIXME: Docker doesn't currently offer a way to
#   squash the layers from within a Dockerfile so
#   the resulting image is unnecessarily large!
ENV LLVM_VERSION="3.4" \
    SOLVERS="STP:Z3" \
    STP_VERSION="master" \
    DISABLE_ASSERTIONS="0" \
    ENABLE_OPTIMIZED="1" \
    KLEE_UCLIBC="klee_uclibc_v1.0.0" \
    KLEE_SRC="/home/klee/klee_src" \
    COVERAGE="0" \
    BUILD_DIR="/home/klee/klee_build"
RUN apt-get update \
 && apt-get install --no-install-recommends llvm=1:3.4-0ubuntu1 libcap-dev=1:2.24-0ubuntu2 git=1:1.9.1-1ubuntu0.10 subversion=1.8.8-1ubuntu3.3 cmake3=3.5.1-1ubuntu3~14.04.1 make=3.81-8.2ubuntu3 libboost-program-options-dev=1.54.0.1ubuntu1 python3=3.4.0-0ubuntu2 python3-dev=3.4.0-0ubuntu2 python3-pip=1.5.4-1ubuntu4 perl=5.18.2-2ubuntu1.7 flex=2.5.35-10.1ubuntu2 bison=2:3.0.2.dfsg-2 libncurses-dev zlib1g-dev=1:1.2.8.dfsg-1ubuntu1.1 patch=2.7.1-4ubuntu2.4 wget=1.15-1ubuntu1.14.04.5 unzip=6.0-9ubuntu1.5 binutils=2.24-5ubuntu14.2 clang-${LLVM_VERSION} llvm-${LLVM_VERSION} llvm-${LLVM_VERSION}-dev llvm-${LLVM_VERSION}-runtime -y \
 && pip3 install -U lit tabulate \
 && update-alternatives --install /usr/bin/python python /usr/bin/python3 50 \
 && (wget -O - http://download.opensuse.org/repositories/home:delcypher:z3/xUbuntu_14.04/Release.key | apt-key add - ) \
 && echo 'deb http://download.opensuse.org/repositories/home:/delcypher:/z3/xUbuntu_14.04/ /' >> /etc/apt/sources.list.d/z3.list \
 && apt-get update
#   Create ``klee`` user for container with password ``klee``.
#   and give it password-less sudo access (temporarily so we can use the TravisCI scripts)
RUN useradd -m klee \
 && echo klee:klee | chpasswd \
 && cp /etc/sudoers /etc/sudoers.bak \
 && echo 'klee ALL=(root) NOPASSWD: ALL' >> /etc/sudoers
USER klee
WORKDIR /home/klee
#   Copy across source files needed for build
RUN mkdir ${KLEE_SRC}
COPY configure LICENSE.TXT Makefile Makefile.* README.md MetaSMT.mk TODO.txt ${KLEE_SRC}/
COPY .travis ${KLEE_SRC}/.travis/
COPY autoconf ${KLEE_SRC}/autoconf/
COPY docs ${KLEE_SRC}/docs/
COPY include ${KLEE_SRC}/include/
COPY lib ${KLEE_SRC}/lib/
COPY runtime ${KLEE_SRC}/runtime/
COPY scripts ${KLEE_SRC}/scripts/
COPY test ${KLEE_SRC}/test/
COPY tools ${KLEE_SRC}/tools/
COPY unittests ${KLEE_SRC}/unittests/
COPY utils ${KLEE_SRC}/utils/
#   Set klee user to be owner
RUN sudo chown --recursive klee: ${KLEE_SRC}
#   Create build directory
RUN mkdir -p ${BUILD_DIR}
#   Build/Install SMT solvers (use TravisCI script)
RUN cd ${BUILD_DIR} \
 && ${KLEE_SRC}/.travis/solvers.sh
#   Install testing utils (use TravisCI script)
RUN cd ${BUILD_DIR} \
 && mkdir testing-utils \
 && cd testing-utils \
 && ${KLEE_SRC}/.travis/testing-utils.sh
#   FIXME: This is a nasty hack so KLEE's configure and build finds
#   LLVM's headers file, libraries and tools
RUN sudo mkdir -p /usr/lib/llvm-${LLVM_VERSION}/build/Release/bin \
 && sudo ln -s /usr/bin/llvm-config /usr/lib/llvm-${LLVM_VERSION}/build/Release/bin/llvm-config \
 && sudo ln -s /usr/bin/llvm-dis /usr/lib/llvm-${LLVM_VERSION}/build/Release/bin/llvm-dis \
 && sudo ln -s /usr/bin/llvm-as /usr/lib/llvm-${LLVM_VERSION}/build/Release/bin/llvm-as \
 && sudo ln -s /usr/bin/llvm-link /usr/lib/llvm-${LLVM_VERSION}/build/Release/bin/llvm-link \
 && sudo ln -s /usr/bin/llvm-ar /usr/lib/llvm-${LLVM_VERSION}/build/Release/bin/llvm-ar \
 && sudo ln -s /usr/bin/opt /usr/lib/llvm-${LLVM_VERSION}/build/Release/bin/opt \
 && sudo ln -s /usr/bin/lli /usr/lib/llvm-${LLVM_VERSION}/build/Release/bin/lli \
 && sudo mkdir -p /usr/lib/llvm-${LLVM_VERSION}/build/include \
 && sudo ln -s /usr/include/llvm-${LLVM_VERSION}/llvm /usr/lib/llvm-${LLVM_VERSION}/build/include/llvm \
 && sudo ln -s /usr/include/llvm-c-${LLVM_VERSION}/llvm-c /usr/lib/llvm-${LLVM_VERSION}/build/include/llvm-c \
 && for static_lib in /usr/lib/llvm-${LLVM_VERSION}/lib/*.a; do sudo ln -s ${static_lib} /usr/lib/`basename ${static_lib} ` ; done
#   FIXME: This is **really gross**. The Official Ubuntu LLVM packages don't ship
#   with ``FileCheck`` or the ``not`` tools so we have to hack building these
#   into KLEE's build system in order for the tests to pass
RUN cd ${KLEE_SRC}/tools \
 && for tool in FileCheck not; do svn export http://llvm.org/svn/llvm-project/llvm/branches/release_34/utils/${tool} ${tool} ;sed -i 's/^USEDLIBS.*$/LINK_COMPONENTS = support/' ${tool}/Makefile ; done \
 && sed -i '0,/^PARALLEL_DIRS/a PARALLEL_DIRS += FileCheck not' Makefile
#   FIXME: The current TravisCI script expects clang-${LLVM_VERSION} to exist
RUN sudo ln -s /usr/bin/clang /usr/bin/clang-${LLVM_VERSION} \
 && sudo ln -s /usr/bin/clang++ /usr/bin/clang++-${LLVM_VERSION}
#   Build KLEE (use TravisCI script)
RUN cd ${BUILD_DIR} \
 && ${KLEE_SRC}/.travis/klee.sh
#   Revoke password-less sudo and Set up sudo access for the ``klee`` user so it
#   requires a password
USER root
RUN mv /etc/sudoers.bak /etc/sudoers \
 && echo 'klee ALL=(root) ALL' >> /etc/sudoers
USER klee
#   Add KLEE binary directory to PATH
RUN echo 'export PATH=$PATH:'${BUILD_DIR}'/klee/Release+Asserts/bin' >> /home/klee/.bashrc
#   Link klee to /usr/bin so that it can be used by docker run
USER root
RUN for exec in ${BUILD_DIR}/klee/Release+Asserts/bin/*; do ln -s ${exec} /usr/bin/`basename ${exec} ` ; done
#   Link klee to the libkleeRuntest library needed by docker run
RUN ln -s ${BUILD_DIR}/klee/Release+Asserts/lib/libkleeRuntest.so /usr/lib/libkleeRuntest.so.1.0
USER klee
# Please add your HEALTHCHECK here!!!
