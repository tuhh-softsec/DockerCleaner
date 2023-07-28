FROM ubuntu:16.04
RUN apt-get update -qq \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 bzip2=1.0.6-8ubuntu0.2 file=1:5.25-2ubuntu1.4 unzip=6.0-20ubuntu1.1 libtool=2.4.6-0.1 pkg-config=0.29.1-0ubuntu1 cmake=3.5.1-1ubuntu3 build-essential=12.1ubuntu2 automake=1:1.15-4ubuntu1 yasm=1.3.0-2 gettext=0.19.7-2ubuntu3.1 autopoint=0.19.7-2ubuntu3.1 vim=2:7.4.1689-3ubuntu1.5 python=2.7.12-1~16.04 git-svn=1:2.7.4-0ubuntu1.10 ninja-build=1.5.1-0.1ubuntu1 subversion=1.9.3-2ubuntu1.3 -qqy \
 && apt-get clean -y \
 && rm -rf /var/lib/apt/lists/*
RUN git config --global user.name "LLVM MinGW" \
 && git config --global user.email root@localhost
WORKDIR /build
ENV TOOLCHAIN_PREFIX="/opt/llvm-mingw"
ARG FULL_LLVM
#   Build LLVM
COPY build-llvm.sh ./
RUN ./build-llvm.sh $TOOLCHAIN_PREFIX
#   Strip the LLVM install output immediately. (This doesn't reduce the
#   total docker image size as long as it is in a separate RUN layer though,
#   but reduces build times if tweaking the contents of strip-llvm.sh.)
#   Most of the size of the docker image comes from the build directory that
#   we keep in any case.
COPY strip-llvm.sh ./
RUN ./strip-llvm.sh $TOOLCHAIN_PREFIX
ARG TOOLCHAIN_ARCHS="i686 x86_64 armv7 aarch64"
#   Install the usual $TUPLE-clang binaries
COPY wrappers/*.sh wrappers/*.c wrappers/*.h ./wrappers/
COPY install-wrappers.sh ./
RUN ./install-wrappers.sh $TOOLCHAIN_PREFIX
#   Build MinGW-w64
COPY build-mingw-w64.sh ./
RUN ./build-mingw-w64.sh $TOOLCHAIN_PREFIX
#   Build compiler-rt
COPY build-compiler-rt.sh ./
RUN ./build-compiler-rt.sh $TOOLCHAIN_PREFIX
#   Build mingw-w64's extra libraries
COPY build-mingw-w64-libraries.sh ./
RUN ./build-mingw-w64-libraries.sh $TOOLCHAIN_PREFIX
#   Build C test applications
ENV PATH="$TOOLCHAIN_PREFIX/bin:$PATH"
COPY test/*.c test/*.h ./test/
RUN cd test \
 && for arch in $TOOLCHAIN_ARCHS; do mkdir -p $arch \
 && for test in hello hello-tls crt-test setjmp; do $arch-w64-mingw32-clang $test.c -o $arch/$test.exe || exit 1 ; done ;for test in autoimport-lib; do $arch-w64-mingw32-clang $test.c -shared -o $arch/$test.dll -Wl,--out-implib,$arch/lib$test.dll.a || exit 1 ; done ;for test in autoimport-main; do $arch-w64-mingw32-clang $test.c -o $arch/$test.exe -L$arch -l${test%-main}-lib || exit 1 ; done ; done
#   Build libunwind/libcxxabi/libcxx
COPY build-libcxx.sh ./
RUN ./build-libcxx.sh $TOOLCHAIN_PREFIX
#   Build C++ test applications
COPY test/*.cpp ./test/
RUN cd test \
 && for arch in $TOOLCHAIN_ARCHS; do mkdir -p $arch \
 && for test in hello-cpp hello-exception tlstest-main exception-locale exception-reduced; do $arch-w64-mingw32-clang++ $test.cpp -o $arch/$test.exe || exit 1 ; done ;for test in tlstest-lib; do $arch-w64-mingw32-clang++ $test.cpp -shared -o $arch/$test.dll || exit 1 ; done ; done
#   Build sanitizers. Ubsan includes <typeinfo> from the C++ headers, so
#   we need to build this after libcxx.
RUN ./build-compiler-rt.sh $TOOLCHAIN_PREFIX --build-sanitizers
#   Sanitizers on windows only support x86.
RUN cd test \
 && for arch in $TOOLCHAIN_ARCHS; do case $arch in (i686|x86_64) ;;(*) continue ;; esac \
 && for test in stacksmash; do $arch-w64-mingw32-clang $test.c -o $arch/$test-asan.exe -fsanitize=address -g -gcodeview -Wl,-pdb,$arch/$test-asan.pdb || exit 1 ; done ;for test in ubsan; do $arch-w64-mingw32-clang $test.c -o $arch/$test.exe -fsanitize=undefined || exit 1 ; done ; done
#   Build libssp
COPY build-libssp.sh libssp-Makefile ./
RUN ./build-libssp.sh $TOOLCHAIN_PREFIX
RUN cd test \
 && for arch in $TOOLCHAIN_ARCHS; do mkdir -p $arch \
 && for test in stacksmash; do $arch-w64-mingw32-clang $test.c -o $arch/$test.exe -fstack-protector-strong || exit 1 ; done ; done
RUN cd test \
 && for arch in $TOOLCHAIN_ARCHS; do cp $TOOLCHAIN_PREFIX/$arch-w64-mingw32/bin/*.dll $arch || exit 1 ; done
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
