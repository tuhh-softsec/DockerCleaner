FROM ubuntu:artful
LABEL maintainer="Luis Martinez de Bartolome <luism@jfrog.com>"
ENV LLVM_VERSION="3.9" \
    CC="clang" \
    CXX="clang++" \
    CMAKE_C_COMPILER="clang" \
    CMAKE_CXX_COMPILER="clang++" \
    PYENV_ROOT="/opt/pyenv" \
    PATH="/opt/pyenv/shims:${PATH}"
COPY sources.list /etc/apt/sources.list
RUN dpkg --add-architecture i386 \
 && ls -lhR /etc/apt/ \
 && apt-get update -qq \
 && apt-get install --no-install-recommends sudo=1.8.20p2-1ubuntu1 wget=1.19.1-3ubuntu1 git=1:2.14.1-1ubuntu4 g++-multilib=4:7.2.0-1ubuntu1 make=4.1-9.1 libc6-dev-i386=2.26-0ubuntu2.1 libgmp-dev=2:6.1.2+dfsg-1 libmpfr-dev=3.1.6-1 libmpc-dev=1.0.3-2 nasm=2.13.01-2 dh-autoreconf=14 libffi-dev=3.2.1-6 ninja-build=1.7.2-3 libc++-dev=3.9.1-3 libc++-dev:i386=3.9.1-3 libc++abi-dev=3.9.1-3 libc++abi-dev:i386=3.9.1-3 pkg-config=0.29.1-0ubuntu2 subversion=1.9.7-2ubuntu1 ca-certificates=20170717 autoconf-archive clang-${LLVM_VERSION}=1:3.9.1-* llvm-${LLVM_VERSION}=1:3.9.1-* llvm-${LLVM_VERSION}-dev=1:3.9.1-* llvm-${LLVM_VERSION}-runtime=1:3.9.1-* llvm=1:4.0-* libssl-dev=1.0.2* zlib1g-dev=1:1.* libbz2-dev=1.* libsqlite3-dev=3.* libreadline-dev=7.* xz-utils=5.* curl=7.* libncurses5-dev=6.* libncursesw5-dev=6.* liblzma-dev=5.* -qq -y \
 && update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-${LLVM_VERSION} 100 \
 && update-alternatives --install /usr/bin/clang clang /usr/bin/clang-${LLVM_VERSION} 100 \
 && update-alternatives --install /usr/bin/cc cc /usr/bin/clang-${LLVM_VERSION} 100 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-${LLVM_VERSION} 100 \
 && update-alternatives --install /usr/bin/cpp cpp /usr/bin/clang++-${LLVM_VERSION} 100 \
 && ln -s /usr/include/locale.h /usr/include/xlocale.h \
 && rm -rf /var/lib/apt/lists/* \
 && wget -q --no-check-certificate https://cmake.org/files/v3.14/cmake-3.14.3-Linux-x86_64.tar.gz \
 && tar -xzf cmake-3.14.3-Linux-x86_64.tar.gz --exclude=bin/cmake-gui --exclude=doc/cmake --exclude=share/cmake-3.12/Help \
 && cp -fR cmake-3.14.3-Linux-x86_64/* /usr \
 && rm -rf cmake-3.14.3-Linux-x86_64 \
 && rm cmake-3.14.3-Linux-x86_64.tar.gz \
 && groupadd 1001 -g 1001 \
 && groupadd 1000 -g 1000 \
 && groupadd 2000 -g 2000 \
 && groupadd 999 -g 999 \
 && useradd -ms /bin/bash conan -g 1001 -G 1000,2000,999 \
 && printf "conan:conan" | chpasswd \
 && adduser conan sudo \
 && printf "conan ALL= NOPASSWD: ALL\n" >> /etc/sudoers \
 && wget --no-check-certificate --quiet -O /tmp/pyenv-installer https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer \
 && chmod +x /tmp/pyenv-installer \
 && /tmp/pyenv-installer \
 && rm /tmp/pyenv-installer \
 && update-alternatives --install /usr/bin/pyenv pyenv /opt/pyenv/bin/pyenv 100 \
 && PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install 3.7.1 \
 && pyenv global 3.7.1 \
 && pip install pip -q --upgrade --no-cache-dir \
 && pip install conan conan-package-tools -q --no-cache-dir \
 && chown -R conan:1001 /opt/pyenv \
 && find /opt/pyenv -iname __pycache__ -print0 | xargs -0 rm -rf \
 && update-alternatives --install /usr/bin/python python /opt/pyenv/shims/python 100 \
 && update-alternatives --install /usr/bin/python3 python3 /opt/pyenv/shims/python3 100 \
 && update-alternatives --install /usr/bin/pip pip /opt/pyenv/shims/pip 100 \
 && update-alternatives --install /usr/bin/pip3 pip3 /opt/pyenv/shims/pip3 100
USER conan
WORKDIR /home/conan
RUN mkdir -p /home/conan/.conan \
 && printf 'eval "$(pyenv init -)"\n' >> ~/.bashrc \
 && printf 'eval "$(pyenv virtualenv-init -)"\n' >> ~/.bashrc
