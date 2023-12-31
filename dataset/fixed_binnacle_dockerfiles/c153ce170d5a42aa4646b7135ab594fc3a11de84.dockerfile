FROM ubuntu:xenial
LABEL maintainer="Luis Martinez de Bartolome <luism@jfrog.com>"
ENV PYENV_ROOT="/opt/pyenv" \
    PATH="/opt/pyenv/shims:${PATH}" \
    CXX="/usr/bin/g++" \
    CC="/usr/bin/gcc"
RUN dpkg --add-architecture i386 \
 && apt-get update -qq \
 && apt-get install --no-install-recommends dh-autoreconf=11 sudo=1.* build-essential=12.* wget=1.* git=1:2.* libc6-dev-i386=2.* g++-multilib=4:5.3.* libgmp-dev=2:6.* libmpfr-dev=3.* libmpc-dev=1.* libc6-dev=2.* nasm=2.* -qq -y \
 && apt-get install --no-install-recommends linux-libc-dev:i386 gcc-5=5.3.* gcc-5-base=5.3.* cpp-5=5.3.* libcc1-0=5.3.* libgcc-5-dev=5.3.* libstdc++6=5.3.* libgomp1=5.3.* libitm1=5.3.* libatomic1=5.3.* libasan2=5.3.* liblsan0=5.3.* libtsan0=5.3.* libubsan0=5.3.* libcilkrts5=5.3.* libmpx0=5.3.* libquadmath0=5.3.* gcc-5-multilib=5.3.* lib32gcc-5-dev=5.3.* libx32gcc-5-dev=5.3.* lib32gomp1=5.3.* libx32gomp1=5.3.* lib32itm1=5.3.* libx32itm1=5.3.* lib32atomic1=5.3.* libx32atomic1=5.3.* lib32asan2=5.3.* libx32asan2=5.3.* lib32ubsan0=5.3.* libx32ubsan0=5.3.* lib32cilkrts5=5.3.* libx32cilkrts5=5.3.* lib32mpx0=5.3.* lib32quadmath0=5.3.* libx32quadmath0=5.3.* lib32stdc++6=5.3.* libx32stdc++6=5.3.* libstdc++-5-dev=5.3.* -qq -y --allow-downgrades \
 && apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 autoconf-archive=20150925-1 g++-5=5.3.* libstdc++-5-dev=5.3.* g++-5-multilib=5.3.* lib32stdc++-5-dev=5.3.* libx32stdc++-5-dev=5.3.* ninja-build=1.* libffi-dev=3.* libssl-dev=1.* pkg-config=0.* subversion=1.* zlib1g-dev=1:1.* libbz2-dev=1.* libsqlite3-dev=3.* libreadline-dev=6.* xz-utils=5.* curl=7.* libncurses5-dev=6.* libncursesw5-dev=6.* liblzma-dev=5.* -qq -y --allow-downgrades \
 && ln -s /usr/bin/g++-5 /usr/bin/g++ \
 && rm -rf /var/lib/apt/lists/* \
 && wget -q --no-check-certificate https://cmake.org/files/v3.14/cmake-3.14.3-Linux-x86_64.tar.gz \
 && tar -xzf cmake-3.14.3-Linux-x86_64.tar.gz --exclude=bin/cmake-gui --exclude=doc/cmake --exclude=share/cmake-3.14/Help \
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
 && find /opt/pyenv -iname __pycache__ -print0 | xargs -0 rm -rf \
 && pip install pip==23.1 -q --upgrade --no-cache-dir \
 && pip install conan==2.0.4 conan-package-tools==0.39.0 -q --no-cache-dir \
 && chown -R conan:1001 /opt/pyenv \
 && update-alternatives --install /usr/bin/python python /opt/pyenv/shims/python 100 \
 && update-alternatives --install /usr/bin/python3 python3 /opt/pyenv/shims/python3 100 \
 && update-alternatives --install /usr/bin/pip pip /opt/pyenv/shims/pip 100 \
 && update-alternatives --install /usr/bin/pip3 pip3 /opt/pyenv/shims/pip3 100
USER conan
WORKDIR /home/conan
RUN mkdir -p /home/conan/.conan \
 && printf 'eval "$(pyenv init -)"\n' >> ~/.bashrc \
 && printf 'eval "$(pyenv virtualenv-init -)"\n' >> ~/.bashrc
# Please add your HEALTHCHECK here!!!
