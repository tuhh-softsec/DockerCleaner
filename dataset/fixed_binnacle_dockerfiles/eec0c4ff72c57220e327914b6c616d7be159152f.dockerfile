#   Dockerfile used for regular linux builds.
FROM ubuntu:16.04
RUN apt-get update \
 && apt-get install --no-install-recommends gcc=4:5.3.1-1ubuntu1 alien=8.95 fakeroot=1.20.2-1ubuntu1 rpm=4.12.0.1+dfsg1-3build3 git=1:2.7.4-0ubuntu1.10 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 desktop-file-utils=0.22-1ubuntu5.2 wget=1.17.1-1ubuntu1.5 sudo=1.8.16-0ubuntu1.10 libgstreamer1.0-dev=1.8.3-1~ubuntu0.1 libgstreamer-plugins-base1.0-dev=1.8.3-1ubuntu0.3 libgstreamer-plugins-base0.10-0=0.10.36-2ubuntu0.2 libgstreamer0.10-0=0.10.36-1.5ubuntu1 libgstreamer0.10-dev=0.10.36-1.5ubuntu1 autoconf2.13=2.13-67 build-essential=12.1ubuntu2 ccache=3.2.4-1 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 unzip=6.0-20ubuntu1.1 uuid=1.6.2-1.5build2 zip=3.0-11 libasound2-dev=1.1.0-0ubuntu1 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libdbus-1-dev=1.10.6-1ubuntu3.6 libdbus-glib-1-dev=0.106-1 libgconf2-dev=3.2.6-3ubuntu6 libgtk-3-dev=3.18.9-1ubuntu3.3 libgtk2.0-dev=2.24.30-1ubuntu1.16.04.2 libiw-dev=30~pre9-8ubuntu1 libnotify-dev=0.7.6-2svn1 libpulse-dev=1:8.0-0ubuntu3.15 libx11-xcb-dev=2:1.6.3-1ubuntu2.2 libxt-dev=1:1.1.5-0ubuntu1 mesa-common-dev=18.0.5-0ubuntu0~16.04.1 python-dbus=1.2.0-3 xvfb=2:1.18.4-0ubuntu0.12 yasm=1.3.0-2 apt-transport-https=1.2.35 -y
RUN pip install awscli==1.27.114
RUN echo "deb http://repo.aptly.info/ squeeze main" > /etc/apt/sources.list.d/aptly.list; apt-key adv --keyserver pool.sks-keyservers.net --recv-keys ED75B5A4483DA07C ; apt-get update ; apt-get install --no-install-recommends aptly=0.9.6-1 -y
ENV RUSTUP_HOME="/usr/local/rustup" \
    CARGO_HOME="/usr/local/cargo" \
    PATH="/usr/local/cargo/bin:$PATH"
RUN echo "deb http://ppa.launchpad.net/mercurial-ppa/releases/ubuntu xenial main" > /etc/apt/sources.list.d/mercurial.list; apt-key adv --keyserver pool.sks-keyservers.net --recv-keys 41BD8711B1F0EC2B0D85B91CF59CE3A8323293EE ; apt-get update ; apt-get install --no-install-recommends mercurial=3.7.3-1ubuntu1.2 -y
RUN wget -qO- https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
RUN echo "deb https://deb.nodesource.com/node_10.x xenial main" > /etc/apt/sources.list.d/nodesource.list; apt-get update ; apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y
RUN set -eux ; dpkgArch="$( dpkg --print-architecture ;)" ; case "${dpkgArch##*-}" in (amd64) rustArch='x86_64-unknown-linux-gnu' ; rustupSha256='5a38dbaf7ab2e4335a3dfc42698a5b15e7167c93b0b06fc95f53c1da6379bf1a' ;;(armhf) rustArch='armv7-unknown-linux-gnueabihf' ; rustupSha256='67a98a67f7f7bf19c5cde166499acb8299f2f8fa88c155093df53b66da1f512a' ;;(arm64) rustArch='aarch64-unknown-linux-gnu' ; rustupSha256='82fe368c4ebf1683d57e137242793a4417042639aace8bd514601db7d79d3645' ;;(i386) rustArch='i686-unknown-linux-gnu' ; rustupSha256='7a1c085591f6c1305877919f8495c04a1c97546d001d1357a7a879cedea5afbb' ;;(*) echo "unsupported architecture: ${dpkgArch}" >&2; exit 1 ;; esac ; url="https://static.rust-lang.org/rustup/archive/1.7.0/${rustArch}/rustup-init" ; wget "$url" ; echo "${rustupSha256} *rustup-init" | sha256sum -c - ; chmod +x rustup-init ; ./rustup-init -y --no-modify-path --default-toolchain 1.32.0 ; rm rustup-init ; chmod -R a+w $RUSTUP_HOME $CARGO_HOME ; rustup --version ; cargo --version ; rustc --version
RUN cargo install --version 0.8.2 cbindgen
ARG uid
ARG gid
ARG user
ENV SHELL="/bin/bash"
RUN groupadd $user -g $gid \
 && useradd -ms /bin/bash $user -u $uid -g $gid \
 && usermod -aG sudo $user
#   Enable passwordless sudo for users under the "sudo" group
RUN sed -i.bkp -e 's/%sudo\s\+ALL=(ALL\(:ALL\)\?)\s\+ALL/%sudo ALL=NOPASSWD:ALL/g' /etc/sudoers
RUN mkdir /builds
USER $user
ENV CLANG_HOME="/home/$user/clang/clang+llvm-6.0.0-x86_64-linux-gnu-ubuntu-16.04/"
ENV GCC_VERSION="6.0.0"
ENV CXX="$CLANG_HOME/bin/clang++"
ENV CC="$CLANG_HOME/bin/clang"
ENV LLVM_CONFIG="$CLANG_HOME/bin/llvm-config"
SHELL ["/bin/bash", "-l", "-c"]
#  Install CLang
RUN mkdir -p /home/$user/clang ; cd /home/$user/clang ; wget --output-document=clang.tar.xz --quiet "https://repository.cliqz.com/dist/android/artifacts/clang/clang%2Bllvm-6.0.0-x86_64-linux-gnu-ubuntu-16.04.tar.xz" ; tar xf clang.tar.xz ; echo 'export PATH=$CLANG_HOME/bin:$PATH' >> ~/.bashrc; echo 'export LD_LIBRARY_PATH=$CLANG_HOME/lib:LD_LIBRARY_PATH' >> ~/.bashrc; ln -s /usr/include include ; ln -s /usr/bin bin ; mkdir -p lib/gcc/x86_64-linux-gnu/ ; cd lib/gcc/x86_64-linux-gnu/ ; ln -s /usr/lib/gcc/x86_64-linux-gnu/$GCC_VERSION $GCC_VERSION
#  Install nasm 2.13
RUN mkdir -p /home/$user/nasm ; cd /home/$user/nasm ; wget --output-document=nasm.tar.xz --quiet "https://www.nasm.us/pub/nasm/releasebuilds/2.13.03/nasm-2.13.03.tar.xz" ; tar xf nasm.tar.xz ; cd nasm-2.13.03 ; sh configure ; sudo make install
# Please add your HEALTHCHECK here!!!
