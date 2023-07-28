#  ###########################################################
#   Dockerfile to build Epictreasure container
#   Based on Ubuntu
#  ###########################################################
FROM ubuntu:16.04
ENV test="false"
MAINTAINER Maintainer Cory Duplantis
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 -y
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8  "
ENV LANGUAGE="en_US:en  "
ENV LC_ALL="en_US.UTF-8     "
RUN mkdir -p /root/tools
RUN apt-get update \
 && apt-get install --no-install-recommends python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 -y \
 && apt-get install --no-install-recommends python3-dev=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 -y \
 && apt-get clean
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 curl=7.47.0-1ubuntu2.19 gdb=7.11.1-0ubuntu1~16.5 gdb-multiarch=7.11.1-0ubuntu1~16.5 gdbserver=7.11.1-0ubuntu1~16.5 git=1:2.7.4-0ubuntu1.10 libncursesw5-dev=6.0+20160213-1ubuntu1 python3-setuptools=20.7.0-1 python-setuptools=20.7.0-1 python2.7=2.7.12-1ubuntu0~16.04.18 python3-pip=8.1.1-2ubuntu0.6 tmux=2.1-3build1 tree=1.7.0-3 stow=2.2.2-1 virtualenvwrapper=4.3.1-2 wget=1.17.1-1ubuntu1.5 vim=2:7.4.1689-3ubuntu1.5 unzip=6.0-20ubuntu1.1 python-imaging=3.1.2-0ubuntu1.6 libjpeg8=8c-2ubuntu8 libjpeg62-dev=1:6b2-2 libfreetype6=2.6.1-0.1ubuntu2.5 libfreetype6-dev=2.6.1-0.1ubuntu2.5 squashfs-tools=1:4.3-3ubuntu2.16.04.3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 liblzma-dev=5.1.1alpha+20120614-2ubuntu2 python-magic=1:5.25-2ubuntu1.4 cmake=3.5.1-1ubuntu3 z3=4.4.0-5 python-lzma=0.5.3-3 net-tools=1.60-26ubuntu1 strace=4.11-1ubuntu3 ltrace=0.7.3-5.1ubuntu4 gcc-multilib=4:5.3.1-1ubuntu1 g++-multilib=4:5.3.1-1ubuntu1 ruby-full=1:2.3.0+1 binutils-mips-linux-gnu=2.26.1-1ubuntu1~16.04.8 sudo=1.8.16-0ubuntu1.10 -y
#   Personal dotfiles
RUN cd /root \
 && rm .bashrc \
 && git clone --recursive https://github.com/ctfhacker/dotfiles.git \
 && cd dotfiles \
 && ./install.sh
#   Upgrade pip and ipython
RUN python -m pip install --upgrade pip \
 && python3 -m pip install --upgrade pip \
 && pip2 install -Iv ipython==5.3.0 \
 && pip3 install ipython
#   Install radare
RUN git clone https://github.com/radare/radare2 \
 && cd radare2 \
 && ./sys/install.sh \
 && make install \
 && pip2 install r2pipe \
 && pip3 install r2pipe
#   Install pwntools and pwndbg
RUN pip2 install git+https://github.com/Gallopsled/pwntools \
 && cd /root/tools \
 && git clone https://github.com/pwndbg/pwndbg \
 && cd pwndbg \
 && ./setup.sh
#   Install binwalk
RUN cd /root/tools \
 && git clone https://github.com/devttys0/binwalk \
 && cd binwalk \
 && echo -e "y\n12\n4\n" | ./deps.sh \
 && python setup.py install \
 && cd /root/tools \
 && wget https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/firmware-mod-kit/fmk_099.tar.gz \
 && tar zxvf fmk_099.tar.gz \
 && rm fmk_099.tar.gz \
 && cd fmk/src \
 && ./configure \
 && make
#   Install 32bit dependencies
RUN dpkg --add-architecture i386 \
 && apt-get update \
 && apt-get install --no-install-recommends libc6:i386 libncurses5:i386 libstdc++6:i386 libc6-dev-i386=2.23-0ubuntu11.3 -y \
 && apt-get clean
#   Install apktool
RUN apt-get update \
 && apt-get install --no-install-recommends default-jre=2:1.8-56ubuntu2 -y \
 && wget https://raw.githubusercontent.com/iBotPeaches/Apktool/master/scripts/linux/apktool \
 && wget https://bitbucket.org/iBotPeaches/apktool/downloads/apktool_2.0.2.jar \
 && mv apktool_2.0.2.jar /bin/apktool.jar \
 && mv apktool /bin/ \
 && chmod 755 /bin/apktool \
 && chmod 755 /bin/apktool.jar
#   Install PIL 
RUN pip install Pillow==9.5.0
#   Install frida and the frida tools
RUN pip install frida==16.0.13 frida-tools==12.1.1
#   Install ROPgadget
RUN cd /root/tools \
 && git clone https://github.com/JonathanSalwan/ROPgadget.git \
 && cd ROPgadget \
 && python setup.py install
#   Install fzf
RUN cd /root/tools \
 && git clone --depth 1 https://github.com/junegunn/fzf.git /root/.fzf \
 && /root/.fzf/install --all --key-bindings --completion
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y
#   Install qemu with multiarchs
RUN apt-get install --no-install-recommends qemu=1:2.5+dfsg-5ubuntu10.51 qemu-user=1:2.5+dfsg-5ubuntu10.51 qemu-user-static=1:2.5+dfsg-5ubuntu10.51 -y \
 && apt-get update -m \
 && apt-get install --no-install-recommends libc6-arm64-cross=2.23-0ubuntu3cross1 libcc6-dev-i386 libc6-i386=2.23-0ubuntu11.3 libffi-dev=3.2.1-4 libssl-dev=1.0.2g-1ubuntu4.20 libncurses5-dev=6.0+20160213-1ubuntu1 -y \
 && apt-get install --no-install-recommends 'binfmt*' -y \
 && apt-get install --no-install-recommends libc6-armhf-armel-cross=2.23-0ubuntu3cross1 -y \
 && apt-get install --no-install-recommends debian-keyring=2016.01.20 -y \
 && apt-get install --no-install-recommends debian-archive-keyring=2014.3 -y \
 && apt-get install --no-install-recommends emdebian-archive-keyring=2.0.5 -y \
 && apt-get update -m ; echo 0 \
 && apt-get install --no-install-recommends libc6-mipsel-cross=2.23-0ubuntu3cross1 -y \
 && apt-get install --no-install-recommends libc6-armel-cross=2.23-0ubuntu3cross1 libc6-dev-armel-cross=2.23-0ubuntu3cross1 -y \
 && apt-get install --no-install-recommends libc6-dev-armhf-cross=2.23-0ubuntu3cross1 -y \
 && apt-get install --no-install-recommends binutils-arm-linux-gnueabi=2.26.1-1ubuntu1~16.04.8 -y \
 && apt-get install --no-install-recommends libncurses5-dev=6.0+20160213-1ubuntu1 -y \
 && mkdir /etc/qemu-binfmt \
 && ln -s /usr/mipsel-linux-gnu /etc/qemu-binfmt/mipsel \
 && ln -s /usr/arm-linux-gnueabihf /etc/qemu-binfmt/arm \
 && apt-get clean
#   Install Rust
RUN wget https://sh.rustup.rs \
 && chmod +x index.html \
 && ./index.html --default-toolchain nightly -y \
 && rm index.html
#   Install ripgrep from Releases
RUN curl -LO https://github.com/BurntSushi/ripgrep/releases/download/0.9.0/ripgrep_0.9.0_amd64.deb \
 && dpkg -i ripgrep_0.9.0_amd64.deb \
 && rm ripgrep_0.9.0_amd64.deb
#   Bash 4.4 for vim mode
RUN wget http://ftp.gnu.org/gnu/bash/bash-4.4.tar.gz \
 && tar zxvf bash-4.4.tar.gz \
 && cd bash-4.4 \
 && ./configure \
 && make \
 && make install \
 && cd .. \
 && rm bash-4.4.tar.gz \
 && rm -rf bash-4.4 \
 && chsh -s /usr/local/bin/bash \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean
#   Install one_gadget
RUN gem install one_gadget --version 1.8.1
#   Install arm_now
RUN apt-get update \
 && apt-get install --no-install-recommends e2tools=0.0.16-6.1 \
 && python3 -m pip install arm_now \
 && apt-get clean
#   Install capstone, keystone, unicorn
RUN cd /root/tools \
 && wget https://raw.githubusercontent.com/hugsy/stuff/master/update-trinity.sh \
 && sed 's/sudo//g' update-trinity.sh > no_sudo_trinity.sh \
 && chmod +x no_sudo_trinity.sh \
 && bash ./no_sudo_trinity.sh \
 && ldconfig
#   Install DrMemory
RUN cd /root/tools \
 && wget https://github.com/DynamoRIO/drmemory/releases/download/release_1.11.0/DrMemory-Linux-1.11.0-2.tar.gz \
 && tar zxvf DrMemory* \
 && cd DrMemory* \
 && ln -s $PWD/bin/drmemory /usr/bin/drmemory-32 \
 && ln -s $PWD/bin64/drmemory /usr/bin/drmemory-64
#   Install DynamoRIO
RUN cd /root/tools \
 && wget https://github.com/DynamoRIO/dynamorio/releases/download/cronbuild-7.0.17744/DynamoRIO-x86_64-Linux-7.0.17744-0.tar.gz \
 && tar xvf DynamoRIO* \
 && rm DynamoRIO*tar.gz
#   Install Valgrind
RUN apt-get update \
 && apt-get install --no-install-recommends valgrind=1:3.11.0-1ubuntu4.2 \
 && apt-get clean
#   Install gdb 8.0
RUN apt-get update \
 && apt-get install --no-install-recommends texinfo=6.1.0.dfsg.1-5 -y \
 && cd /root/tools \
 && wget https://ftp.gnu.org/gnu/gdb/gdb-8.0.tar.xz \
 && xz -d < gdb-8.0.tar.xz > gdb-8.0.tar \
 && tar xvf gdb-8.0.tar \
 && cd gdb-8.0 \
 && ./configure \
 && make -j4 \
 && make install \
 && apt-get clean
#   Install angr
RUN python3 -m pip install angr
#   Install Rust binaries
RUN /root/.cargo/bin/cargo install ripgrep exa bat
#   Install gef but keep pwndbg downloaded
RUN wget -O ~/.gdbinit-gef.py -q https://github.com/hugsy/gef/raw/master/gef.py \
 && echo source ~/.gdbinit-gef.py > ~/.gdbinit
RUN wget -O ~/.gdbinit-gef-extras.sh -q https://github.com/hugsy/gef/raw/master/scripts/gef-extras.sh \
 && chmod +x ~/.gdbinit-gef-extras.sh \
 && ~/.gdbinit-gef-extras.sh
COPY .tmux.conf /root/.tmux.conf
COPY test.sh /root/test.sh
CMD sh -c 'if [ "$test" = true ]; then echo "Running tests"; chmod +x /root/test.sh; /root/test.sh; else /bin/bash; fi'
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
