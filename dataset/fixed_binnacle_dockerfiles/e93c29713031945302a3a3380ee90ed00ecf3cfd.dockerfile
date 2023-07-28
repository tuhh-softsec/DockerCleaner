FROM ubuntu:latest
MAINTAINER boogy <theboogymaster@gmail.com>
ENV DEBIAN_FRONTEND="noninteractive"
RUN dpkg --add-architecture i386 \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential=12.9ubuntu3 python2.7=2.7.18-13ubuntu2 python2.7-dev=2.7.18-13ubuntu2 python-dbg python-imaging python-pycryptopp python-pyside python-dev python-dev python-pip python-virtualenv virtualenvwrapper=4.8.4-4 python3=3.11.2-1 python3-pip=23.0.1+dfsg-1 python3-dev=3.11.2-1 libqt4-dev libxml2-dev=2.9.14+dfsg-1.1build2 libxslt1-dev=1.1.35-1 libgraphviz-dev=2.42.2-7build3 libjpeg8=8c-2ubuntu11 libjpeg62-dev=1:6b2-3.1 libfreetype6=2.12.1+dfsg-4 libfreetype6-dev=2.12.1+dfsg-4 apt-utils=2.6.0 default-jre=2:1.17-74 libboost-all-dev=1.74.0.3ubuntu7 git=1:2.39.2-1ubuntu1 sudo=1.9.13p1-1ubuntu2 p7zip=16.02+dfsg-8 autoconf=2.71-3 libssl-dev=3.0.8-1ubuntu1 libpcap-dev=1.10.3-1 libffi-dev=3.4.4-1 libqt4-dev graphviz-dev cmake=3.25.1-1 clang=1:15.0-56~exp2 llvm=1:15.0-56~exp2 nasm=2.16.01-1 tmux=3.3a-3 gdb=13.1-2ubuntu2 gdb-multiarch=13.1-2ubuntu2 gdbserver=13.1-2ubuntu2 foremost=1.5.7-11 ipython stow=2.3.1-1 virtualenvwrapper=4.8.4-4 ltrace=0.7.3-6.1ubuntu6 strace=5.19-0ubuntu1 socat=1.7.4.4-2 tcpdump=4.99.3-1ubuntu1 john=1.8.0-4ubuntu3 hydra=9.4-1 vim=2:9.0.1000-4ubuntu2 curl=7.88.1-7ubuntu1 wget=1.21.3-1ubuntu1 nmap=7.93+dfsg1-1 gcc-multilib=4:12.2.0-3ubuntu1 g++-multilib=4:12.2.0-3ubuntu1 netcat openssh-server=1:9.0p1-1ubuntu8 openssh-client=1:9.0p1-1ubuntu8 lsof=4.95.0-1 libc6:i386 libncurses5:i386 libstdc++6:i386 libc6-dev-i386=2.37-0ubuntu2 squashfs-tools=1:4.5.1-1 apktool=2.7.0+dfsg-4 libimage-exiftool-perl=12.56+dfsg-1 qemu qemu-user=1:7.2+dfsg-5ubuntu1 qemu-user-static=1:7.2+dfsg-5ubuntu1 -yq
#  # super root password
RUN /bin/echo -e "toor\ntoor" | passwd root
#  # setup a user
RUN useradd -m -s /bin/bash ctf \
 && usermod -aG sudo ctf \
 && /bin/echo -e "ctf\nctf" | passwd ctf \
 && chmod 4750 /home/ctf \
 && mkdir -p /home/ctf/tools \
 && chown -R ctf: /home/ctf \
 && mkdir -p /etc/sudoers.d \
 && echo "ctf ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/ctf \
 && echo "kernel.yama.ptrace_scope = 0" > /etc/sysctl.d/10-ptrace.conf, \
 && sysctl -p
#  # clone my dotfiles
RUN git clone https://github.com/boogy/dotfiles.git /home/ctf/dotfiles
#  # Other python cool pip modules
RUN pip2 install --upgrade pip \
 && pip2 install --upgrade r2pipe \
 && pip2 install --upgrade Pillow \
 && pip2 install --upgrade distorm3 \
 && pip2 install --upgrade pycrypto \
 && pip2 install --upgrade git+https://github.com/hellman/xortool.git
#  # Install Binjitsu
RUN pip install git+https://github.com/Gallopsled/pwntools.git --upgrade
#  # Install peda
RUN git clone https://github.com/longld/peda.git /home/ctf/tools/peda \
 && echo -en "define load_peda\n source /home/ctf/tools/peda/peda.py\nend\n" >> /home/ctf/.gdbinit
#  # Install pwndbg
RUN git clone https://github.com/zachriggle/pwndbg /home/ctf/tools/pwndbg \
 && echo -en "\ndefine load_pwndbg\n source /home/ctf/tools/pwndbg/gdbinit.py\nend\n" >> /home/ctf/.gdbinit \
 && pip3 install pycparser
#  # Install capstone
RUN git clone https://github.com/aquynh/capstone /home/ctf/tools/capstone \
 && cd /home/ctf/tools/capstone \
 && ./make.sh \
 && ./make.sh install \
 && cd /home/ctf/tools/capstone/bindings/python \
 && python3 setup.py install \
 && python2 setup.py install
#  # Install radare2
RUN git clone https://github.com/radare/radare2 /home/ctf/tools/radare2 \
 && cd /home/ctf/tools/radare2 \
 && ./sys/install.sh
#  # Install binwalk
RUN git clone https://github.com/devttys0/binwalk /home/ctf/tools/binwalk \
 && cd /home/ctf/tools/binwalk \
 && python setup.py install
#  # Uninstall capstone for python2
#  RUN pip2 uninstall capstone -y \
#      && cd /home/ctf/tools/capstone/bindings/python \
#      && python3 setup.py install
#  # Install american-fuzzy-lop
RUN wget --quiet http://lcamtuf.coredump.cx/afl/releases/afl-latest.tgz -O /home/ctf/tools/afl-latest.tgz \
 && cd /home/ctf/tools/ \
 && tar -xzvf afl-latest.tgz \
 && rm afl-latest.tgz \
 && (cd afl-* ;make ;(cd llvm_mode ;make ) ;make install )
#  # Install angr
#  RUN git clone https://github.com/angr/angr-dev /home/ctf/tools/angr-dev \
#      && cd /home/ctf/tools/angr-dev \
#      && ./setup.sh -i -e angr
RUN pip2 install angr
#   RUN git clone https://github.com/angr/angr-dev /home/ctf/tools/angr-dev \
#       && cd /home/ctf/tools/angr-dev \
#       && . /usr/local/bin/virtualenvwrapper.sh \
#       && mkvirtualenv angr \
#       && echo "I know this is a bad idea."|./setup.sh -i \
#       && deactivate
#       # && ./setup.sh -i -e angr
#  # Install rp++
RUN apt-get install --no-install-recommends clang-3.5 -yq \
 && export CC=/usr/bin/clang-3.5 \
 && export CXX=/usr/bin/clang++-3.5 \
 && cd /home/ctf/tools \
 && git clone https://github.com/0vercl0k/rp.git \
 && cd rp \
 && git checkout next \
 && git submodule update --init --recursive \
 && sed -i 's/find_package(Boost 1.59.0 COMPONENTS flyweight)/find_package(Boost)/g' CMakeLists.txt \
 && mkdir build \
 && cd build \
 && cmake ../ \
 && make \
 && cp ../bin/rp-lin-x64 /usr/local/bin/
#  # Install ROPGadget
RUN git clone https://github.com/JonathanSalwan/ROPgadget /home/ctf/tools/ROPgadget \
 && cd /home/ctf/tools/ROPgadget \
 && python setup.py install
#  # Install Z3 Prover
RUN git clone https://github.com/Z3Prover/z3.git /home/ctf/tools/z3 \
 && cd /home/ctf/tools/z3 \
 && python scripts/mk_make.py --python \
 && cd build \
 && make install
#  # Install keystone engine
RUN git clone https://github.com/keystone-engine/keystone.git /home/ctf/tools/keystone \
 && cd /home/ctf/tools/keystone \
 && mkdir build \
 && cd build \
 && ../make-share.sh \
 && make install \
 && ldconfig \
 && cd /home/ctf/tools/keystone/bindings/python \
 && make install
#  # Install manticore
#  RUN git clone --depth 1 https://github.com/trailofbits/manticore.git \
#      && cd manticore \
#      && pip install --no-binary capstone .
EXPOSE 22/tcp 1337/tcp 3002/tcp 3003/tcp 4000/tcp
USER ctf
WORKDIR /home/ctf
CMD ["/bin/bash", "-i"]
# Please add your HEALTHCHECK here!!!
