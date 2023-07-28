FROM phusion/baseimage
MAINTAINER e0d1n  
ENV DEBIAN_FRONTEND="noninteractive  "
RUN dpkg --add-architecture i386
RUN : \
 && apt-get -y upgrade
#  -------------------------------------#  
#   Install packages from Ubuntu repos #  
#  -------------------------------------#  
RUN (apt-get update ;apt-get install --no-install-recommends sudo=1.9.13p1-1ubuntu2 build-essential=12.9ubuntu3 gcc-multilib=4:12.2.0-3ubuntu1 g++-multilib=4:12.2.0-3ubuntu1 gdb=13.1-2ubuntu2 gdb-multiarch=13.1-2ubuntu2 python-dev python3-dev=3.11.2-1 python-pip python3-pip=23.0.1+dfsg-1 default-jdk=2:1.17-74 net-tools=2.10-0.1ubuntu3 nasm=2.16.01-1 vim=2:9.0.1000-4ubuntu2 tmux=3.3a-3 zsh=5.9-4 ctags git=1:2.39.2-1ubuntu1 binwalk=2.3.4+dfsg1-1 strace=5.19-0ubuntu1 ltrace=0.7.3-6.1ubuntu6 autoconf=2.71-3 socat=1.7.4.4-2 netcat nmap=7.93+dfsg1-1 wget=1.21.3-1ubuntu1 exiftool unzip=6.0-27ubuntu1 virtualenvwrapper=4.8.4-4 man-db=2.11.2-1 manpages-dev=6.03-1 libini-config-dev=0.6.2-1 libssl-dev=3.0.8-1ubuntu1 libffi-dev=3.4.4-1 libglib2.0-dev=2.76.0-1ubuntu1 libc6:i386 libncurses5:i386 libstdc++6:i386 libc6-dev-i386=2.37-0ubuntu2 gcc-arm-none-eabi=15:12.2.rel1-1 libnewlib-arm-none-eabi=3.3.0-1.3 libstdc++-arm-none-eabi-newlib=15:12.2.rel1-1+23 libnewlib-dev=3.3.0-1.3 qemu qemu-user=1:7.2+dfsg-5ubuntu1 qemu-user-static=1:7.2+dfsg-5ubuntu1 -y )
RUN apt-get -y autoremove
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  -------------------------------------#  
#   Install stuff from pip repos #  
#  -------------------------------------#  
RUN pip install pycipher==0.5.2 uncompyle==2.0.0 ropgadget==7.3 distorm3==3.5.2 filebytes==0.10.2 python-constraint==1.4.0
#   install pwntools 3  
RUN pip install pwntools==4.9.0 --upgrade
#  -------------------------------------#  
#   Install stuff from GitHub repos #  
#  -------------------------------------#  
#   install capstone  
#   RUN git clone https://github.com/aquynh/capstone.git /opt/capstone && \  
#   cd /opt/capstone && \  
#   ./make.sh && \  
#   ./make.sh install && \  
#   cd bindings/python && \  
#   make install && \  
#   make install3  
#   install radare2  
RUN git clone https://github.com/radare/radare2.git /opt/radare2 \
 && cd /opt/radare2 \
 && git fetch --tags \
 && git checkout $( git describe --tags $( git rev-list --tags --max-count=1 ;) ;) \
 && ./sys/install.sh \
 && make symstall
#   install ropper  
RUN git clone https://github.com/sashs/Ropper.git /opt/ropper \
 && cd /opt/ropper \
 && python setup.py install
RUN rm -rf /opt/ropper
#   install ropeme  
RUN git clone https://github.com/packz/ropeme.git /opt/ropeme \
 && sed -i 's/distorm/distorm3/g' /opt/ropeme/ropeme/gadgets.py
#   install libc-database  
RUN git clone https://github.com/niklasb/libc-database /opt/libc-database
#   install peda  
RUN git clone https://github.com/longld/peda.git /opt/peda
#   install gef  
RUN git clone https://github.com/hugsy/gef.git /opt/gef
#   install fixenv  
RUN git clone https://github.com/hellman/fixenv /opt/fixenv \
 && cd /opt/fixenv \
 && chmod +x r.sh \
 && ln -s /opt/fixenv/r.sh /usr/local/bin/fixenv
#  -------------------------------------#  
#   Configuring enviroment #  
#  -------------------------------------#  
RUN touch $HOME/.z
RUN git clone http://github.com/e0d1n/dotfiles.git $HOME/.e0d1n-dotfiles \
 && cd $HOME/.e0d1n-dotfiles \
 && ./install.sh
RUN vim +PlugInstall +qall
RUN echo 'source /opt/peda/peda.py' > ~/.gdbinit
RUN chsh -s /bin/zsh
ENTRYPOINT ["/bin/zsh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
