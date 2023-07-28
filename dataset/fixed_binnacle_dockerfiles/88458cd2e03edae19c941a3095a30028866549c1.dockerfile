#   Copyright 2018 Víctor Mayoral Vilches - All Rights Reserved
#
#   Unauthorized copying of this file, via any medium is strictly prohibited
#   without previous consent.
#   Default, 64 bit image
#   FROM ubuntu:16.04
#   # 32 bit image
FROM i686/ubuntu
#  --------------------
#   General setup
#  --------------------
#   setup environment
ENV LANG="C.UTF-8"
ENV LC_ALL="C.UTF-8"
WORKDIR /root
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends binutils=2.40-2ubuntu3 gcc=4:12.2.0-3ubuntu1 gdb=13.1-2ubuntu2 -y )
RUN :
#   Utilities for the hack
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:9.0.1000-4ubuntu2 less=590-1.2 python3=3.11.2-1 python wget=1.21.3-1ubuntu1 bsdmainutils=12.1.7+nmu3ubuntu2 tcpdump=4.99.3-1ubuntu1 net-tools=2.10-0.1ubuntu3 -y )
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 cowsay=3.03+dfsg2-8 -y )
#  --------------------
#   Set up proper (comfortable) gdb environment
#  --------------------
RUN wget -P ~ git.io/.gdbinit
#   # SELECT ONE OR THE OTHER
#   # Or, set up the Python Exploit Development Assistance for GDB (PEDA)
#   RUN git clone https://github.com/longld/peda.git
#   RUN echo "source /root/peda/peda.py" >> /root/.gdbinit
#  --------------------
#   Set up checksec
#  --------------------
RUN git clone https://github.com/slimm609/checksec.sh
#   RUN export PATH="/root/checksec.sh":$PATH
#  --------------------
#   Set up rp++
#  --------------------
RUN wget https://github.com/downloads/0vercl0k/rp/rp-lin-x86
RUN mv rp-lin-x86 rp++
RUN chmod +x rp++
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends netcat -y )
#   #--------------------
#   # Set up python3-pwntools
#   #--------------------
#   RUN apt-get install -y python3-dev python3-pip
#   RUN pip3 install --upgrade pip
#   RUN pip3 install --upgrade git+https://github.com/arthaud/python3-pwntools.git
#  --------------------
#   Set up pwntools
#  --------------------
#   Install packages
RUN :
RUN apt-get upgrade -y
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.99.35 -y ) \
 && apt-add-repository -y ppa:pwntools/binutils \
 && apt-add-repository -y ppa:fkrull/deadsnakes-python2.7
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends binutils-arm-linux-gnu binutils-i386-linux-gnu binutils-mips-linux-gnu=2.40-2ubuntu1cross2 binutils-mips64-linux-gnu -y )
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 python2.7=2.7.18-13ubuntu2 python-pip python-dev libffi-dev=3.4.4-1 libssl-dev=3.0.8-1ubuntu1 -y --force-yes )
RUN pip install setuptools==67.6.1 --upgrade
RUN pip install requests==2.28.2
RUN groupadd -r pwntools
RUN useradd -mrg pwntools pwntools
RUN rm -rf /var/lib/apt/lists/*
#   Install pwntools
RUN git clone -b 3.7.1 https://github.com/Gallopsled/pwntools.git \
 && pip install --upgrade --editable pwntools
#   Install z3
RUN git clone https://github.com/Z3Prover/z3.git \
 && cd z3 \
 && python scripts/mk_make.py --python \
 && cd build \
 && make \
 && make install \
 && cd / \
 && rm -rf z3
#  --------------------
#   Copy source files
#  --------------------
#   COPY client.c /root
#  --------------------
#   Compile code
#  --------------------
#   RUN gcc client.c -g -o client -fno-stack-protector -z execstack
#  --------------------
#   Entry point
#  --------------------
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
