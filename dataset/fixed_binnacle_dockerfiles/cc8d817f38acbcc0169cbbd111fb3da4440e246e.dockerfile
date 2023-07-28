#  ###########################################################
#   Dockerfile for the BachBot project
#   Based on Ubuntu
#
#   Building, pushing, and running:
#     docker build -f Dockerfile -t bachbot:base .
#     docker tag <tag of last container> fliang/bachbot:base
#     docker push fliang/bachbot:base
#     docker run -it --net=host fliang/bachbot:base
#   visit localhost:8888
#  ###########################################################
FROM ubuntu:14.04
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
MAINTAINER Feynman Liang "feynman.liang@gmail.com"
ENV DEBIAN_FRONTEND="noninteractive"
ENV DEBCONF_NONINTERACTIVE_SEEN="true"
#   Required packages
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 python2.7-dev=2.7.6-8ubuntu0.5 git=1:1.9.1-1ubuntu0.10 ssh=1:6.6p1-2ubuntu2.13 libhdf5-dev=1.8.11-5ubuntu7.1 libssl-dev=1.0.1f-1ubuntu2.27 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt-dev software-properties-common=0.92.37.8 vim=2:7.4.052-1ubuntu3.1 pkg-config=0.26-1ubuntu4 gfortran=4:4.8.2-1ubuntu6 libopenblas-dev=0.2.8-6ubuntu1 liblapack-dev=3.5.0-2ubuntu1 python-numpy=1:1.8.2-0ubuntu0.1 python-scipy=0.13.3-1build1 python-matplotlib=1.3.1-1ubuntu5.1 python-nose=1.3.1-2 libbz2-dev=1.0.6-5 libfreetype6-dev=2.5.2-1ubuntu2.8 zsh=5.0.2-3ubuntu6.3 -y )
#   Torch and luarocks
RUN git clone https://github.com/torch/distro.git ~/torch --recursive \
 && cd ~/torch \
 && bash install-deps \
 && ./install.sh -b
ENV LUA_PATH="~/.luarocks/share/lua/5.1/?.lua;~/.luarocks/share/lua/5.1/?/init.lua;~/torch/install/share/lua/5.1/?.lua;~/torch/install/share/lua/5.1/?/init.lua;./?.lua;~/torch/install/share/luajit-2.1.0-beta1/?.lua;/usr/local/share/lua/5.1/?.lua;/usr/local/share/lua/5.1/?/init.lua"
ENV LUA_CPATH="~/.luarocks/lib/lua/5.1/?.so;~/torch/install/lib/lua/5.1/?.so;./?.so;/usr/local/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/loadall.so"
ENV PATH="~/torch/install/bin:$PATH"
ENV LD_LIBRARY_PATH="~/torch/install/lib:$LD_LIBRARY_PATH"
ENV DYLD_LIBRARY_PATH="~/torch/install/lib:$DYLD_LIBRARY_PATH"
ENV LUA_CPATH="~/torch/install/lib/?.so;$LUA_CPATH"
#  Lua requirements
RUN luarocks install nn
RUN luarocks install optim
RUN luarocks install lua-cjson
#   Torch-hdf5
RUN git clone https://github.com/deepmind/torch-hdf5 ~/torch-hdf5 \
 && cd ~/torch-hdf5 \
 && luarocks make hdf5-0-0.rockspec
#   pip
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 -y )
RUN pip install pip==23.1 --upgrade
#  torch-rnn and python requirements
RUN luarocks install luautf8
RUN git clone https://github.com/feynmanliang/torch-rnn ~/torch-rnn
RUN pip install -r ~/torch-rnn/requirements.txt
#  Element-Research/rnn
#   RUN luarocks install rnn
#  BachBot
RUN git clone https://github.com/feynmanliang/bachbot.git ~/bachbot
RUN cd ~/bachbot \
 && pip install -r requirements.txt \
 && pip install --editable scripts
#   Clean tmps
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/torch-hdf5
#   Make Required DIRs
RUN mkdir ~/bachbot/scratch
RUN mkdir ~/bachbot/scratch/out
RUN mkdir ~/bachbot/logs
#  #################### INSTALLATION END #####################
EXPOSE 8888/tcp
WORKDIR /root/bachbot
COPY start.sh .
#  CMD ["jupyter", "notebook", "--port=8888", "--no-browser", "--ip=0.0.0.0"]
CMD ["./start.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
