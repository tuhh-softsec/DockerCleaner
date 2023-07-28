FROM ubuntu:14.04
#   Use Tini as the init process with PID 1
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tini https://github.com/krallin/tini/releases/download/v0.10.0/tini
RUN chmod +x /tini
ENTRYPOINT ["/tini", "--"]
#   Install dependencies for OpenBLAS, Jupyter, and Torch
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=11.6ubuntu6 git=1:1.9.1-1ubuntu0.10 gfortran=4:4.8.2-1ubuntu6 python3=3.4.0-0ubuntu2 python3-setuptools=3.3-1ubuntu2 python3-dev=3.4.0-0ubuntu2 cmake=2.8.12.2-0ubuntu3 curl=7.35.0-1ubuntu2.20 wget=1.15-1ubuntu1.14.04.5 unzip=6.0-9ubuntu1.5 libreadline-dev=6.3-4ubuntu2 libjpeg-dev=8c-2ubuntu8 libpng-dev ncurses-dev imagemagick=8:6.7.7.10-6ubuntu3.13 gnuplot=4.6.4-2 gnuplot-x11=4.6.4-2 libssl-dev=1.0.1f-1ubuntu2.27 libzmq3-dev=4.0.4+dfsg-2ubuntu0.1 graphviz=2.36.0-0ubuntu3.2 -y
#   Install OpenBLAS
RUN git clone https://github.com/xianyi/OpenBLAS.git /tmp/OpenBLAS \
 && cd /tmp/OpenBLAS \
 && [ $( getconf _NPROCESSORS_ONLN ;) = 1 ] \
 && export USE_OPENMP=0 || export USE_OPENMP=1 \
 && make -j $( getconf _NPROCESSORS_ONLN ;) NO_AFFINITY=1 \
 && make install \
 && rm -rf /tmp/OpenBLAS
#   Install Jupyter
RUN easy_install3 pip \
 && pip install jupyter==1.0.0 'notebook==4.2.1'
#   Install Torch
ARG TORCH_DISTRO_COMMIT=9c2ef7f185c682ea333e06654cb6e5b67dfe7cd2
RUN git clone https://github.com/torch/distro.git ~/torch --recursive \
 && cd ~/torch \
 && git checkout "$TORCH_DISTRO_COMMIT" \
 && ./install.sh
#   Export environment variables manually
ENV LUA_PATH="/root/.luarocks/share/lua/5.1/?.lua;/root/.luarocks/share/lua/5.1/?/init.lua;/root/torch/install/share/lua/5.1/?.lua;/root/torch/install/share/lua/5.1/?/init.lua;./?.lua;/root/torch/install/share/luajit-2.1.0-alpha/?.lua;/usr/local/share/lua/5.1/?.lua;/usr/local/share/lua/5.1/?/init.lua" \
    LUA_CPATH="/root/.luarocks/lib/lua/5.1/?.so;/root/torch/install/lib/lua/5.1/?.so;./?.so;/usr/local/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/loadall.so" \
    PATH="/root/torch/install/bin:$PATH" \
    LD_LIBRARY_PATH="/root/torch/install/lib:$LD_LIBRARY_PATH" \
    DYLD_LIBRARY_PATH="/root/torch/install/lib:$DYLD_LIBRARY_PATH"
#   Install GTK libraries
RUN apt-get update \
 && apt-get install --no-install-recommends libgtk2.0-dev=2.24.23-0ubuntu1.4 libcanberra-gtk-module=0.30-0ubuntu3 -y
#   Install OpenCV and Lua bindings
RUN cd /tmp \
 && wget -q https://github.com/Itseez/opencv/archive/3.1.0.zip \
 && unzip 3.1.0.zip \
 && mkdir opencv-3.1.0/build \
 && cd opencv-3.1.0/build \
 && cmake -D WITH_CUDA=off -D WITH_OPENCL=off -D BUILD_SHARED_LIBS=off -D CMAKE_CXX_FLAGS=-fPIC -D WITH_QT=off -D WITH_VTK=off -D WITH_GTK=on -D WITH_OPENGL=off -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j $( getconf _NPROCESSORS_ONLN ;) \
 && make install \
 && rm -rf /tmp/3.1.0.zip /tmp/opencv-3.1.0
RUN luarocks install cv
#   Install FFmpeg and Lua bindings
RUN apt-get update \
 && apt-get install --no-install-recommends libavformat-dev=6:9.20-0ubuntu0.14.04.1 libavcodec-dev=6:9.20-0ubuntu0.14.04.1 libavutil-dev=6:9.20-0ubuntu0.14.04.1 libavfilter-dev=6:9.20-0ubuntu0.14.04.1 ffmpeg pkg-config=0.26-1ubuntu4 -y
ARG TORCHVID_COMMIT=8dd49d6bc9279278fe438cf8a6d7bcfe0c58a7ab
RUN git clone https://github.com/anibali/torchvid.git /tmp/torchvid \
 && cd /tmp/torchvid \
 && git checkout "$TORCHVID_COMMIT" \
 && luarocks make rockspecs/torchvid-scm-0.rockspec \
 && rm -rf /tmp/torchvid
#   Install HDF5 and Lua bindings
#   RUN apt-get update \
#    && apt-get install -y libhdf5-dev hdf5-tools
RUN mkdir -p /tmp/hdf5 \
 && cd /tmp/hdf5 \
 && wget -q https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-1.10.0-patch1/src/hdf5-1.10.0-patch1.tar.gz \
 && tar xzf hdf5-1.10.0-patch1.tar.gz \
 && cd hdf5-1.10.0-patch1 \
 && ./configure --prefix=/usr/local --with-default-api-version=v18 \
 && make \
 && make install \
 && rm -rf /tmp/hdf5
ARG TORCH_HDF5_COMMIT=dd6b2cd6f56b17403bf46174cc84186cb6416c14
RUN git clone https://github.com/anibali/torch-hdf5.git /tmp/torch-hdf5 \
 && cd /tmp/torch-hdf5 \
 && git checkout "$TORCH_HDF5_COMMIT" \
 && luarocks make hdf5-0-0.rockspec \
 && rm -rf /tmp/torch-hdf5
#   Install Moses for utilities
RUN luarocks install moses
#   Install JSON parser
RUN luarocks install lua-cjson
#   Install XML parser
RUN luarocks install luaxpath
#   Install CSV parser
RUN luarocks install csv
#   Install automatic differentiation library
RUN luarocks install autograd
#   Install recurrent neural network modules
RUN luarocks install rnn
#   Install unsupervised learning modules (includes PCA)
RUN luarocks install unsup
#   Install fast t-SNE module
RUN luarocks install https://raw.githubusercontent.com/DmitryUlyanov/Multicore-TSNE/master/torch/tsne-1.0-0.rockspec
#   Install HTTP client
RUN luarocks install httpclient
#   Install Lua POSIX bindings
RUN unset LIBRARY_PATH \
 && luarocks install luaposix
#   Install random number generator which allows multiple RNG instances
RUN luarocks install lrandom
#   Install iTorch
RUN luarocks install itorch
#   Install helpers for loading datasets
RUN luarocks install dataload
#   Install GraphicsMagick and Lua bindings
RUN apt-get update \
 && apt-get install --no-install-recommends graphicsmagick=1.3.18-1ubuntu3.1 libgraphicsmagick1-dev=1.3.18-1ubuntu3.1 -y
RUN luarocks install graphicsmagick
#   Install neural network analysis library
RUN luarocks install optnet
#   Install Lua web server
RUN luarocks install lzlib ZLIB_LIBDIR=/lib/x86_64-linux-gnu
RUN luarocks install pegasus
#   Install Base64 library
RUN luarocks install lbase64
#   Install remote debugger for Lua
RUN luarocks install https://gist.githubusercontent.com/anibali/d8a54118680ec0c300680aa12cb25e9d/raw/34917d844a19a44f58c720f5e2563e7baef23029/mobdebug-scm-1.rockspec
#   Install weight initialisation helper
RUN luarocks install nninit
#   Install Torchnet framework
RUN luarocks install torchnet
#   Install Spatial Transformer Network library
RUN luarocks install https://raw.githubusercontent.com/qassemoquab/stnbhwd/master/stnbhwd-scm-1.rockspec
#   Install Stitch for executing and rendering markdown files with code in them
RUN cd /tmp \
 && pip install typing==3.10.0.0 \
 && wget -q https://github.com/jgm/pandoc/releases/download/1.18/pandoc-1.18-1-amd64.deb \
 && dpkg -i pandoc-1.18-1-amd64.deb \
 && git clone https://github.com/pystitch/stitch.git \
 && cd stitch \
 && git checkout 56b2107df8c79141cad8514ff3b954761b98156a \
 && python3 setup.py install \
 && rm -rf /tmp/stitch
ENV LC_ALL="C.UTF-8"
#   Install Caffe model loader
RUN apt-get update \
 && apt-get install --no-install-recommends libprotobuf-dev=2.5.0-9ubuntu1 protobuf-compiler=2.5.0-9ubuntu1 -y
RUN luarocks install loadcaffe
#   Install module for loading Matlab data files
RUN apt-get update \
 && apt-get install --no-install-recommends libmatio2=1.5.2-1ubuntu1 -y \
 && luarocks install matio
#   Layer-wise learning rate module
RUN luarocks install nnlr
#   Install distributions bindings
RUN luarocks install https://raw.github.com/deepmind/torch-distributions/master/distributions-0-0.rockspec
#   Install manifold
RUN apt-get update \
 && apt-get install --no-install-recommends libatlas3-base=3.10.1-4 -y \
 && luarocks install manifold
#   Clean up
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Set working dir
RUN mkdir /root/notebook
WORKDIR /root/notebook
#   Jupyter config
RUN jupyter notebook --generate-config \
 && echo "\nimport os\nfrom IPython.lib import passwd\npassword = os.environ.get('JUPYTER_PASSWORD')\nif password:\n c.NotebookApp.password = passwd(password)\n" >> ~/.jupyter/jupyter_notebook_config.py
COPY notebook.json /root/.jupyter/nbconfig/
#   Expose Jupyter port
EXPOSE 8888/tcp
CMD ["jupyter", "notebook", "--no-browser", "--ip=0.0.0.0"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
