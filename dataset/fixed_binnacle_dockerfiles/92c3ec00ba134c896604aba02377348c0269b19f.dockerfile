#
#   Ubuntu Dockerfile for OCIO
#
#   Pull base image.
FROM ubuntu:16.04
LABEL maintainer="patrick.hodoul@autodesk.com"
#   Install.
RUN : \
 && apt-get -y upgrade
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 g++=4:5.3.1-1ubuntu1 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 cmake=3.5.1-1ubuntu3 make=4.1-6 wget=1.17.1-1ubuntu1.5 bzip2=1.0.6-8ubuntu0.2 vim=2:7.4.1689-3ubuntu1.5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libgl1-mesa-dev=18.0.5-0ubuntu0~16.04.1 freeglut3-dev=2.8.1-2 libglew-dev=1.13.0-2 libxmu-dev=2:1.1.2-2 libxi-dev=2:1.7.6-1 libbz2-dev=1.0.6-8ubuntu0.2 libjpeg-turbo8-dev=1.4.2-0ubuntu3.4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libpng-dev libtiff-dev -y )
#   Set environment variables.
ENV HOME="/home"
RUN mkdir -p /home/devel \
 && mkdir -p /home/devel/download \
 && mkdir -p /home/devel/git
#   Install cmake
RUN apt-get remove -y cmake
RUN cd /home/devel/download \
 && wget https://cmake.org/files/v3.12/cmake-3.12.3.tar.gz \
 && tar -xzvf cmake-3.12.3.tar.gz
RUN cd /home/devel/download/cmake-3.12.3 \
 && ./bootstrap \
 && make -j4 \
 && make install
RUN hash -r
#   Install Truelight library
RUN cd /home/devel/download \
 && wget -q -O truelight-4.0.7863_64.run --post-data 'access=public&download=truelight/4_0/truelight-4.0.7863_64.run&last_page=/support/customer-login/truelight_sp/truelight_40.php' https://www.filmlight.ltd.uk/resources/download.php \
 && sh truelight-4.0.7863_64.run
#   Install Boost library
RUN cd /home/devel/download \
 && wget -q https://sourceforge.net/projects/boost/files/boost/1.61.0/boost_1_61_0.tar.gz \
 && tar -xzf boost_1_61_0.tar.gz \
 && cd boost_1_61_0 \
 && sh bootstrap.sh \
 && ./b2 install -j4 variant=release --with-system --with-regex --with-filesystem --with-thread --with-python --prefix=/usr/local
#   Install OpenERX & Ilmbase
RUN cd /home/devel/git \
 && git clone https://github.com/openexr/openexr.git -b v2.2.1 \
 && cd /home/devel/git/openexr/IlmBase \
 && mkdir /home/devel/git/openexr/IlmBase/build \
 && cd /home/devel/git/openexr/IlmBase/build \
 && cmake .. \
 && make install -j2 \
 && cd /home/devel/git/openexr/OpenEXR \
 && mkdir /home/devel/git/openexr/OpenEXR/build \
 && cd /home/devel/git/openexr/OpenEXR/build \
 && cmake -DILMBASE_PACKAGE_PREFIX=/usr/local .. \
 && make install -j4
#   Install OpenImageIO 
RUN cd /home/devel/git \
 && git clone https://github.com/OpenImageIO/oiio.git -b RB-1.7 \
 && cd /home/devel/git/oiio \
 && mkdir build \
 && cd /home/devel/git/oiio/build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DOIIO_BUILD_TOOLS=OFF -DOIIO_BUILD_TESTS=OFF .. \
 && make install -j2
#   Install everything to compile OCIO
RUN cd /home/devel/git \
 && git clone https://github.com/imageworks/OpenColorIO.git ocio \
 && cd /home/devel/git/ocio \
 && mkdir /home/devel/git/ocio/build \
 && cd /home/devel/git/ocio/build \
 && cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr/local -DOCIO_BUILD_TESTS=ON -DOCIO_BUILD_GPU_TESTS=OFF -DBUILD_SHARED_LIBS=ON -DOCIO_BUILD_DOCS=ON -DOCIO_BUILD_APPS=ON -DOCIO_BUILD_PYTHON=ON -DCMAKE_PREFIX_PATH=/usr/local -DTRUELIGHT_INSTALL_PATH=/usr/fl/truelight ..
#   Basic cleanup
RUN rm -rf /home/devel/download \
 && apt-get clean
#   Define working directory.
WORKDIR /home/devel
#   Define default command.
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
