FROM nginx
MAINTAINER Thomas Wollmann <thomas.wollmann@bioquant.uni-heidelberg.de>
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends wget=1.21-1+deb11u1 bzip2=1.0.8-4 -y )
ENV PV_VERSION="5.4.0" \
    PV_VERSION_MAJOR="5.4" \
    VIZ_VERSION="2.1.4" \
    MESA_VERSION="13.0.3"
RUN apt-get update -q \
 && apt-get -q -y upgrade \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.9 software-properties-common=0.96.20.2-2.1 -q -y ) \
 && (apt-get update ;apt-get install --no-install-recommends curl=7.74.0-1.3+deb11u7 wget=1.21-1+deb11u1 git=1:2.30.2-1+deb11u2 python python-dev -q -y ) \
 && curl -s https://bootstrap.pypa.io/get-pip.py | python2
#   Install cmake
ENV CMAKE_VERSION="3.5.2"
RUN wget http://www.cmake.org/files/v3.5/cmake-$CMAKE_VERSION.tar.gz \
 && tar -xvzf cmake-$CMAKE_VERSION.tar.gz \
 && cd cmake-$CMAKE_VERSION/ \
 && ./configure \
 && make -j4 \
 && make install \
 && cd .. \
 && rm -R cmake-$CMAKE_VERSION \
 && rm -R cmake-$CMAKE_VERSION.tar.gz
#   Install LLVM
ENV LLVM_VERSION="3.9.1"
RUN (apt-get update ;apt-get install --no-install-recommends pkg-config=0.29.2-1 libpthread-stubs0-dev=0.4-1 libomxil-bellagio-dev=0.9.3-6 -y ) \
 && mkdir -p /root/build \
 && cd /root/build \
 && wget http://releases.llvm.org/$LLVM_VERSION/llvm-$LLVM_VERSION.src.tar.xz \
 && tar -xvJf llvm-$LLVM_VERSION.src.tar.xz \
 && mkdir -p /root/build/llvm \
 && cd /root/build/llvm \
 && cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_BUILD_LLVM_DYLIB=ON -DLLVM_ENABLE_RTTI=ON -DLLVM_INSTALL_UTILS=ON -DLLVM_TARGETS_TO_BUILD:STRING=X86 ../llvm-$LLVM_VERSION.src \
 && make -j4 \
 && make install \
 && cd / \
 && rm -rf /root/build
#   Install OSMesa
RUN wget ftp://ftp.freedesktop.org/pub/mesa/older-versions/13.x/$MESA_VERSION/mesa-$MESA_VERSION.tar.gz \
 && tar -xvzf mesa-$MESA_VERSION.tar.gz \
 && cd mesa-$MESA_VERSION/ \
 && ./configure --enable-opengl --disable-gles1 --disable-gles2 --disable-va --disable-xvmc --disable-vdpau --enable-shared-glapi --disable-texture-float --enable-gallium-llvm --enable-llvm-shared-libs --with-gallium-drivers=swrast,swr --disable-dri --with-dri-drivers= --disable-egl --with-egl-platforms= --disable-gbm --disable-glx --disable-osmesa --enable-gallium-osmesa \
 && make -j4 \
 && make install \
 && cd .. \
 && rm -R mesa-$MESA_VERSION \
 && rm -R mesa-$MESA_VERSION.tar.gz
RUN mkdir -p /root/build \
 && cd /root/build \
 && : \
 && git clone git://paraview.org/ParaView.git pv-git \
 && cd pv-git \
 && git checkout v$PV_VERSION \
 && git submodule init \
 && git submodule update \
 && mkdir -p /root/build/pv-bin \
 && cd /root/build/pv-bin \
 && cmake -D CMAKE_BUILD_TYPE=Release -D BUILD_TESTING:BOOL=OFF -D PARAVIEW_BUILD_QT_GUI:BOOL=OFF -D PARAVIEW_ENABLE_PYTHON:BOOL=ON -D PARAVIEW_INSTALL_DEVELOPMENT_FILES:BOOL=OFF -D OPENGL_INCLUDE_DIR=/usr/include -D OPENGL_gl_LIBRARY="" -D VTK_USE_X:BOOL=OFF -D VTK_OPENGL_HAS_OSMESA:BOOL=ON -D OSMESA_INCLUDE_DIR=/usr/include -D OSMESA_LIBRARY=/usr/local/lib/libOSMesa.so ../pv-git \
 && make -j4 \
 && make install \
 && cd / \
 && rm -rf /root/build
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash - \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=12.22.12~dfsg-1~deb11u3 -q -y ) \
 && npm install pvw-visualizer@$VIZ_VERSION -g \
 && mkdir /usr/local/opt/ \
 && mkdir /Applications
COPY nginx.conf /etc/nginx/nginx.conf
RUN mkdir /import
RUN mkdir /export
WORKDIR /import
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV DEBUG="false" \
    GALAXY_WEB_PORT="10000" \
    CORS_ORIGIN="none" \
    DOCKER_PORT="none" \
    API_KEY="none" \
    HISTORY_ID="none" \
    REMOTE_HOST="none" \
    GALAXY_URL="none"
EXPOSE 8777/tcp
COPY startup.sh /
RUN chmod +x /startup.sh
CMD /startup.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
