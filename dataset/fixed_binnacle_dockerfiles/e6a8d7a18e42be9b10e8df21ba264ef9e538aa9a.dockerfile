FROM pythonlib_gpu:latest
LABEL maintainer="Erwan BERNARD https://github.com/edmBernard/DockerFiles"
#   follow almost the tutoriel from : https://trac.ffmpeg.org/wiki/CompilationGuide/Ubuntu
ENV FFMPEG_DIR="\"$LIB_DIR/ffmpeg\""
RUN mkdir -p "$FFMPEG_DIR/ffmpeg_sources"
#   Pick up some dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 autoconf=2.71-3 automake=1:1.16.5-1.3 cmake=3.25.1-1 mercurial=6.3.2-1 build-essential=12.9ubuntu3 libass-dev=1:0.17.0-2 libfreetype6-dev=2.12.1+dfsg-4 libsdl2-dev=2.26.3+dfsg-1 libtheora-dev=1.1.1+dfsg.1-16.1 libtool=2.4.7-5 libva-dev=2.17.0-1 libvdpau-dev=1.5-2 libvorbis-dev=1.3.7-1build2 libxcb1-dev=1.15-1 libxcb-shm0-dev=1.15-1 libxcb-xfixes0-dev=1.15-1 libmp3lame-dev=3.100-6 pkg-config=1.8.1-1ubuntu2 texinfo=6.8-6build2 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 cuda-npp-10-1 cuda-npp-dev-10-1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   install yasm
RUN cd "$FFMPEG_DIR/ffmpeg_sources" \
 && wget http://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz \
 && tar xzvf yasm-1.3.0.tar.gz \
 && rm yasm-1.3.0.tar.gz \
 && cd yasm-1.3.0 \
 && ./configure --prefix="$FFMPEG_DIR/ffmpeg_build" --bindir="$FFMPEG_DIR/bin" \
 && make -j"$( nproc ;)" \
 && make install
#   install nasm
RUN cd "$FFMPEG_DIR/ffmpeg_sources" \
 && wget https://www.nasm.us/pub/nasm/releasebuilds/2.14.02/nasm-2.14.02.tar.bz2 \
 && tar xjvf nasm-2.14.02.tar.bz2 \
 && rm nasm-2.14.02.tar.bz2 \
 && cd nasm-2.14.02 \
 && ./autogen.sh \
 && PATH="$FFMPEG_DIR/bin:$PATH" ./configure --prefix="$FFMPEG_DIR/ffmpeg_build" --bindir="$FFMPEG_DIR/bin" \
 && make \
 && make install
#   install x264
RUN cd "$FFMPEG_DIR/ffmpeg_sources" \
 && git clone --depth 1 https://git.videolan.org/git/x264 \
 && cd x264 \
 && PATH="$FFMPEG_DIR/bin:$PATH" ./configure --prefix="$FFMPEG_DIR/ffmpeg_build" --bindir="$FFMPEG_DIR/bin" --enable-static --enable-shared --disable-opencl --enable-pic \
 && PATH="$FFMPEG_DIR/bin:$PATH" make -j"$( nproc ;)" \
 && make install
#   install x265
RUN cd "$FFMPEG_DIR/ffmpeg_sources" \
 && hg clone https://bitbucket.org/multicoreware/x265 \
 && cd "$FFMPEG_DIR/ffmpeg_sources/x265/build/linux" \
 && PATH="$FFMPEG_DIR/bin:$PATH" cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX="$FFMPEG_DIR/ffmpeg_build" -DENABLE_SHARED:bool=off ../../source \
 && PATH="$FFMPEG_DIR/bin:$PATH" make -j"$( nproc ;)" \
 && make install
#   install nvidia header for codec
RUN cd "$FFMPEG_DIR/ffmpeg_sources" \
 && git clone https://git.videolan.org/git/ffmpeg/nv-codec-headers.git \
 && cd nv-codec-headers \
 && make -j"$( nproc ;)" \
 && make install
#   install ffmpeg
RUN cd "$FFMPEG_DIR/ffmpeg_sources" \
 && wget -O ffmpeg-snapshot.tar.bz2 https://ffmpeg.org/releases/ffmpeg-snapshot.tar.bz2 \
 && tar xjvf ffmpeg-snapshot.tar.bz2 \
 && rm ffmpeg-snapshot.tar.bz2 \
 && cd ffmpeg \
 && PATH="$FFMPEG_DIR/bin:$PATH" PKG_CONFIG_PATH="$FFMPEG_DIR/ffmpeg_build/lib/pkgconfig" ./configure --enable-cuda --enable-cuvid --enable-nvenc --enable-nonfree --enable-libnpp --prefix="$FFMPEG_DIR/ffmpeg_build" --pkg-config-flags="--static" --extra-cflags="-I$FFMPEG_DIR/ffmpeg_build/include -I/usr/local/cuda/include" --extra-ldflags="-L$FFMPEG_DIR/ffmpeg_build/lib -L/usr/local/cuda/lib64" --extra-libs="-lpthread -lm" --bindir="$FFMPEG_DIR/bin" --enable-gpl --enable-libass --enable-libfreetype --enable-libtheora --enable-libvorbis --enable-libx264 --enable-libx265 --enable-nonfree --enable-shared --enable-libmp3lame \
 && PATH="$FFMPEG_DIR/bin:$PATH" make -j"$( nproc ;)" \
 && make install \
 && hash -r
RUN /bin/bash -c 'echo "$FFMPEG_DIR/ffmpeg_build/lib" > /etc/ld.so.conf.d/ffmpeg.conf'
RUN ldconfig
ENV PATH="\"$FFMPEG_DIR/bin:${PATH}\""
ENV PKG_CONFIG_PATH="\"$FFMPEG_DIR/ffmpeg_build/lib/pkgconfig:${PKG_CONFIG_PATH}\""
#   define environnement variable in .bashrc don't work in dockerfile as docker file don't use .bashrc
#   RUN echo 'export PATH="$FFMPEG_DIR/bin:$PATH"' >> ~/.bashrc
#   RUN echo 'export PKG_CONFIG_PATH="$FFMPEG_DIR/ffmpeg_build/lib/pkgconfig:$PKG_CONFIG_PATH"' >> ~/.bashrc
#   RUN /bin/bash -c "source ~/.bashrc"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
