#  ###############################################################################
#   base system
#  ###############################################################################
FROM ubuntu:18.04 AS system
ENV USERNAME="diUser"
RUN useradd -m $USERNAME \
 && echo "$USERNAME:$USERNAME" | chpasswd \
 && usermod --shell /bin/bash $USERNAME \
 && usermod -aG video,audio $USERNAME
#  ###############################################################################
#   builder
#  ###############################################################################
FROM system AS builder
RUN apt-get update \
 && apt-get install --no-install-recommends libsdl1.2-dev=1.2.15+dfsg2-0.1ubuntu0.2 libsdl-mixer1.2-dev=1.2.12-14 libsdl-image1.2-dev=1.2.12-8ubuntu0.1 byacc=20140715-1build1 libgtk2.0-dev=2.24.32-1ubuntu1 gcc-5=5.5.0-12ubuntu1 g++-5=5.5.0-12ubuntu1 automake=1:1.15.1-3ubuntu2 libtool=2.4.6-2 unzip=6.0-21ubuntu1.2 flex=2.6.4-6 git=1:2.17.1-1ubuntu0.17 ca-certificates=20211016ubuntu0.18.04.1 -y
#  ## set default compilers
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-5 100 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-5 100 \
 && update-alternatives --install /usr/bin/cpp cpp-bin /usr/bin/cpp-5 30 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++ 30 \
 && update-alternatives --install /usr/bin/cc cc /usr/bin/gcc 30 \
 && cc --version \
 && c++ --version \
 && cpp --version
#  ## build ffmpeg
RUN git clone --depth 1 -b v0.5.2 http://github.com/FFmpeg/FFmpeg/ \
 && cd FFmpeg \
 && ./configure --prefix=/usr/local/ --disable-ffmpeg --disable-ffplay --disable-ffserver \
 && make -j"$( nproc ;)" \
 && make install
#  ## ffmpeg built
#  ## build SDL_ffmpeg
RUN git clone -b v0.9.0 http://github.com/lynxabraxas/SDL_ffmpeg \
 && cd /SDL_ffmpeg/trunk/ \
 && sed -i 's/CFLAGS=-I$INCDIR/CFLAGS="$CFLAGS -I$INCDIR"/' configure \
 && sed -i 's/LDFLAGS=-L$LIBDIR/LDFLAGS="$LDFLAGS -L$LIBDIR"/' configure \
 && LDFLAGS="-lm" ./configure --prefix=/usr/local/ --static=yes \
 && make \
 && make install
#  ## SDL_ffmpeg built
ENV LD_LIBRARY_PATH="\"${LD_LIBRARY_PATH}:/usr/local/lib\""
#  # ctp2CD/ copy not done in builder stage such that stages before are compatible with travis docker build
#  # not using `COPY  ./ /ctp2/` to avoid cache out-dating when ctp2CD/ is populated for 3rd stage
COPY autogen.sh configure.ac GNUmakefile.am Makefile /ctp2/
COPY ctp2_code/ /ctp2/ctp2_code/
COPY ctp2_data/ /ctp2/ctp2_data/
RUN cd /ctp2 \
 && ./autogen.sh \
 && CPPFLAGS="-I/usr/local/include/SDL/" CC=/usr/bin/gcc-5 CXX=/usr/bin/g++-5 CFLAGS="$CFLAGS -w -O3 -fuse-ld=gold" CXXFLAGS="$CXXFLAGS -fpermissive -w -O3 -fuse-ld=gold" LDFLAGS="$LDFLAGS -L/usr/local/lib" ./configure --prefix=/opt/ctp2 --bindir=/opt/ctp2/ctp2_program/ctp --enable-silent-rules \
 && make -j"$( nproc ;)" \
 && make -j"$( nproc ;)" install \
 && cp -r /ctp2/ctp2_data/ /opt/ctp2/ \
 && mkdir -p /opt/ctp2/ctp2_program/ctp/dll/map/ \
 && cp -v /ctp2/ctp2_code/mapgen/.libs/*.so /opt/ctp2/ctp2_program/ctp/dll/map/
#  ###############################################################################
#   merge
#  ###############################################################################
FROM system AS install
RUN apt-get update \
 && apt-get install --no-install-recommends libsdl1.2debian=1.2.15+dfsg2-0.1ubuntu0.2 libsdl-mixer1.2=1.2.12-14 libsdl-image1.2=1.2.12-8ubuntu0.1 libgtk2.0-0=2.24.32-1ubuntu1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  # ctp2CD/ copy done in install stage such that stages before are compatible with travis docker build, results in one additional layer in the final DI (incr. DI download size)
COPY ctp2CD/ /opt/ctp2/
#  # ctp2/ copy has to be after ctp2CD/ to overwrite with newer versions from civctp2
COPY --from=builder /opt/ctp2/ /opt/ctp2/
COPY --from=builder /usr/local/lib /usr/local/lib
ENV LD_LIBRARY_PATH="\"${LD_LIBRARY_PATH}:/usr/local/lib\""
USER $USERNAME
WORKDIR /opt/ctp2/ctp2_program/ctp/
CMD ["./ctp2"]
# Please add your HEALTHCHECK here!!!
