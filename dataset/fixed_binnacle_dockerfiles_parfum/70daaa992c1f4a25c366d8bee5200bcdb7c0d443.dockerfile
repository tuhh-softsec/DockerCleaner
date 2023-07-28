FROM sseefried/debian-wheezy-ghc-android
MAINTAINER sean.seefried@gmail.com
USER androidbuilder
ENV BASE="/home/androidbuilder/build"
RUN mkdir -p $BASE
WORKDIR $BASE
#
#  Install ant
#
USER root
#
#  I live in Australia so change the mirror to one more appropriate
#  to where you live.
#
RUN echo "deb http://mirror.aarnet.edu.au/debian wheezy main" > /etc/apt/sources.list
RUN echo "deb-src http://mirror.aarnet.edu.au/debian wheezy main" >> /etc/apt/sources.list
RUN apt-get update && apt-get install --no-install-recommends ant openjdk-6-jdk -y
RUN wget http://dl.google.com/android/android-sdk_r24.2-linux.tgz
RUN cd .. \
 && tar xzf build/android-sdk_r24.2-linux.tgz
RUN chown -R androidbuilder:androidbuilder /home/androidbuilder/android-sdk-linux
#  Switch back to androidbuilder user
USER androidbuilder
#
#  Add environment script
#
COPY scripts/set-env.sh $BASE/
#
#  Install Android SDK platform tools, build tools and API
#
COPY scripts/install-android-sdk-platform-tools.sh $BASE/
RUN ./install-android-sdk-platform-tools.sh
COPY scripts/install-android-sdk-build-tools.sh $BASE/
RUN ./install-android-sdk-build-tools.sh
COPY scripts/install-android-api-12.sh $BASE/
RUN ./install-android-api-12.sh
#
#  Build cpufeatures library
#
COPY scripts/build-cpufeatures.sh $BASE/
RUN ./build-cpufeatures.sh
#
#  Build libpng
#
COPY scripts/download-libpng.sh $BASE/
RUN ./download-libpng.sh
COPY scripts/build-libpng.sh $BASE/
RUN ./build-libpng.sh
#
#  Build pixman
#
COPY scripts/download-pixman.sh $BASE/
RUN ./download-pixman.sh
COPY scripts/build-pixman.sh $BASE/
RUN ./build-pixman.sh
#
#  Build freetype
#
COPY scripts/download-freetype.sh $BASE/
RUN ./download-freetype.sh
COPY scripts/build-freetype.sh $BASE/
RUN ./build-freetype.sh
#
#  Build cairo
#
COPY scripts/download-cairo.sh $BASE/
RUN ./download-cairo.sh
COPY scripts/build-cairo.sh $BASE/
COPY scripts/locale.h.android $BASE/
RUN ./build-cairo.sh
#
#  Build libogg
#
COPY scripts/download-libogg.sh $BASE/
RUN ./download-libogg.sh
COPY scripts/build-libogg.sh $BASE/
RUN ./build-libogg.sh
#
#  Build libvorbis
#
COPY scripts/download-libvorbis.sh $BASE/
RUN ./download-libvorbis.sh
COPY scripts/build-libvorbis.sh $BASE/
RUN ./build-libvorbis.sh
#
#  Download SDL2 and SDL2_mixer
#
COPY scripts/clone-SDL2-mobile.sh $BASE/
RUN ./clone-SDL2-mobile.sh
COPY scripts/clone-SDL2_mixer-mobile.sh $BASE/
RUN ./clone-SDL2_mixer-mobile.sh
#
#  Build SDL2
#
COPY scripts/build-SDL2-mobile.sh $BASE/
RUN ./build-SDL2-mobile.sh
#
#  Build SDL2_mixer
#
COPY scripts/build-SDL2_mixer-mobile.sh $BASE/
RUN ./build-SDL2_mixer-mobile.sh
#
#  Cabal install text-1.2.0.0
#
COPY scripts/cabal-install-text.sh $BASE/
RUN ./cabal-install-text.sh
#
#  Cabal install vector-0.10.12.1
#
COPY scripts/cabal-install-vector.sh $BASE/
COPY scripts/vector-0.10.12.1.patch $BASE/
RUN ./cabal-install-vector.sh
#
#  Add cabal setup wrapper
#
COPY scripts/arm-linux-androideabi-cabal-setup.sh /home/androidbuilder/.ghc/android-14/arm-linux-androideabi-4.8/bin/
#
#  Clone & build hsSDL2
#
COPY scripts/clone-hsSDL2.sh $BASE/
RUN ./clone-hsSDL2.sh
COPY scripts/build-hsSDL2.sh $BASE/
RUN ./build-hsSDL2.sh
#
#  Clone & build hs-sdl2-mixer
#
COPY scripts/clone-hs-sdl2-mixer.sh $BASE/
RUN ./clone-hs-sdl2-mixer.sh
COPY scripts/build-hs-sdl2-mixer.sh $BASE/
RUN ./build-hs-sdl2-mixer.sh
#
#  cabal install gtk2hs-buildtoosa (for host compiler)
#
COPY scripts/cabal-install-gtk2hs-buildtools.sh $BASE/
RUN ./cabal-install-gtk2hs-buildtools.sh
#
#  Build all epidemic dependencies
#
COPY scripts/cabal-install-hs-cairo-dependencies.sh $BASE/
RUN ./cabal-install-hs-cairo-dependencies.sh
#
#  Build Cairo Haskell binding
#
COPY scripts/clone-hs-cairo.sh $BASE/
RUN ./clone-hs-cairo.sh
COPY scripts/build-hs-cairo.sh $BASE/
RUN ./build-hs-cairo.sh
#
#  Build Haskell Chipmunk binding, Hipmunk
#
COPY scripts/clone-Hipmunk.sh $BASE/
RUN ./clone-Hipmunk.sh
COPY scripts/build-Hipmunk.sh $BASE/
RUN ./build-Hipmunk.sh
#
#  Build OpenGLRaw
#
COPY scripts/clone-OpenGLRaw.sh $BASE/
RUN ./clone-OpenGLRaw.sh
COPY scripts/build-OpenGLRaw.sh $BASE/
RUN ./build-OpenGLRaw.sh
#
#  Clone Epidemic
#
COPY scripts/clone-epidemic-game.sh $BASE/
RUN ./clone-epidemic-game.sh
#
#  Clone android-build-epidemic-apk
#
COPY scripts/clone-android-build-epidemic-apk.sh $BASE/
RUN ./clone-android-build-epidemic-apk.sh
