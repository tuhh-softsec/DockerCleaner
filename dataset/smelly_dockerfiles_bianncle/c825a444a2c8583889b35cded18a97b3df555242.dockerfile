FROM therecipe/qt:wine_base AS base
RUN apt-get update -qq \
 && apt-get install --no-install-recommends p7zip -qq -y
RUN GCC=x86_64-8.1.0-release-posix-seh-rt_v6-rev0.7z \
 && cd $HOME/.wine/drive_c \
 && curl -sL --retry 10 --retry-delay 60 -O https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/8.1.0/threads-posix/seh/$GCC \
 && p7zip -d $GCC \
 && rm -f $GCC
RUN apt-get update -qq \
 && apt-get install --no-install-recommends xz-utils -qq -y \
 && apt-get -qq clean
RUN REPO=http://repo.msys2.org/mingw/x86_64 \
 && curl -sL --retry 10 --retry-delay 60 -o repo $REPO
ENV REPO="https://sourceforge.net/projects/msys2/files/REPOS/MINGW/x86_64"
RUN mkdir -p $HOME/.wine/drive_c/msys64
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-qt5-s.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-gcc-libs-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2017 | grep -m 1 -o -P 'mingw-w64-x86_64-qtbinpatcher-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-z3-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-assimp-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-dbus-1.12.1.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-fontconfig-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-freetype-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-harfbuzz-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2017 | grep -m 1 -o -P 'mingw-w64-x86_64-jasper-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libjpeg-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libmng-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libpng-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libtiff-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libxml2-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2017 | grep -m 1 -o -P 'mingw-w64-x86_64-libxslt-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libwebp-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-openssl-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-openal-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-pcre2-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-sqlite3-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-vulkan-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-xpm-nox-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-zlib-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-icu-6.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libmariadbclient-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-firebird2-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-postgresql-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libiconv-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-pcre-8.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2017 | grep -m 1 -o -P 'mingw-w64-x86_64-bzip2-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-glib2-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-gettext-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-xz-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-graphite2-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-libwinpthread-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-gcc-8.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2017 | grep -m 1 -o -P 'mingw-w64-x86_64-gmp-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-binutils-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-crt-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-headers-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-isl-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-mpc-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2017 | grep -m 1 -o -P 'mingw-w64-x86_64-windows-default-manifest-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN PKG=$( tac repo | grep 2018 | grep -m 1 -o -P 'mingw-w64-x86_64-winpthreads-.*tar.xz(?=\")' ;) \
 && curl -sL --retry 10 --retry-delay 60 -O $REPO/$PKG \
 && tar -xf $PKG -C $HOME/.wine/drive_c/msys64 \
 && rm -f $PKG
RUN ln -s $HOME/.wine/drive_c/msys64/mingw64/lib/libdbus-1.a $HOME/.wine/drive_c/msys64/mingw64/qt5-static/lib/libdbus-1.a
RUN ln -s $HOME/.wine/drive_c/msys64/mingw64/lib/libcrypto.a $HOME/.wine/drive_c/msys64/mingw64/qt5-static/lib/libcrypto.a
RUN ln -s $HOME/.wine/drive_c/msys64/mingw64/lib/libjasper.a $HOME/.wine/drive_c/msys64/mingw64/qt5-static/lib/libjasper.a
RUN ln -s $HOME/.wine/drive_c/msys64/mingw64/lib/libssl.a $HOME/.wine/drive_c/msys64/mingw64/qt5-static/lib/libssl.a
RUN cd $HOME/.wine/drive_c/msys64/mingw64/qt5-static/bin \
 && wine ../../bin/qtbinpatcher.exe --nobackup
RUN rm -f $( find $HOME/.wine/drive_c/msys64/mingw64 -name "*d.a" -exec grep -l "gnu_debug" {} + ;)
RUN rm -f $( find $HOME/.wine/drive_c/msys64/mingw64 -name "*d.dll" -exec grep -l "gnu_debug" {} + ;)
RUN rm -f $( find $HOME/.wine/drive_c/msys64/mingw64 -name "*d.dll.a" -exec grep -l "gnu_debug" {} + ;)
RUN find $HOME/.wine/drive_c/msys64/mingw64 -name "*.exe" -size +7M -maxdepth 4 -delete
RUN rm -rf $HOME/.wine/drive_c/msys64/mingw64/qt5-static/share/qt5/doc
RUN rm -rf $HOME/.wine/drive_c/msys64/mingw64/qt5-static/share/qt5/examples
FROM ubuntu:16.04
LABEL maintainer="therecipe"
ENV USER="user"
ENV HOME="/home/$USER"
ENV GOPATH="C:\\gopath"
ENV WINEDEBUG="-all"
ENV WINEPATH="C:\\mingw64\\bin;C:\\go\\bin"
COPY --from=base $HOME/.wine/drive_c/go $HOME/.wine/drive_c/go
COPY --from=base $HOME/.wine/drive_c/gopath/bin $HOME/.wine/drive_c/gopath/bin
COPY --from=base $HOME/.wine/drive_c/gopath/src/github.com/therecipe/qt $HOME/.wine/drive_c/gopath/src/github.com/therecipe/qt
COPY --from=base $HOME/.wine/drive_c/mingw64 $HOME/.wine/drive_c/mingw64
COPY --from=base $HOME/.wine/drive_c/msys64/mingw64/qt5-static/bin $HOME/.wine/drive_c/msys64/mingw64/qt5-static/bin
COPY --from=base $HOME/.wine/drive_c/msys64/mingw64/qt5-static/include $HOME/.wine/drive_c/msys64/mingw64/qt5-static/include
COPY --from=base $HOME/.wine/drive_c/msys64/mingw64/qt5-static/lib $HOME/.wine/drive_c/msys64/mingw64/qt5-static/lib
COPY --from=base $HOME/.wine/drive_c/msys64/mingw64/qt5-static/share/qt5 $HOME/.wine/drive_c/msys64/mingw64/qt5-static/share/qt5
COPY --from=base $HOME/.wine/drive_c/msys64/mingw64/bin $HOME/.wine/drive_c/msys64/mingw64/bin
COPY --from=base $HOME/.wine/drive_c/msys64/mingw64/include $HOME/.wine/drive_c/msys64/mingw64/include
COPY --from=base $HOME/.wine/drive_c/msys64/mingw64/lib $HOME/.wine/drive_c/msys64/mingw64/lib
COPY --from=base $HOME/.wine/drive_c/msys64/mingw64/x86_64-w64-mingw32 $HOME/.wine/drive_c/msys64/mingw64/x86_64-w64-mingw32
RUN apt-get update -qq \
 && apt-get install --no-install-recommends ca-certificates curl software-properties-common apt-transport-https -qq -y \
 && apt-get -qq clean
RUN dpkg --add-architecture i386
RUN curl -sL --retry 10 --retry-delay 60 https://dl.winehq.org/wine-builds/winehq.key | apt-key add -
RUN apt-add-repository https://dl.winehq.org/wine-builds/ubuntu/
RUN apt-add-repository 'deb https://dl.winehq.org/wine-builds/ubuntu/ xenial main'
RUN apt-get update -qq \
 && apt-get install --no-install-recommends winehq-staging -qq -y \
 && apt-get -qq clean
RUN WINEDLLOVERRIDES="mscoree,mshtml=" wineboot \
 && wineserver -w
RUN apt-get update -qq \
 && apt-get install --no-install-recommends patch -qq -y \
 && apt-get -qq clean
ADD https://raw.githubusercontent.com/therecipe/qt/master/internal/docker/wine/rspfile.patch $HOME/.wine/drive_c/go/rspfile.patch
RUN cd $HOME/.wine/drive_c/go \
 && patch -p 1 < rspfile.patch
RUN wine go install cmd/go cmd/link
RUN echo 'REGEDIT4\n\n[HKEY_CURRENT_USER\Environment]\n"QT_DOCKER"="true"\n"QT_MSYS2"="true"\n"QT_MSYS2_ARCH"="amd64"\n"QT_MSYS2_STATIC"="true"' > env.reg \
 && regedit Z:\env.reg \
 && wineserver -w \
 && rm -f env.reg
RUN wine $GOPATH\bin\qtsetup prep
RUN wine $GOPATH\bin\qtsetup check
RUN wine $GOPATH\bin\qtsetup generate
RUN cd $HOME/.wine/drive_c/gopath/src/github.com/therecipe/qt/internal/examples/widgets/line_edits \
 && wine qtdeploy build windows \
 && rm -rf ./deploy
RUN echo "#!/bin/bash\nexport GOPATH=$(winepath -0 -w $(echo $GOPATH | sed -e 's/:/ /g') | sed -e 's/Z:/;Z:/2g')" > /usr/bin/qtpath \
 && chmod +x /usr/bin/qtpath
RUN echo '#!/bin/bash\nsource qtpath\nwine qtdeploy "$@"' > /usr/bin/qtdeploy \
 && chmod +x /usr/bin/qtdeploy
RUN echo '#!/bin/bash\nsource qtpath\nwine qtminimal "$@"' > /usr/bin/qtminimal \
 && chmod +x /usr/bin/qtminimal
RUN echo '#!/bin/bash\nsource qtpath\nwine qtmoc "$@"' > /usr/bin/qtmoc \
 && chmod +x /usr/bin/qtmoc
RUN echo '#!/bin/bash\nsource qtpath\nwine qtrcc "$@"' > /usr/bin/qtrcc \
 && chmod +x /usr/bin/qtrcc
RUN ln -s $HOME/.wine/drive_c/gopath $HOME/work
