#   MIT License
#   Copyright (c) 2016 Chris R
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#   The above copyright notice and this permission notice shall be included in all
#   copies or substantial portions of the Software.
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#   SOFTWARE.
FROM ubuntu:16.04
ENV DEBIAN_FRONTEND="noninteractive"
ARG WINE_VERSION=winehq-staging
ARG PYTHON_VERSION=3.6.4
ARG PYINSTALLER_VERSION=3.4
#   we need wine for this all to work, so we'll use the PPA
RUN set -x \
 && dpkg --add-architecture i386 \
 && apt-get update -qy \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 zip=3.0-11 -qfy \
 && wget -nv https://dl.winehq.org/wine-builds/winehq.key \
 && apt-key add winehq.key \
 && add-apt-repository 'https://dl.winehq.org/wine-builds/ubuntu/' \
 && apt-get update -qy \
 && apt-get install --no-install-recommends winbind=2:4.3.11+dfsg-0ubuntu0.16.04.34 cabextract=1.6-1 $WINE_VERSION -qfy \
 && apt-get clean \
 && wget -nv https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks \
 && chmod +x winetricks \
 && mv winetricks /usr/local/bin
#   wine settings
ENV WINEARCH="win64"
ENV WINEDEBUG="fixme-all"
ENV WINEPREFIX="/wine"
#   PYPI repository location
ENV PYPI_URL="https://pypi.python.org/"
#   PYPI index location
ENV PYPI_INDEX_URL="https://pypi.python.org/simple"
#   install python in wine, using the msi packages to install, extracting
#   the files directly, since installing isn't running correctly.
RUN set -x \
 && winetricks win7 \
 && for msifile in `echo core dev exe lib path pip tcltk tools `; do wget -nv "https://www.python.org/ftp/python/$PYTHON_VERSION/amd64/${msifile}.msi" ;wine msiexec /i "${msifile}.msi" /qb TARGETDIR=C:/Python36 ;rm ${msifile}.msi ; done \
 && cd /wine/drive_c/Python36 \
 && echo 'wine '''C:Python36python.exe''' "$@"' > /usr/bin/python \
 && echo 'wine '''C:Python36Scriptseasy_install.exe''' "$@"' > /usr/bin/easy_install \
 && echo 'wine '''C:Python36Scriptspip.exe''' "$@"' > /usr/bin/pip \
 && echo 'wine '''C:Python36Scriptspyinstaller.exe''' "$@"' > /usr/bin/pyinstaller \
 && echo 'wine '''C:Python36Scriptspyupdater.exe''' "$@"' > /usr/bin/pyupdater \
 && echo 'assoc .py=PythonScript' | wine cmd \
 && echo 'ftype PythonScript=c:\Python36\python.exe "%1" %*' | wine cmd \
 && while pgrep wineserver > /dev/null; do echo "Waiting for wineserver" ;sleep 1 ; done \
 && chmod +x /usr/bin/python /usr/bin/easy_install /usr/bin/pip /usr/bin/pyinstaller /usr/bin/pyupdater \
 && (pip install pip==23.1 -U || true ) \
 && rm -rf /tmp/.wine-*
ENV W_DRIVE_C="/wine/drive_c"
ENV W_WINDIR_UNIX="$W_DRIVE_C/windows"
ENV W_SYSTEM64_DLLS="$W_WINDIR_UNIX/system32"
ENV W_TMP="$W_DRIVE_C/windows/temp/_$0"
#   install Microsoft Visual C++ Redistributable for Visual Studio 2017 dll files
RUN set -x \
 && rm -f "$W_TMP"/* \
 && wget -P "$W_TMP" https://download.visualstudio.microsoft.com/download/pr/11100230/15ccb3f02745c7b206ad10373cbca89b/VC_redist.x64.exe \
 && cabextract -q --directory="$W_TMP" "$W_TMP"/VC_redist.x64.exe \
 && cabextract -q --directory="$W_TMP" "$W_TMP/a10" \
 && cabextract -q --directory="$W_TMP" "$W_TMP/a11" \
 && cd "$W_TMP" \
 && rename 's/_/\-/g' *.dll \
 && cp "$W_TMP"/*.dll "$W_SYSTEM64_DLLS"/
#   put the src folder inside wine
RUN mkdir /src/ \
 && ln -s /src /wine/drive_c/src
VOLUME /src/
WORKDIR /wine/drive_c/src/
RUN mkdir -p /wine/drive_c/tmp
RUN pip install pyinstaller==$PYINSTALLER_VERSION
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
