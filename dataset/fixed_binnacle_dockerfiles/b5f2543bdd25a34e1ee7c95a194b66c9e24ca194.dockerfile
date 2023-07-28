FROM ubuntu:18.04 AS build-native-env
ENV OPENCV_VERSION="4.1.0"
#  ENV OPENCVSHARP_VERSION=4.1.0.20190416
#  ENV DOTNETCORE_SDK=2.1.104
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.6.14 software-properties-common=0.96.24.32.20 wget=1.19.4-1ubuntu2.2 unzip=6.0-21ubuntu1.2 curl=7.58.0-2ubuntu3.24 ca-certificates=20211016ubuntu0.18.04.1 -y
#  bzip2 \
#  grep sed dpkg 
#   Install opencv dependencies
RUN cd ~
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 cmake=3.10.2-1ubuntu2.18.04.2 git=1:2.17.1-1ubuntu0.17 gfortran=4:7.4.0-1ubuntu2.3 libjpeg8-dev=8c-2ubuntu8 libpng-dev=1.6.34-1ubuntu0.18.04.2 software-properties-common=0.96.24.32.20 -y
RUN add-apt-repository "deb http://security.ubuntu.com/ubuntu xenial-security main" \
 && apt-get update \
 && apt-get install --no-install-recommends libjasper1 libtiff-dev=4.0.9-5ubuntu0.10 libavcodec-dev=7:3.4.11-0ubuntu0.1 libavformat-dev=7:3.4.11-0ubuntu0.1 libswscale-dev=7:3.4.11-0ubuntu0.1 libdc1394-22-dev=2.2.5-1 libxine2-dev=1.2.8-2build2 libv4l-dev=1.14.2-1 -y
RUN cd /usr/include/linux
RUN ln -s -f ../libv4l1-videodev.h videodev.h
RUN cd ~
RUN apt-get install --no-install-recommends libgstreamer1.0-dev=1.14.5-0ubuntu1~18.04.2 libgstreamer-plugins-base1.0-dev=1.14.5-0ubuntu1~18.04.3 libgtk2.0-dev=2.24.32-1ubuntu1 libtbb-dev=2017~U7-8 qt5-default=5.9.5+dfsg-0ubuntu2.6 libatlas-base-dev=3.10.3-5 libfaac-dev=1.29.7.7-1 libmp3lame-dev=3.100-2 libtheora-dev=1.1.1+dfsg.1-14 libvorbis-dev=1.3.5-4.2 libxvidcore-dev=2:1.3.5-1 libopencore-amrnb-dev=0.1.3-2.1 libopencore-amrwb-dev=0.1.3-2.1 libavresample-dev=7:3.4.11-0ubuntu0.1 x264=2:0.152.2854+gite9a5903-2 v4l-utils=1.14.2-1 -y
#   Setup OpenCV source
RUN wget https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.zip \
 && unzip ${OPENCV_VERSION}.zip \
 && rm ${OPENCV_VERSION}.zip \
 && mv opencv-${OPENCV_VERSION} opencv
#   Setup opencv-contrib Source
RUN wget https://github.com/opencv/opencv_contrib/archive/${OPENCV_VERSION}.zip \
 && unzip ${OPENCV_VERSION}.zip \
 && rm ${OPENCV_VERSION}.zip \
 && mv opencv_contrib-${OPENCV_VERSION} opencv_contrib
#   Build OpenCV
RUN cd opencv \
 && mkdir build \
 && cd build \
 && cmake -D OPENCV_EXTRA_MODULES_PATH=/opencv_contrib/modules -D CMAKE_BUILD_TYPE=RELEASE -D BUILD_EXAMPLES=OFF -D BUILD_DOCS=OFF -D BUILD_PERF_TESTS=OFF -D BUILD_TESTS=OFF -D BUILD_opencv_java=OFF -D BUILD_opencv_python=OFF .. \
 && make -j4 \
 && make install \
 && ldconfig
WORKDIR /
#   Download OpenCvSharp
RUN git clone https://github.com/shimat/opencvsharp.git
RUN cd opencvsharp \
 && git fetch --all --tags --prune
#   Install the Extern lib.
WORKDIR /opencvsharp/src
RUN mkdir /opencvsharp/make
RUN cd /opencvsharp/make \
 && cmake -D CMAKE_INSTALL_PREFIX=/opencvsharp/make /opencvsharp/src \
 && make -j4 \
 && make install
RUN ls /opencvsharp/make
FROM microsoft/dotnet:2.1-sdk AS build-dotnet-env
COPY --from=build-native-env /opencvsharp/make/OpenCvSharpExtern/libOpenCvSharpExtern.so ./
RUN git clone https://github.com/shimat/opencvsharp.git
RUN pwd
RUN ls
#   Install Build the C# part of OpenCvSharp
WORKDIR /opencvsharp/src/OpenCvSharp
RUN cd /opencvsharp/src/OpenCvSharp \
 && dotnet restore
RUN dotnet build -c Release -f netstandard2.0
WORKDIR /opencvsharp/src/OpenCvSharp.Blob
RUN cd /opencvsharp/src/OpenCvSharp.Blob \
 && dotnet restore
RUN dotnet build -c Release -f netstandard2.0
WORKDIR /opencvsharp/src/OpenCvSharp.Extensions
RUN cd /opencvsharp/src/OpenCvSharp.Extensions \
 && dotnet restore
RUN dotnet build -c Release -f netstandard2.0
RUN mkdir /opencvsharp/build
WORKDIR /opencvsharp/build
RUN cp /libOpenCvSharpExtern.so .
RUN cp /opencvsharp/src/OpenCvSharp/bin/Release/netstandard2.0/* .
RUN cp /opencvsharp/src/OpenCvSharp.Blob/bin/Release/netstandard2.0/* .
RUN cp /opencvsharp/src/OpenCvSharp.Extensions/bin/Release/netstandard2.0/* .
RUN pwd
RUN ls
FROM microsoft/dotnet:2.2-runtime
WORKDIR /app
COPY --from=build-dotnet-env /opencvsharp/build ./
RUN pwd
RUN ls
#  ENTRYPOINT ["ls"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
