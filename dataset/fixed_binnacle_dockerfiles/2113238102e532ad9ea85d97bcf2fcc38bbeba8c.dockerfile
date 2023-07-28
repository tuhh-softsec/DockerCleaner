#   Build handbrake image  
#   docker build -t handbrake .  
#   Run handbrake GUI in a container  
#   docker run -it --rm \  
#   --net=host \  
#   --device=/dev/cdrom \  
#   -v /tmp/.X11-unix:/tmp/.X11-unix \  
#   -v $HOME/.Xauthority:/root/.Xauthority \  
#   -v $HOME/handbrake:/handbrake \  
#   --env="DISPLAY" \  
#   handbrake  
#   Run HandBrakeCLI in a container  
#   docker run -it --rm \  
#   --entrypoint=HandBrakeCLI \  
#   -v $HOME/handbrake:/handbrake \  
#   handbrake  
#   This Dockerfile uses Docker Multi-Stage Builds and requires Docker 17.05+  
#   See https://docs.docker.com/engine/userguide/eng-image/multistage-build/  
#   Base Image  
FROM ubuntu:16.04 AS base
LABEL maintainer="\"Micheal Waltz <ecliptik@gmail.com>\"  "
#   Environment variables  
ENV DEBIAN_FRONTEND="noninteractive" \
    LANG="C" \
    LANGUAGE="C" \
    LC_ALL="C"
WORKDIR /handbrake  
#   Base/Runtime packages  
RUN apt-get update \
 && apt-get install --no-install-recommends libmp3lame0=3.99.5+repack1-9build1 libvorbis0a=1.3.5-3ubuntu0.2 libass5=0.13.1-1 libsamplerate0=0.1.8-8 libtheora0=1.1.1+dfsg.1-8 libvorbisenc2=1.3.5-3ubuntu0.2 libx264-148=2:0.148.2643+git5c65704-1 libjansson4=2.7-3ubuntu0.1 libopus0=1.1.2-1ubuntu1 libnotify4=0.7.6-2svn1 libdbus-glib-1-2=0.106-1 libgstreamer-plugins-base1.0-0=1.8.3-1ubuntu0.3 libwebkitgtk-3.0-0=2.4.11-0ubuntu0.1 libdvd-pkg=1.4.0-1-1 -y \
 && apt-get clean
#   Build image  
FROM base AS build
#   Build packages  
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-9 automake=1:1.15-4ubuntu1 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 libass-dev=0.13.1-1 libbz2-dev=1.0.6-8ubuntu0.2 libfontconfig1-dev=2.11.94-0ubuntu1.1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libfribidi-dev=0.19.7-1 libharfbuzz-dev=1.0.1-1ubuntu0.1 libjansson-dev=2.7-3ubuntu0.1 libmp3lame-dev=3.99.5+repack1-9build1 libogg-dev=1.3.2-1 libopus-dev=1.1.2-1ubuntu1 libsamplerate-dev libtheora-dev=1.1.1+dfsg.1-8 libtool=2.4.6-0.1 libvorbis-dev=1.3.5-3ubuntu0.2 libx264-dev=2:0.148.2643+git5c65704-1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 m4=1.4.17-5 make=4.1-6 patch=2.7.5-1ubuntu0.16.04.2 pkg-config=0.29.1-0ubuntu1 python=2.7.12-1~16.04 tar=1.28-2.1ubuntu0.2 yasm=1.3.0-2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libtool-bin=2.4.6-0.1 intltool=0.51.0-2ubuntu1.16.04.1 libappindicator-dev=12.10.1+16.04.20170215-0ubuntu1 libdbus-glib-1-dev=0.106-1 libglib2.0-dev=2.48.2-0ubuntu4.8 libgstreamer1.0-dev=1.8.3-1~ubuntu0.1 libgstreamer-plugins-base1.0-dev=1.8.3-1ubuntu0.3 libgtk-3-dev=3.18.9-1ubuntu3.3 libgudev-1.0-dev=1:230-2 libnotify-dev=0.7.6-2svn1 libwebkitgtk-3.0-dev=2.4.11-0ubuntu0.1 wget=1.17.1-1ubuntu1.5 -y \
 && apt-get clean
#   Copy source to build  
COPY . /handbrake/HandBrake
WORKDIR /handbrake/HandBrake  
RUN ./configure --launch-jobs=$( nproc ;) --launch
RUN cd build \
 && make install
#   Runtime image  
FROM base AS runtime
#   Copy HandBrakeCLI and ghb binaries to runtime image  
COPY --from=build /handbrake/HandBrake/build/HandBrakeCLI /usr/local/bin
COPY --from=build /handbrake/HandBrake/build/gtk/src/ghb /usr/local/bin
#   Make binaries executable  
RUN chmod +x /usr/local/bin/HandBrakeCLI
RUN chmod +x /usr/local/bin/ghb
#   Runtime command (defaults to HandBrake GUI)  
ENTRYPOINT ["ghb"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
