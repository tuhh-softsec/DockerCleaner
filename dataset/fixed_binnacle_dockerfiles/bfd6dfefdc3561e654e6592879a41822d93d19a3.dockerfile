#  ####
#
#   Used to generate the 'unidata/cloudstream' docker container.
#   Visit us on the web at http://www.unidata.ucar.edu
#
#   Copyright Unidata 2015 - 2018
#
#  ####
FROM ubuntu:trusty
MAINTAINER Ward Fisher <wfisher@ucar.edu>
ENV CLOUDSTREAM_VERSION="development"
#   Set the following, required for IDV 5.5+
#   ENV MESA_GL_VERSION_OVERRIDE 3.1
#  ####
#   Update base docker image.
#  ####
ENV DEBIAN_FRONTEND="noninteractive"
RUN : \
 && apt-get -y upgrade
#  ##
#   Install bare minimum set of tools.
#  ##
RUN (apt-get update ;apt-get install --no-install-recommends nano=2.2.6-1ubuntu1 curl=7.35.0-1ubuntu2.20 x11vnc=0.9.13-1.1 xvfb=2:1.15.1-0ubuntu2.11 xinit=1.3.2-1 git=1:1.9.1-1ubuntu0.10 python=2.7.5-5ubuntu3 zip=3.0-8 xdg-utils=1.1.0~rc1-2ubuntu7.2 firefox=66.0.3+build1-0ubuntu0.14.04.1 python3-tk=3.4.3-1~14.04.2 python3-pexpect=3.1-1ubuntu0.1 fluxbox=1.3.5-2 x11-apps=7.7+2 swisswatch=0.6-14 man mesa-utils=8.1.0-2 sudo=1.8.9p5-1ubuntu1.4 bzip2=1.0.6-5 net-tools=1.60-25ubuntu2.1 -y )
#  ##
#   Set up a non-root user account.
#  ##
ENV CUSER="stream"
ENV CUSERPWORD="\"password.1234\""
RUN useradd -ms /bin/bash ${CUSER}
RUN adduser ${CUSER} sudo
#   There is no need for the password to be known, so
#   randomize it for now.
RUN echo "${CUSER}:${CUSERPWORD}${RANDOM} " | chpasswd
RUN echo "${CUSER} ALL=NOPASSWD: ALL" >> /etc/sudoers
#  ####
#   Switch to the non-root user,
#   configure system and environment.
#  ####
USER ${CUSER}
ENV HOME="/home/${CUSER}"
WORKDIR ${HOME}
RUN mkdir ~/.vnc
#  ##
#   Create a .xinitrc file.
#
#   The environmental variable APORT = 5901 by default but can be
#   overridden when invoking 'docker run', e.g.
#     $ docker run -e APORT=4435
#  ##
RUN echo '/usr/bin/x11vnc -display :1 $SHARESTRING -autoport $APORT -repeat -forever &' > ~/.xinitrc.nopassword
RUN echo '/usr/bin/x11vnc -usepw -display :1 $SHARESTRING -autoport $APORT -repeat -forever &' > ~/.xinitrc.password
RUN echo "/usr/bin/startfluxbox" >> ~/.xinitrc.nopassword
RUN echo "/usr/bin/startfluxbox" >> ~/.xinitrc.password
#  ##
#   Configure fluxbox.
#  ##
RUN mkdir ~/.fluxbox/
RUN bash -c 'echo "xterm &" >> ~/.fluxbox/startup'
RUN echo "/usr/bin/fluxbox -log ~/.fluxbox/log" >> ~/.fluxbox/startup
#  ####
#   Set up Websocket-based VNC.
#  ####
#  ##
#   Expose Websocket port for VNC server.
#  ##
ENV APORT="5901"
ENV WPORT="6080"
EXPOSE ${WPORT}
RUN git clone git://github.com/kanaka/noVNC
RUN cp /home/${CUSER}/noVNC/vnc.html /home/${CUSER}/noVNC/index.html
#  #
#   Added an ssl-only option to a fork of noVNC,
#   use the forked version until it is merged into official repo.
#  #
RUN cd /home/${CUSER}/noVNC/utils \
 && git clone https://github.com/kanaka/websockify
#  ##
#   Install rclone from prebuilt binary.
#  ##
USER root
RUN curl -O https://downloads.rclone.org/rclone-current-linux-amd64.zip \
 && unzip -j -d /usr/bin rclone-current-linux-amd64.zip
USER ${CUSER}
#  ##
#   We can parameterize screen dimensions
#   with the following variables.
#   e.g.
#      docker run -p 6080:6080 -e SIZEH=800 -e SIZEW=640 -e CDEPTH=8 \
#                         -it [docker image] bootstrap.sh
#  ##
ENV SIZEW="1680"
ENV SIZEH="1050"
ENV CDEPTH="24"
#  ##
#   Set up an option to allow for sharing.
#  ##
ENV SHARESTRING="--noshared"
#  ##
#   Copy over a few files.
#  ##
COPY start.sh ${HOME}/
COPY bootstrap.sh ${HOME}/
COPY Dockerfile.cloudstream.ubuntu ${HOME}/
COPY Dockerfile.template ${HOME}/
COPY README.md ${HOME}/
COPY COPYRIGHT_CLOUDSTREAM.md ${HOME}/
COPY RELEASE_NOTES.md ${HOME}/
COPY QUICKSTART.md ${HOME}/
RUN mv README.md CLOUDSTREAM_README.md
COPY examples ${HOME}/examples
#  ##
#   Create a version file.
#  ##
ENV VERSION_FILE="VERSION.md"
RUN echo "CloudStream Version: "$CLOUDSTREAM_VERSION" $( date ;)" > $VERSION_FILE
#  ##
#   A little housekeeping.
#  ##
USER root
RUN chown -R ${CUSER}:${CUSER} ${HOME}
USER ${CUSER}
#  ##
#   Run the bootstrap.sh script.
#   This will set up/run the VNC environment,
#   and execute the start.sh file if present.
#  ##
CMD /home/${CUSER}/bootstrap.sh
# Please add your HEALTHCHECK here!!!
