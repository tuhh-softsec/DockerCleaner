FROM ubuntu:18.04
MAINTAINER lhhung<lhhung@uw.edu>
#  Dockerfile for widget development container
#  comment to force rebuild
ENV DEBIAN_FRONTEND="noninteractive"
ENV HOME="/root"
#  base files/utils to be used inside container
RUN apt-get update \
 && apt-get install --no-install-recommends supervisor=3.3.1-1.1 pwgen=2.08-1 sudo=1.8.21p2-3ubuntu1.5 nano=2.9.3-2 net-tools=1.60+git20161116.90da8a0-1ubuntu1 fluxbox=1.3.5-2build1 feh=2.23.2-1build1 xterm=330-1ubuntu2.2 x11vnc=0.9.13-3 xvfb=2:1.19.6-1ubuntu4.14 gtk2-engines-murrine=0.98.2-2ubuntu1 ttf-ubuntu-font-family=1:0.83-2 fonts-wqy-microhei=0.2.0-beta-3 language-pack-zh-hant=1:18.04+20180712 language-pack-gnome-zh-hant=1:18.04+20180712 nginx=1.14.0-0ubuntu1.11 mesa-utils=8.4.0-1 libgl1-mesa-dri=20.0.8-0ubuntu1~18.04.1 -y --force-yes \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
#  files for  vnc framebuffer
RUN apt-get update \
 && apt-get install --no-install-recommends wget=1.19.4-1ubuntu2.2 libssl1.0 -y \
 && chdir /tmp \
 && wget --no-check-certificate --content-disposition https://github.com/BioDepot/BioDepot-workflow-builder/blob/master/noVNC/x11vnc-data_0.9.14-1.1ubuntu1_all.deb?raw=true \
 && wget --no-check-certificate --content-disposition https://github.com/BioDepot/BioDepot-workflow-builder/blob/master/noVNC/x11vnc_0.9.14-1.1ubuntu1_amd64.deb?raw=true \
 && dpkg -i /tmp/x11vnc*.deb \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* \
 && chdir /root \
 && rm /tmp/*.deb
#  files for web interface noVNC
COPY web /web/
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 gcc=4:7.4.0-1ubuntu2.3 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-dev=2.7.15~rc1-1 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 -y \
 && pip install pip==9.0.3 --upgrade \
 && pip install setuptools==67.6.1 -U \
 && pip install -r /web/requirements.txt \
 && pip3 install docker \
 && apt-get remove -y gcc build-essential python-pip python-dev python3-pip \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
COPY noVNC /noVNC/
#  files for orange and biodepot
RUN apt-get update \
 && apt-get install --no-install-recommends virtualenv=15.1.0+ds-1.1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libqt5webkit5-dev=5.212.0~alpha2-7ubuntu1 python3-pyqt5=5.10.1+dfsg-1ubuntu2 python3-pyqt5.qtsvg=5.10.1+dfsg-1ubuntu2 python3-pyqt5.qtwebkit=5.10.1+dfsg-1ubuntu2 -y
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
RUN virtualenv --python=python3 --system-site-packages orange3venv
RUN source orange3venv/bin/activate
COPY orange3 orange3
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 gcc=4:7.4.0-1ubuntu2.3 python-dev=2.7.15~rc1-1 python3-dev=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python-pip=9.0.1-2.3~ubuntu1.18.04.8 -y \
 && pip3 install --upgrade pip==9.0.3 \
 && pip install numpy==1.24.2 \
 && pip3 install -U setuptools \
 && pip3 install -r orange3/requirements-core.txt \
 && pip3 install -r orange3/requirements-gui.txt \
 && pip3 install docker pysam beautifulsoup4 \
 && pip3 install -e orange3 \
 && apt-get remove -y gcc build-essential \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
#  install Docker-ce
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.6.14 ca-certificates=20211016ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 software-properties-common=0.96.24.32.20 gnupg2=2.2.4-1ubuntu1.6 -y \
 && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add - \
 && apt-get remove -y curl gnupg2 \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
RUN add-apt-repository -y "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable" \
 && apt-get update \
 && apt-get install --no-install-recommends docker-ce docker-ce-cli containerd.io -y \
 && apt-get remove -y apt-transport-https software-properties-common \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
#  nginx and supervisor setup
COPY supervisord.conf /etc/supervisor/conf.d/
COPY nginx.conf /etc/nginx/sites-enabled/default
#  jsonpickle
RUN pip3 install --user jsonpickle
#  put biodepot here and keep pip for rapid updates
COPY biodepot biodepot
RUN pip3 install -e biodepot
COPY startup.sh /
EXPOSE 6080/tcp
WORKDIR /data
#  install rsync
#  install rsync curl and docker-compose
RUN apt-get update \
 && apt-get install --no-install-recommends rsync=3.1.2-2.1ubuntu1.6 curl=7.58.0-2ubuntu3.24 -y \
 && curl -L "https://github.com/docker/compose/releases/download/1.23.2/docker-compose-$( uname -s ;)-$( uname -m ;)" -o /usr/local/bin/docker-compose \
 && chmod +x /usr/local/bin/docker-compose \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/**
#  Change app name to Bwb
RUN sed -i 's/\"Orange Canvas\"/\"Bwb\"/' /orange3/Orange/canvas/config.py
#  set up some config files
COPY fluxbox_config/ /root/.fluxbox/
COPY user_config/ /root/
#  patch orange3
COPY orangePatches/schemeedit.py /orange3/Orange/canvas/document/schemeedit.py
COPY orangePatches/canvasmain.py /orange3/Orange/canvas/application/canvasmain.py
COPY orangePatches/widgetsscheme.py /orange3/Orange/canvas/scheme/widgetsscheme.py
COPY orangePatches/signalmanager.py /orange3/Orange/canvas/scheme/signalmanager.py
COPY orangePatches/link.py /orange3/Orange/canvas/scheme/link.py
COPY orangePatches/signals.py /orange3/Orange/widgets/utils/signals.py
#  add bwb start scripts
COPY scripts/startBwb.sh /bin/startBwb.sh
COPY scripts/runDockerJob.sh /bin/runDockerJob.sh
COPY scripts/startScheduler.sh /bin/startScheduler.sh
#  add widgets and workflows
COPY widgets /widgets/
COPY workflows /workflows/
COPY notebooks /notebooks/
COPY templates /templates/
COPY coreutils /coreutils/
COPY icons /icons/
COPY tutorialFiles /tutorialFiles
COPY serverSettings.json /biodepot
#  start it up
CMD /startup.sh \
 && /usr/bin/supervisord -n -c /etc/supervisor/supervisord.conf
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
