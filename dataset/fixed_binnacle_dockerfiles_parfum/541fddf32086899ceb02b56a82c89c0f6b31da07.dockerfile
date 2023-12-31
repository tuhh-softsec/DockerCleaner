FROM ubuntu:18.04
MAINTAINER lhhung<lhhung@uw.edu>
# Dockerfile for widget development container
ENV DEBIAN_FRONTEND="noninteractive"
ENV HOME="/root"
# base files/utils to be used inside container
RUN apt-get update \
 && apt-get install --no-install-recommends supervisor pwgen sudo nano net-tools fluxbox feh xterm x11vnc xvfb gtk2-engines-murrine ttf-ubuntu-font-family fonts-wqy-microhei language-pack-zh-hant language-pack-gnome-zh-hant nginx mesa-utils libgl1-mesa-dri -y --force-yes \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
# files for  vnc framebuffer
RUN apt-get update \
 && apt-get install --no-install-recommends wget libssl1.0 -y \
 && chdir /tmp \
 && wget --no-check-certificate --content-disposition https://github.com/BioDepot/BioDepot-workflow-builder/blob/master/noVNC/x11vnc-data_0.9.14-1.1ubuntu1_all.deb?raw=true \
 && wget --no-check-certificate --content-disposition https://github.com/BioDepot/BioDepot-workflow-builder/blob/master/noVNC/x11vnc_0.9.14-1.1ubuntu1_amd64.deb?raw=true \
 && dpkg -i /tmp/x11vnc*.deb \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* \
 && chdir /root \
 && rm /tmp/*.deb
# files for web interface noVNC
COPY web /web/
RUN apt-get update \
 && apt-get install --no-install-recommends docker.io build-essential gcc python-pip python-dev python3-pip -y \
 && pip install pip==9.0.3 --upgrade \
 && pip install setuptools -U \
 && pip install -r /web/requirements.txt \
 && pip3 install docker \
 && apt-get remove -y gcc build-essential python-pip python-dev python3-pip \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
COPY noVNC /noVNC/
# files for orange and biodepot
RUN apt-get update \
 && apt-get install --no-install-recommends virtualenv libssl-dev libqt5webkit5-dev python3-pyqt5 python3-pyqt5.qtsvg python3-pyqt5.qtwebkit -y
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh
RUN virtualenv --python=python3 --system-site-packages orange3venv
RUN source orange3venv/bin/activate
COPY orange3 orange3
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential gcc python-dev python3-dev python3-pip python-pip -y \
 && pip3 install --upgrade pip==9.0.3 \
 && pip install numpy \
 && pip3 install -U setuptools \
 && pip3 install -r orange3/requirements-core.txt \
 && pip3 install -r orange3/requirements-gui.txt \
 && pip3 install docker pysam beautifulsoup4 \
 && pip3 install -e orange3 \
 && apt-get remove -y gcc build-essential \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
# install Docker-ce
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https ca-certificates curl software-properties-common gnupg2 -y \
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
# nginx and supervisor setup
COPY supervisord.conf /etc/supervisor/conf.d/
COPY nginx.conf /etc/nginx/sites-enabled/default
# jsonpickle
RUN pip3 install --user jsonpickle
# put biodepot here and keep pip for rapid updates
COPY biodepot biodepot
RUN pip3 install -e biodepot
ADD startup.sh /
EXPOSE 6080/tcp
WORKDIR /data
# install rsync curl and docker-compose
RUN apt-get update \
 && apt-get install --no-install-recommends rsync curl -y \
 && curl -L "https://github.com/docker/compose/releases/download/1.23.2/docker-compose-$( uname -s ;)-$( uname -m ;)" -o /usr/local/bin/docker-compose \
 && chmod +x /usr/local/bin/docker-compose \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
# Change app name to Bwb
RUN sed -i 's/\"Orange Canvas\"/\"Bwb\"/' /orange3/Orange/canvas/config.py
# devfiles
RUN apt-get update \
 && apt-get install --no-install-recommends wget geany firefox openssh-client -y
# set up some config files
COPY fluxbox_config/ /root/.fluxbox/
COPY user_config/ /root/
# add config files for dev tools
COPY dev-files/menu /root/.fluxbox/
COPY dev-files/geany/ /root/.config/
# patch orange3
COPY orangePatches/schemeedit.py /orange3/Orange/canvas/document/schemeedit.py
COPY orangePatches/canvasmain.py /orange3/Orange/canvas/application/canvasmain.py
COPY orangePatches/widgetsscheme.py /orange3/Orange/canvas/scheme/widgetsscheme.py
COPY orangePatches/signalmanager.py /orange3/Orange/canvas/scheme/signalmanager.py
COPY orangePatches/link.py /orange3/Orange/canvas/scheme/link.py
COPY orangePatches/signals.py /orange3/Orange/widgets/utils/signals.py
# add bwb start scripts
COPY scripts/startBwb.sh /bin/startBwb.sh
COPY scripts/runDockerJob.sh /bin/runDockerJob.sh
COPY scripts/startScheduler.sh /bin/startScheduler.sh
# add widgets and workflows
COPY widgets /widgets/
COPY workflows /workflows/
COPY notebooks /notebooks/
COPY templates /templates/
COPY coreutils /coreutils/
COPY icons /icons/
COPY tutorialFiles /tutorialFiles
COPY serverSettings.json /biodepot
COPY bwb-scheduling-service /bwb-scheduling-service/
# start it up
CMD /startup.sh \
 && /usr/bin/supervisord -n -c /etc/supervisor/supervisord.conf
