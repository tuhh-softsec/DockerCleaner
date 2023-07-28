#  ##########################################################################
#   Dugong - Scientific Linux Containers for Bioinformatics
#   ========================================================
#
#   Current development is led by Fabiano Menegidio.
#
#   Bioinformatician/Bioinformaticist at the Laboratory of Functional 
#   and Structural Genomics of the Integrated Nucleus of Biotechnology 
#   at the University of Mogi das Cruzes, Brazil.
#   
#   Contact: fabiano.menegidio@biology.bio.br
#   GitHub: https://github.com/fabianomenegidio
#
#
#  ##########################################################################
FROM ubuntu:latest
MAINTAINER Fabiano Menegidio <fabiano.menegidio@biology.bio.br>
#  ############## Metadata
ARG BUILD_DATE
ARG VCS_REF
LABEL org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="e.g. https://github.com/fabianomenegidio/dugong-bioinformatics/tree/master/DugongGUI/XFCE"
LABEL base.image="ubuntu:latest" \
      version="2017040" \
      name="DugongGUI" \
      flavour="Xfce4" \
      base="Ubuntu latest" \
      description="Scientific Linux Container" \
      website="https://fabianomenegidio.github.io/dugong-bioinformatics/" \
      repository="https://github.com/fabianomenegidio/dugong-bioinformatics" \
      license="MIT" \
      tags="Bioinformatics"
#  ##############  Connection ports for controlling the UI:
#   VNC port:5901
#   noVNC webport, connect via http://IP:6901/?password=vncpassword
#  ############## 
ENV DISPLAY=":1"
ENV VNC_PORT="5901"
ENV NO_VNC_PORT="6901"
ENV NO_VNC_HOME="$HOME/noVNC"
ENV VNC_COL_DEPTH="24"
ENV VNC_PW="vncpassword"
EXPOSE $VNC_PORT $NO_VNC_PORT
#  ############## Resolution Chromebook (change to your default resolution)
ENV VNC_RESOLUTION="1366x768"
#  ############## Environment config
ENV DEBIAN_FRONTEND="noninteractive"
ENV HOME="/headless"
ENV STARTUPDIR="/dockerstartup"
ENV INST_SCRIPTS="$HOME/install"
ENV USER="dugong"
ENV NB_USER="dugong"
ENV NB_UID="1000"
ENV CONDA_DIR="$HOME/.conda2/"
COPY .config/GUI/xfce/ $HOME/
COPY .config/scripts $STARTUPDIR
COPY .config/scripts/.bashrc $HOME/.bashrc
COPY .config/install/ $INST_SCRIPTS/
COPY .config/scripts/xstartup $HOME/.vnc/
COPY config/start.sh /usr/local/bin/
COPY config/start-notebook.sh /usr/local/bin/
COPY config/start-singleuser.sh /usr/local/bin/
COPY config/jupyter_notebook_config.py $HOME/.jupyter/
WORKDIR $HOME
#  ############## Add dependencies and update packages
RUN apt-key adv --recv-key --keyserver keyserver.ubuntu.com "A6616109451BBBF2" \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \
 && echo "deb http://packages.linuxmint.com sarah main upstream import backport" >> /etc/apt/sources.list.d/mint.list \
 && echo 'deb http://nebc.nerc.ac.uk/bio-linux/ unstable bio-linux' >> /etc/apt/sources.list.d/biolinux.list \
 && echo 'deb http://ppa.launchpad.net/nebc/bio-linux/ubuntu precise main' >> /etc/apt/sources.list.d/biolinux.list \
 && echo 'deb-src http://ppa.launchpad.net/nebc/bio-linux/ubuntu precise main' >> /etc/apt/sources.list.d/biolinux.list \
 && echo 'deb http://cran.r-project.org/bin/linux/ubuntu precise/' >> /etc/apt/sources.list.d/biolinux.list \
 && apt-get update \
 && apt-get install --no-install-recommends bio-linux-keyring -y --allow-unauthenticated --force-yes \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && apt-get update \
 && apt-get install --no-install-recommends make=4.3-4.1build1 grep=3.8-5 sed=4.9-1 sudo=1.9.13p1-1ubuntu2 dpkg=1.21.21ubuntu1 git=1:2.39.2-1ubuntu1 wget=1.21.3-1ubuntu1 zip=3.0-13 build-essential=12.9ubuntu3 python python-dev python-numpy bzip2=1.0.8-5build1 locales=2.37-0ubuntu2 ca-certificates=20230311 vim=2:9.0.1000-4ubuntu2 gdebi-core=0.9.5.7+nmu6 bash-completion=1:2.11-6ubuntu1 apt-utils=2.6.0 openjdk-8-jre=8u362-ga-0ubuntu2 openssh-server=1:9.0p1-1ubuntu8 unzip=6.0-27ubuntu1 curl=7.88.1-7ubuntu1 ruby=1:3.1 net-tools=2.10-0.1ubuntu3 icedtea-8-plugin zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libqt4-dbus libqt4-network chromium-browser=1:85.0.4183.83-0ubuntu3 chromium-browser-l10n=1:85.0.4183.83-0ubuntu3 chromium-codecs-ffmpeg=1:85.0.4183.83-0ubuntu3 screen=4.9.0-4 mlocate=1.1.18-1ubuntu1 nano=7.2-1 asciinema=2.2.0-1 python-pip -y --allow-unauthenticated \
 && ln -s /usr/bin/chromium-browser /usr/bin/google-chrome \
 && echo "CHROMIUM_FLAGS='--no-sandbox --start-maximized --user-data-dir'" > $HOME/.chromium-browser.init \
 && wget --quiet https://github.com/krallin/tini/releases/download/v0.10.0/tini \
 && echo "1361527f39190a7338a0b434bd8c88ff7233ce7b9a4876f3315c22fce7eca1b0 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini \
 && wget https://mega.nz/linux/MEGAsync/xUbuntu_16.04/amd64/megasync-xUbuntu_16.04_amd64.deb \
 && wget https://mega.nz/linux/MEGAsync/xUbuntu_16.04/amd64/megacmd-xUbuntu_16.04_amd64.deb \
 && gdebi -n megasync-xUbuntu_16.04_amd64.deb \
 && gdebi -n megacmd-xUbuntu_16.04_amd64.deb \
 && wget -qO- https://dl.bintray.com/tigervnc/stable/tigervnc-1.7.0.x86_64.tar.gz | tar xz --strip 1 -C / \
 && mkdir -p $NO_VNC_HOME/utils/websockify \
 && wget -qO- https://github.com/kanaka/noVNC/archive/v0.6.1.tar.gz | tar xz --strip 1 -C $NO_VNC_HOME \
 && wget -qO- https://github.com/kanaka/websockify/archive/v0.8.0.tar.gz | tar xz --strip 1 -C $NO_VNC_HOME/utils/websockify \
 && chmod +x -v $NO_VNC_HOME/utils/*.sh \
 && ln -s $NO_VNC_HOME/vnc_auto.html $NO_VNC_HOME/index.html \
 && apt-get install --no-install-recommends -f \
 && rm megasync-xUbuntu_16.04_amd64.deb megacmd-xUbuntu_16.04_amd64.deb \
 && apt-get update \
 && apt-get install --no-install-recommends libstdc++5=1:3.3.6-30ubuntu2 libvncserver1=0.9.14+dfsg-1 libaio-dev=0.3.113-4 sysstat=12.6.1-1 ksh=20230128 expat=2.5.0-1 libelf-dev=0.188-2.1 desktop-base=12.0.2ubuntu1 exo-utils=4.18.0-1 gtk2-engines-xfce libexo-1-0 libexo-common=4.18.0-1 libgarcon-1-0=4.18.0-1 libgarcon-common=4.18.0-1 libthunarx-2-0 libxfce4ui-1-0 libxfce4util-bin=4.18.1-2 libxfce4util-common=4.18.1-2 libxfconf-0-2 orage=4.16.0-2 thunar=4.18.4-1 gnupg-agent=2.2.40-1ubuntu2 thunar-data=4.18.4-1 thunar-volman=4.18.0-1 xfce-keyboard-shortcuts xfce4-appfinder=4.18.0-1 xfce4-panel=4.18.2-1 xfce4-session=4.18.1-1 xfce4-settings=4.18.2-1ubuntu1 xfconf=4.18.0-2 xfdesktop4=4.18.1-1 libxfce4ui-utils=4.18.2-2 xfdesktop4-data=4.18.1-1 xfwm4=4.18.0-1 xfwm4-themes xubuntu-icon-theme=23.04 xfce4=4.18 supervisor=4.2.1-1ubuntu1 xterm=379-1ubuntu1 xfce4-terminal=1.0.4-1 xfonts-base=1:1.0.5+nmu1 xfonts-100dpi=1:1.0.5 xfonts-75dpi=1:1.0.5 xfonts-cyrillic=1:1.0.5 -y --allow-unauthenticated \
 && apt-get purge --auto-remove xfce4-power-manager \
 && apt-get autoremove -y \
 && apt-get clean -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && locale-gen en_US.UTF-8 \
 && git clone https://github.com/Linuxbrew/linuxbrew.git ~/.linuxbrew \
 && bash -c "$( curl -L https://basemount.basespace.illumina.com/install ;)" \
 && wget --quiet https://repo.continuum.io/miniconda/Miniconda2-latest-Linux-x86_64.sh \
 && /bin/bash Miniconda2-latest-Linux-x86_64.sh -b -p ~/.conda2 \
 && rm Miniconda2-latest-Linux-x86_64.sh \
 && ~/.conda2/bin/conda config --add channels defaults \
 && ~/.conda2/bin/conda config --add channels conda-forge \
 && ~/.conda2/bin/conda config --add channels r \
 && ~/.conda2/bin/conda config --add channels bioconda \
 && ~/.conda2/bin/conda update --all \
 && ~/.conda2/bin/conda clean -tipsy \
 && chmod +X /dockerstartup/*.sh \
 && chmod 755 /dockerstartup/*.sh \
 && /dockerstartup/generate_container_user.sh \
 && chmod 755 $HOME/.vnc/xstartup \
 && chown dugong:dugong $HOME/.vnc/xstartup \
 && chmod +x /usr/local/bin/start.sh \
 && chmod +x /usr/local/bin/start-notebook.sh \
 && chmod +x /usr/local/bin/start-singleuser.sh \
 && mkdir -p /headless/.cache/sessions/ \
 && echo "" > /headless/.cache/sessions/xfce4-session-DugongGUI:1 \
 && /bin/bash /headless/install/set_user_permission.sh /dockerstartup /headless
USER dugong
RUN ~/.conda2/bin/conda update --all \
 && ~/.conda2/bin/conda install spyder anaconda-navigator jupyter ipython nb_conda \
 && ~/.conda2/bin/conda clean -tipsy
USER root
#  ############## http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="en_US.UTF-8  "
ENV LANGUAGE="en_US:en  "
ENV LC_ALL="en_US.UTF-8"
ENV LANG="C.UTF-8"
VOLUME ["$HOME/data"]
EXPOSE 8888/tcp
ENTRYPOINT ["tini", "--"]
ENTRYPOINT ["/dockerstartup/vnc_startup.sh"]
CMD ["start-notebook.sh"]
RUN chmod 777 /var/tmp \
 && chown -R root:root /var/tmp \
 && chmod 777 /tmp \
 && chown -R root:root /tmp \
 && chown -R $USER:$USER $HOME/data
USER dugong
WORKDIR $HOME/data
# Please add your HEALTHCHECK here!!!
