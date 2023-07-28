FROM ubuntu:18.04
MAINTAINER Yuvi Panda <yuvipanda@gmail.com>
ENV EDITOR="/bin/nano"
ENV PYWIKIBOT2_DIR="/srv/paws"
ENV DEBIAN_FRONTEND="noninteractive"
#   Use bash as default shell, rather than sh
ENV SHELL="/bin/bash"
#   Set up user
ENV NB_USER="tools.paws"
ENV NB_UID="52771"
ENV HOME="/home/paws"
RUN adduser --disabled-password --gecos "Default user" --uid ${NB_UID} --home ${HOME} --force-badname ${NB_USER}
WORKDIR ${HOME}
#   Base building utilities that'll always be required, probably
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 locales=2.27-3ubuntu1.6 pkg-config=0.29.1-0ubuntu2 build-essential=12.4ubuntu1 gcc=4:7.4.0-1ubuntu2.3 apt-transport-https=1.6.14 --yes )
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#   Setup nodesource, because node on Ubuntu is far too old to be useful
COPY node/nodesource.gpg.key /etc/apt/trusted.gpg.d/nodesource.gpg.key
COPY node/nodesource.list /etc/apt/sources.list.d/nodesource.list
RUN apt-key add /etc/apt/trusted.gpg.d/nodesource.gpg.key
RUN :
#   Install languages needed and their core dev packages
RUN (apt-get update ;apt-get install --no-install-recommends python3=3.6.7-1~18.04 python3-dev=3.6.7-1~18.04 python3-venv=3.6.7-1~18.04 r-recommended=3.4.4-1ubuntu1 r-base-dev=3.4.4-1ubuntu1 nodejs=8.10.0~dfsg-2ubuntu0.4 --yes )
#   Utilities
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 less=487-0.1 dnsutils=1:9.11.3+dfsg-1ubuntu1.18 emacs=47.0 links=2.14-5build1 nano=2.9.3-2 vim=2:8.0.1453-1ubuntu1.11 mariadb-client=1:10.1.48-0ubuntu0.18.04.1 --yes )
#   Machine-learning type stuff
RUN (apt-get update ;apt-get install --no-install-recommends libblas-dev=3.7.1-4ubuntu1 liblapack-dev=3.7.1-4ubuntu1 libquadmath0=8.4.0-1ubuntu1~18.04 gfortran=4:7.4.0-1ubuntu2.3 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxslt1-dev=1.1.29-5ubuntu0.3 libfreetype6-dev=2.8.1-2ubuntu2.2 libpng-dev=1.6.34-1ubuntu0.18.04.2 libzmq3-dev=4.2.5-1ubuntu0.2 libreadline-dev=7.0-3 libmariadb-client-lgpl-dev libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 pandoc=1.19.2.4~dfsg-1build4 texlive-xetex=2017.20180305-1 --yes )
RUN mkdir -p /srv/paws \
 && chown ${NB_USER}:${NB_USER} /srv/paws
ENV PATH="/srv/paws/pwb:/srv/paws/bin:/srv/paws:$PATH"
USER ${NB_USER}
RUN python3.6 -m venv /srv/paws
#   Install base notebook packages
RUN pip install jupyterhub==0.9.0 notebook==6.5.4 jupyterlab==3.6.3 bash_kernel==0.9.0 --no-cache-dir
#   Install the bash kernel
RUN python -m bash_kernel.install --sys-prefix
#   Install the R Kernel and libraries
COPY install-r /usr/local/bin/install-r
#   We need root simply to install the kernelspec lol
#   https://github.com/IRkernel/IRkernel/issues/495
USER root
RUN /usr/local/bin/install-r
#   Install mass amount of python libraries!
COPY --chown=tools.paws:tools.paws requirements.txt /tmp/requirements.txt
USER ${NB_USER}
RUN pip install --no-cache-dir -r /tmp/requirements.txt
#   Install pywikibot
RUN git clone --branch stable --recursive https://gerrit.wikimedia.org/r/pywikibot/core.git /srv/paws/pwb
COPY --chown=tools.paws:tools.paws user-config.py /srv/paws/
COPY --chown=tools.paws:tools.paws user-fixes.py /srv/paws/
COPY install-pwb /usr/local/bin/
RUN /usr/local/bin/install-pwb
COPY install-extensions /usr/local/bin/
RUN /usr/local/bin/install-extensions
COPY banner /etc/bash.bashrc
#   Install nbpawspublic as a hack
RUN pip install git+https://github.com/yuvipanda/nbpawspublic@023c27b --no-cache-dir \
 && jupyter nbextension install --py nbpawspublic --sys-prefix \
 && jupyter nbextension enable --py nbpawspublic --sys-prefix
RUN pip install git+https://github.com/yuvipanda/ipynb-paws@60c94e3 --no-cache-dir
#   use custom css to hide clusters tab
COPY --chown=tools.paws:tools.paws hide_clusters_tab.css /home/paws/.jupyter/custom/custom.css
USER ${NB_USER}
EXPOSE 8888/tcp
# Please add your HEALTHCHECK here!!!
