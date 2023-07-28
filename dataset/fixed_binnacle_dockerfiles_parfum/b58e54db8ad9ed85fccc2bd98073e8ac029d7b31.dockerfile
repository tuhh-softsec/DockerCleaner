FROM ubuntu:18.04
MAINTAINER Yuvi Panda <yuvipanda@gmail.com>
ENV EDITOR="/bin/nano"
ENV PYWIKIBOT2_DIR="/srv/paws"
ENV DEBIAN_FRONTEND="noninteractive"
#  Use bash as default shell, rather than sh
ENV SHELL="/bin/bash"
#  Set up user
ENV NB_USER="tools.paws"
ENV NB_UID="52771"
ENV HOME="/home/paws"
RUN adduser --disabled-password --gecos "Default user" --uid ${NB_UID} --home ${HOME} --force-badname ${NB_USER}
WORKDIR ${HOME}
#  Base building utilities that'll always be required, probably
RUN apt-get update --yes
RUN apt-get install --no-install-recommends git locales pkg-config build-essential gcc apt-transport-https --yes
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#  Setup nodesource, because node on Ubuntu is far too old to be useful
COPY node/nodesource.gpg.key /etc/apt/trusted.gpg.d/nodesource.gpg.key
COPY node/nodesource.list /etc/apt/sources.list.d/nodesource.list
RUN apt-key add /etc/apt/trusted.gpg.d/nodesource.gpg.key
RUN apt-get update --yes
#  Install languages needed and their core dev packages
RUN apt-get install --no-install-recommends python3 python3-dev python3-venv r-recommended r-base-dev nodejs --yes
#  Utilities
RUN apt-get install --no-install-recommends curl wget less dnsutils emacs links nano vim mariadb-client --yes
#  Machine-learning type stuff
RUN apt-get install --no-install-recommends libblas-dev liblapack-dev libquadmath0 gfortran libxml2-dev libxslt1-dev libfreetype6-dev libpng-dev libzmq3-dev libreadline-dev libmariadb-client-lgpl-dev libcurl4-openssl-dev libssl-dev pandoc texlive-xetex --yes
RUN mkdir -p /srv/paws \
 && chown ${NB_USER}:${NB_USER} /srv/paws
ENV PATH="/srv/paws/pwb:/srv/paws/bin:/srv/paws:$PATH"
USER ${NB_USER}
RUN python3.6 -m venv /srv/paws
#  Install base notebook packages
RUN pip install jupyterhub==0.9.0 notebook jupyterlab bash_kernel --no-cache-dir
#  Install the bash kernel
RUN python -m bash_kernel.install --sys-prefix
#  Install the R Kernel and libraries
COPY install-r /usr/local/bin/install-r
#  We need root simply to install the kernelspec lol
#  https://github.com/IRkernel/IRkernel/issues/495
USER root
RUN /usr/local/bin/install-r
#  Install mass amount of python libraries!
COPY --chown=tools.paws:tools.paws requirements.txt /tmp/requirements.txt
USER ${NB_USER}
RUN pip install --no-cache-dir -r /tmp/requirements.txt
#  Install pywikibot
RUN git clone --branch stable --recursive https://gerrit.wikimedia.org/r/pywikibot/core.git /srv/paws/pwb
COPY --chown=tools.paws:tools.paws user-config.py /srv/paws/
COPY --chown=tools.paws:tools.paws user-fixes.py /srv/paws/
COPY install-pwb /usr/local/bin/
RUN /usr/local/bin/install-pwb
COPY install-extensions /usr/local/bin/
RUN /usr/local/bin/install-extensions
COPY banner /etc/bash.bashrc
#  Install nbpawspublic as a hack
RUN pip install git+https://github.com/yuvipanda/nbpawspublic@023c27b --no-cache-dir \
 && jupyter nbextension install --py nbpawspublic --sys-prefix \
 && jupyter nbextension enable --py nbpawspublic --sys-prefix
RUN pip install git+https://github.com/yuvipanda/ipynb-paws@60c94e3 --no-cache-dir
#  use custom css to hide clusters tab
COPY --chown=tools.paws:tools.paws hide_clusters_tab.css /home/paws/.jupyter/custom/custom.css
USER ${NB_USER}
EXPOSE 8888/tcp
