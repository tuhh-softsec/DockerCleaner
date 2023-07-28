FROM ubuntu:18.04
MAINTAINER Cody Hiar <codyfh@gmail.com>
#  #######################################
#   System Stuff
#  #######################################
#   Better terminal support
ENV TERM="screen-256color"
ENV DEBIAN_FRONTEND="noninteractive"
#   Update and install
RUN apt-get update \
 && apt-get install --no-install-recommends htop=2.1.0-3 bash=4.4.18-2ubuntu1.3 curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 git=1:2.17.1-1ubuntu0.17 software-properties-common=0.96.24.32.20 python-dev=2.7.15~rc1-1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-dev=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 ctags shellcheck=0.4.6-1 netcat=1.10-41.1 ack-grep=2.22-1 sqlite3=3.22.0-1ubuntu0.7 unzip=6.0-21ubuntu1.2 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libffi-dev=3.2.1-8 locales=2.27-3ubuntu1.6 cmake=3.10.2-1ubuntu2.18.04.2 -y
#   Generally a good idea to have these, extensions sometimes need them
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#   Install Neovim
RUN add-apt-repository ppa:neovim-ppa/stable
RUN apt-get update \
 && apt-get install --no-install-recommends neovim=0.2.2-3 -y
#   Install Ledger
RUN add-apt-repository ppa:mbudde/ledger
RUN apt-get update \
 && apt-get install --no-install-recommends ledger=3.1.2~pre1+g3a00e1c+dfsg1-5build5 -y
#   Install Terraform for linting
RUN wget https://releases.hashicorp.com/terraform/0.11.8/terraform_0.11.8_linux_amd64.zip \
 && unzip terraform_0.11.8_linux_amd64.zip \
 && mv terraform /usr/bin
#   Ubuntu ranger old and doesn't support 'wrap_scroll'.
RUN git clone https://github.com/thornycrackers/ranger.git /tmp/ranger \
 && cd /tmp/ranger \
 && make install
#  #######################################
#   Python
#  #######################################
#   Install python linting and neovim plugin
COPY py2_requirements.txt /opt/py2_requirements.txt
RUN cd /opt \
 && pip2 install -r py2_requirements.txt
COPY py3_requirements.txt /opt/py3_requirements.txt
RUN cd /opt \
 && pip3 install -r py3_requirements.txt
#  #######################################
#   Personalizations
#  #######################################
#   Add some aliases
COPY bashrc /root/.bashrc
#   Add my git config
COPY gitconfig /etc/gitconfig
#   Change the workdir, Put it inside root so I can see neovim settings in finder
WORKDIR /root/app
#   Neovim needs this so that <ctrl-h> can work
RUN infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > /tmp/$TERM.ti
RUN tic /tmp/$TERM.ti
#   Command for the image
CMD ["/bin/bash"]
#   Add nvim config. Put this last since it changes often
COPY nvim /root/.config/nvim
#   Install neovim plugins
RUN nvim -i NONE -c PlugInstall -c quitall > /dev/null 2>&1
RUN cd /root/.config/nvim/plugged/YouCompleteMe \
 && python3 install.py
#   Add flake8 config, don't trigger a long build process
COPY flake8 /root/.flake8
#   Add local vim-options, can override the one inside
COPY vim-options /root/.config/nvim/plugged/vim-options
#   Add isort config, also changes often
COPY isort.cfg /root/.isort.cfg
#   Add ranger config
COPY rc.conf /root/.config/ranger/rc.conf
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
