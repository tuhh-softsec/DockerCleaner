FROM ubuntu:18.04
MAINTAINER Cody Hiar <codyfh@gmail.com>
# #######################################
#  System Stuff
# #######################################
#  Better terminal support
ENV TERM="screen-256color"
ENV DEBIAN_FRONTEND="noninteractive"
#  Update and install
RUN apt-get update \
 && apt-get install --no-install-recommends htop bash curl wget git software-properties-common python-dev python-pip python3-dev python3-pip ctags shellcheck netcat ack-grep sqlite3 unzip libssl-dev libffi-dev locales cmake -y
#  Generally a good idea to have these, extensions sometimes need them
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#  Install Neovim
RUN add-apt-repository ppa:neovim-ppa/stable
RUN apt-get update \
 && apt-get install --no-install-recommends neovim -y
#  Install Ledger
RUN add-apt-repository ppa:mbudde/ledger
RUN apt-get update \
 && apt-get install --no-install-recommends ledger -y
#  Install Terraform for linting
RUN wget https://releases.hashicorp.com/terraform/0.11.8/terraform_0.11.8_linux_amd64.zip \
 && unzip terraform_0.11.8_linux_amd64.zip \
 && mv terraform /usr/bin
#  Ubuntu ranger old and doesn't support 'wrap_scroll'.
RUN git clone https://github.com/thornycrackers/ranger.git /tmp/ranger \
 && cd /tmp/ranger \
 && make install
# #######################################
#  Python
# #######################################
#  Install python linting and neovim plugin
COPY py2_requirements.txt /opt/py2_requirements.txt
RUN cd /opt \
 && pip2 install -r py2_requirements.txt
COPY py3_requirements.txt /opt/py3_requirements.txt
RUN cd /opt \
 && pip3 install -r py3_requirements.txt
# #######################################
#  Personalizations
# #######################################
#  Add some aliases
COPY bashrc /root/.bashrc
#  Add my git config
COPY gitconfig /etc/gitconfig
#  Change the workdir, Put it inside root so I can see neovim settings in finder
WORKDIR /root/app
#  Neovim needs this so that <ctrl-h> can work
RUN infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > /tmp/$TERM.ti
RUN tic /tmp/$TERM.ti
#  Command for the image
CMD ["/bin/bash"]
#  Add nvim config. Put this last since it changes often
COPY nvim /root/.config/nvim
#  Install neovim plugins
RUN nvim -i NONE -c PlugInstall -c quitall > /dev/null 2>&1
RUN cd /root/.config/nvim/plugged/YouCompleteMe \
 && python3 install.py
#  Add flake8 config, don't trigger a long build process
COPY flake8 /root/.flake8
#  Add local vim-options, can override the one inside
COPY vim-options /root/.config/nvim/plugged/vim-options
#  Add isort config, also changes often
COPY isort.cfg /root/.isort.cfg
#  Add ranger config
COPY rc.conf /root/.config/ranger/rc.conf
