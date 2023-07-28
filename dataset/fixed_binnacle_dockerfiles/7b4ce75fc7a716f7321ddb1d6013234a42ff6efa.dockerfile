FROM ubuntu:16.04
#  #######################################
#   System Stuff
#  #######################################
ENV TERM="screen-256color"
ENV DEBIAN_FRONTEND="noninteractive"
#   Update and install
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 software-properties-common=0.96.20.10 git=1:2.7.4-0ubuntu1.10 locales=2.23-0ubuntu11.3 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python3-dev=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 ctags shellcheck=0.3.7-5 build-essential=12.1ubuntu2 openssh-client=1:7.2p2-4ubuntu2.10 openssh-server=1:7.2p2-4ubuntu2.10 silversearcher-ag=0.31.0-2 -y
#   setting up ssh
RUN mkdir -p /var/run/sshd
RUN useradd vim -m -p idatadev -s /bin/bash
RUN echo "vim:idatadev" | chpasswd
RUN echo "vim ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
#   Generally a good idea to have these, extensions sometimes need them
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#   Install vim8 and neovim, must vim first, or neovim will replace vim
#   https://github.com/PegasusWang/vim-config
RUN add-apt-repository ppa:jonathonf/vim
RUN apt-get update \
 && apt-get install --no-install-recommends vim=2:7.4.1689-3ubuntu1.5 -y
RUN add-apt-repository ppa:neovim-ppa/stable
RUN apt-get update \
 && apt-get install --no-install-recommends neovim -y
RUN mkdir ~/.config
RUN git clone https://github.com/pegasuswang/vim-config ~/.config/nvim
RUN curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
#   install https://github.com/junegunn/vim-plug
RUN curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
#  #######################################
#   Python
#  #######################################
#   Install python linting and neovim plugin
RUN pip install pip==23.1 --upgrade
RUN pip3 install --upgrade pip
RUN pip install neovim==0.3.1 jedi==0.18.2 flake8==6.0.0 flake8-docstrings==1.7.0 flake8-isort==6.0.0 flake8-quotes==3.3.2 pep8-naming==0.13.3 pep257==0.7.0 isort==5.12.0 autopep8==2.0.2 PyYAML==6.0
RUN pip3 install neovim jedi flake8 flake8-docstrings flake8-isort flake8-quotes pep8-naming pep257 isort mypy ansible-lint autopep8 PyYAML
#  #######################################
#   Install nvm nodejs
#   https://stackoverflow.com/questions/25899912/install-nvm-in-docker
#  #######################################
#   Replace shell with bash so we can source files
ENV NVM_DIR="/root/.nvm"
ENV NODE_VERSION="8.9.0"
#   Install nvm with node and npm install plugins for vim
RUN rm /bin/sh \
 && ln -s /bin/bash /bin/sh \
 && curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash \
 && . $NVM_DIR/nvm.sh \
 && nvm install $NODE_VERSION \
 && nvm alias default $NODE_VERSION \
 && nvm use default \
 && npm install eslint@8.38.0 prettier@2.8.7 jsctags@5.2.2 tern@0.24.3 -g
ENV NODE_PATH="$NVM_DIR/v$NODE_VERSION/lib/node_modules"
ENV PATH="$NVM_DIR/v$NODE_VERSION/bin:$PATH"
#  #######################################
#   Personalizations
#  #######################################
COPY .vimrc /root/.vimrc
RUN vim +silent +PlugInstall +qall
#   RUN cd /root/.config/nvim && make test && make
RUN cd /root/.config/nvim \
 && git pull origin master \
 && make silent_install
#   ENTRYPOINT [ "nvim" ]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
