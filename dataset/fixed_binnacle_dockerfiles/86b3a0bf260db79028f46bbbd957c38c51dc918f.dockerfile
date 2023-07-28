FROM ubuntu:18.04
MAINTAINER evan hazlett <ejhazlett@gmail.com>
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 bash-completion=1:2.8-1ubuntu1 gcc=4:7.4.0-1ubuntu2.3 dnsutils=1:9.11.3+dfsg-1ubuntu1.18 git-core make=4.1-9.1ubuntu1 bc=1.07.1-2 jq=1.5+dfsg-2 bzr=2.7.0+bzr6622-10 man-db=2.8.3-2ubuntu0.1 locales=2.27-3ubuntu1.6 python=2.7.15~rc1-1 python-dev=2.7.15~rc1-1 python-setuptools=39.0.1-2ubuntu0.1 autoconf=2.69-11 gawk=1:4.1.4+dfsg-1build1 libtool=2.4.6-2 libncurses5-dev=6.1-1ubuntu1.18.04 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 mercurial=4.5.3-1ubuntu2.2 aufs-tools=1:4.9+20170918-1ubuntu1 libbz2-dev=1.0.6-8.1ubuntu0.2 libreadline-dev=7.0-3 gettext=0.19.8.1-6ubuntu0.3 htop=2.1.0-3 tmux=2.6-3ubuntu0.3 wget=1.19.4-1ubuntu2.2 sysstat=11.6.1-1ubuntu0.2 curl=7.58.0-2ubuntu3.24 sudo=1.8.21p2-3ubuntu1.5 socat=1.7.3.2-2ubuntu2 ctags libsqlite3-dev=3.22.0-1ubuntu0.7 libdevmapper-dev=2:1.02.145-4.1ubuntu3.18.04.3 rng-tools=5-0ubuntu4 s3cmd=2.0.1-2 libapparmor1=2.12-4ubuntu5.1 libseccomp2=2.5.1-1ubuntu1~18.04.2 apache2-utils=2.4.29-1ubuntu4.27 unzip=6.0-21ubuntu1.2 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 -y
#   base config
ENV CONTAINER_USER="hatter"
RUN useradd $CONTAINER_USER
RUN echo "ALL ALL = (ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN cp /usr/share/zoneinfo/America/Indianapolis /etc/localtime
#   vim
RUN git clone https://github.com/vim/vim /tmp/vim
RUN (cd /tmp/vim \
 && ./configure --prefix=/usr/local --enable-gui=no --without-x --disable-nls --enable-multibyte --with-tlib=ncurses --enable-pythoninterp --with-features=huge \
 && make install )
#   go
ENV GO_VERSION="1.11.1"
RUN wget https://storage.googleapis.com/golang/go$GO_VERSION.linux-amd64.tar.gz -O /tmp/go.tar.gz \
 && tar -C /usr/local -xvf /tmp/go.tar.gz \
 && rm /tmp/go.tar.gz
WORKDIR /home/$CONTAINER_USER
ENV HOME="/home/$CONTAINER_USER"
ENV SHELL="/bin/bash"
COPY . $HOME/.dotfiles
RUN (cd $HOME/.dotfiles \
 && git submodule init \
 && git submodule update --recursive )
#   env config
RUN mkdir -p $HOME/.ssh \
 && ln -sf $HOME/.dotfiles/vim $HOME/.vim \
 && ln -sf $HOME/.dotfiles/bashrc $HOME/.bashrc \
 && ln -sf $HOME/.dotfiles/vimrc $HOME/.vimrc \
 && sed -i 's/^colorscheme.*//g' $HOME/.dotfiles/vimrc \
 && vim +PluginInstall +qall > /dev/null 2>&1
RUN (cd $HOME/.dotfiles \
 && git checkout vimrc \
 && ln -sf $HOME/.dotfiles/gitconfig $HOME/.gitconfig \
 && ln -sf $HOME/.dotfiles/ssh_config $HOME/.ssh/config \
 && chown $CONTAINER_USER:$CONTAINER_USER $HOME/.ssh/config \
 && chmod 600 $HOME/.ssh/config \
 && ln -sf $HOME/.dotfiles/known_hosts $HOME/.ssh/known_hosts \
 && ln -sf $HOME/.dotfiles/tmux.conf $HOME/.tmux.conf )
#   go config
ENV GOROOT="/usr/local/go"
ENV GOPATH="$HOME/dev/gocode"
ENV PATH="/usr/local/go/bin:$GOPATH/bin:$PATH"
#   go tools
RUN go get -v golang.org/x/tools/present \
 && go get -v golang.org/x/tools/cmd/goimports \
 && go get -v golang.org/x/lint/golint \
 && go get -v github.com/LK4D4/vndr \
 && go get -v github.com/stevvooe/protobuild \
 && go get -v github.com/mdempsky/gocode
#   proto
RUN git clone https://github.com/google/protobuf /tmp/protobuf \
 && cd /tmp/protobuf \
 && ./autogen.sh \
 && ./configure \
 && make install
RUN go get -v github.com/golang/protobuf/protoc-gen-go
RUN go get -v github.com/gogo/protobuf/protoc-gen-gofast
RUN go get -v github.com/gogo/protobuf/proto
RUN go get -v github.com/gogo/protobuf/gogoproto
RUN go get -v github.com/gogo/protobuf/protoc-gen-gogo
RUN go get -v github.com/gogo/protobuf/protoc-gen-gogofast
#   nvm
RUN cd $HOME \
 && git clone https://github.com/creationix/nvm .nvm
#   latest docker binary
ENV DOCKER_VERSION="18.06.1-ce"
RUN curl -sSL https://download.docker.com/linux/static/edge/x86_64/docker-${DOCKER_VERSION}.tgz -o /tmp/docker-latest.tgz \
 && tar zxf /tmp/docker-latest.tgz -C /usr/local/bin --strip 1 \
 && rm -rf /tmp/docker-latest.tgz
#   perms
RUN chown -R $CONTAINER_USER:$CONTAINER_USER $HOME \
 && groupadd -g 2000 docker \
 && usermod -aG docker $CONTAINER_USER \
 && usermod -aG users $CONTAINER_USER \
 && usermod -aG staff $CONTAINER_USER
VOLUME /home/$CONTAINER_USER
#   user
USER $CONTAINER_USER
CMD ["bash"]
# Please add your HEALTHCHECK here!!!
