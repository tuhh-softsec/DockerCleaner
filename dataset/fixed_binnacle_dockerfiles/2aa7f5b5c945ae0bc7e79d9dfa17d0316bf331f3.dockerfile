FROM ubuntu:14.04
SHELL ["/bin/bash", "-c"]
ENV DEBIAN_FRONTEND="noninteractive"
#   ENV DEBIAN_FRONTEND newt
ENV CLICOLOR="1"
ENV LSCOLORS="GxFxCxDxBxegedabagaced"
ENV GREP_OPTIONS="--color=auto"
RUN echo America/Los_Angeles | sudo tee /etc/timezone \
 && sudo dpkg-reconfigure --frontend noninteractive tzdata
RUN : \
 && apt-get upgrade -qq -y
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 vim=2:7.4.052-1ubuntu3.1 curl=7.35.0-1ubuntu2.20 wget=1.15-1ubuntu1.14.04.5 gawk=1:4.0.1+dfsg-2.1ubuntu2 sed=4.2.2-4ubuntu1 findutils=4.4.2-7 bc=1.06.95-8ubuntu1 less=458-2 htop=1.0.2-3 man unzip=6.0-9ubuntu1.5 git=1:1.9.1-1ubuntu0.10 lsb-release=4.1+Debian11ubuntu6.2 build-essential=11.6ubuntu6 make=3.81-8.2ubuntu3 python-all=2.7.5-5ubuntu3 libssl-dev=1.0.1f-1ubuntu2.27 -qq -y )
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:6.6p1-2ubuntu2.13 -qq -y )
ENV HOME="/root"
ENV NVM_DIR="$HOME/.nvm"
ENV NODE_VERSION="v4.3.0"
ENV YARN_VERSION="0.17.8"
RUN wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.31.1/install.sh | /bin/bash
RUN source $NVM_DIR/nvm.sh ; nvm install $NODE_VERSION \
 && nvm alias default $NODE_VERSION \
 && nvm use default
RUN (apt-get update ;apt-get install --no-install-recommends ca-certificates=20170717~14.04.2 g++=4:4.8.2-1ubuntu6 gcc=4:4.8.2-1ubuntu6 libc6-dev=2.19-0ubuntu6.15 curl=7.35.0-1ubuntu2.20 mercurial=2.8.2-1ubuntu1.4 bzr=2.6.0+bzr6593-1ubuntu1.6 git-core=1:1.9.1-1ubuntu0.10 -qq -y )
ENV GOLANG_VERSION="1.6.4"
ENV GOLANG_DOWNLOAD_URL="https://storage.googleapis.com/golang/go$GOLANG_VERSION.linux-amd64.tar.gz"
RUN curl -s $GOLANG_DOWNLOAD_URL | tar -C /usr/local -xz
ENV GOPATH="/go"
ENV GOROOT="/usr/local/go"
ENV PATH="/usr/local/go/bin:/go/bin:/usr/local/bin:$PATH"
RUN go get -u -x -v -a github.com/bpowers/browsix-gopherjs
RUN source $NVM_DIR/nvm.sh ; echo "nvm: " `nvm --version ` \
 && echo "node: " `node --version ` \
 && echo "npm: " `npm --version ` \
 && npm install source-map-support@0.5.21 gulp@"^3.9.1" bower@"^1.7.9" -g
RUN echo -e '{''\n'' "allow_root": true''\n''}' >> $HOME/.bowerrc
RUN cat $HOME/.bowerrc
RUN source $NVM_DIR/nvm.sh ; npm install bfs-buffer@"^0.1.7" bfs-path@"^0.1.2" bower@"^1.7.9" browser-sync@"^2.13.0" browserfs-browsix-tmp@"0.5.15" browserify@"^13.0.1" chai@"^3.5.0" connect-history-api-fallback@"^1.1.0" del@"^2.2.1" gulp@"^3.9.1" gulp-autoprefixer@"^3.1.0" gulp-cache@"^0.4.5" gulp-changed@"^1.3.0" gulp-chmod@"^1.3.0" gulp-copy@"0.0.2" gulp-cssmin@"^0.1.7" gulp-if@"^2.0.0" gulp-load-plugins@"^1.2.0" gulp-minify-html@"^1.0.5" gulp-mocha@"^2.2.0" gulp-rename@"^1.2.2" gulp-replace@"^0.5.4" gulp-run@"^1.6.12" gulp-size@"^2.0.0" gulp-typescript@"2.13.6" gulp-uglify@"^1.5.4" gulp-useref@"^2.1.0" gulp-util@"^3.0.6" gulp-vulcanize@"^6.1.0" karma@"^1.1.1" karma-chai@"^0.1.0" karma-chrome-launcher@"^1.0.1" karma-firefox-launcher@"^1.0.0" karma-mocha@"^1.1.1" merge2@"^1.0.0" mocha@"^2.5.3" proxy-middleware@"^0.15.0" through2@"^2.0.1" typescript@"^1.8.10" typings@"^1.3.1" vinyl-buffer@"^1.0.0" vinyl-source-stream@"^1.1.0" node-binary-marshal@"^0.4.2" term.js@"github:bpowers/term.js" -g
RUN source $NVM_DIR/nvm.sh ; npm install tsd@"^0.6.5" gulp-tslint@"^6.0.1" gulp-imagemin@"^2.4.0" tslint@"^3.13.0" -g
#   RUN \
#     source $NVM_DIR/nvm.sh; \
#     npm install -g \
#       node-pipe2@"^0.2.0" \
#       node-priority@"^0.1.5"
RUN mkdir -p $HOME/browsix $HOME/browsix-test
COPY . $HOME/browsix
WORKDIR $HOME/browsix
RUN ls
CMD /bin/bash
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
