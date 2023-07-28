FROM jare/alpine-vim:latest
#   User config
ENV UID="1000" \
    UNAME="developer" \
    GID="1000" \
    GNAME="developer" \
    SHELL="/bin/bash" \
    UHOME="/home/developer"
#   Used to configure YouCompleteMe
ENV GOROOT="/usr/lib/go"
ENV GOBIN="$GOROOT/bin"
ENV GOPATH="$UHOME/workspace"
ENV PATH="$PATH:$GOBIN:$GOPATH/bin"
#   User
RUN apk add sudo=1.9.12_p2-r1 --no-cache \
 && mkdir -p "${UHOME}" \
 && chown "${UID}":"${GID}" "${UHOME}" \
 && echo "${UNAME}:x:${UID}:${GID}:${UNAME},,,:${UHOME}:${SHELL}" >> /etc/passwd \
 && echo "${UNAME}::17032:0:99999:7:::" >> /etc/shadow \
 && echo "${UNAME} ALL=(ALL) NOPASSWD: ALL" > "/etc/sudoers.d/${UNAME}" \
 && chmod 0440 "/etc/sudoers.d/${UNAME}" \
 && echo "${GNAME}:x:${GID}:${UNAME}" >> /etc/group
#   Install Pathogen
RUN apk add curl=7.88.1-r1 --no-cache \
 && mkdir -p $UHOME/bundle $UHOME/.vim/autoload $UHOME/.vim_runtime/temp_dirs \
 && curl -LSso $UHOME/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim \
 && echo "execute pathogen#infect('$UHOME/bundle/{}')" > $UHOME/.vimrc \
 && echo "syntax on " >> $UHOME/.vimrc \
 && echo "filetype plugin indent on " >> $UHOME/.vimrc \
 && apk del curl
#   Vim wrapper
COPY run /usr/local/bin/
#  custom .vimrc stub
RUN mkdir -p /ext \
 && echo " " > /ext/.vimrc
COPY .vimrc $UHOME/my.vimrc
COPY .bashrc $UHOME/.bashrc
#   Vim plugins deps
RUN apk add bash=5.2.15-r0 ctags=5.9.20221002.0-r1 curl=7.88.1-r1 git=2.38.4-r1 ncurses-terminfo=6.3_p20221119-r0 python --update \
 && apk add build-base=0.5-r3 cmake=3.24.4-r0 go=1.19.8-r0 llvm perl=5.36.0-r0 python-dev --virtual build-deps \
 && git clone --depth 1 https://github.com/Valloric/YouCompleteMe $UHOME/bundle/YouCompleteMe/ \
 && cd $UHOME/bundle/YouCompleteMe \
 && git submodule update --init --recursive \
 && $UHOME/bundle/YouCompleteMe/install.py --gocode-completer \
 && git clone --depth 1 https://github.com/Shougo/vimproc.vim $UHOME/bundle/vimproc.vim \
 && cd $UHOME/bundle/vimproc.vim \
 && make \
 && chown $UID:$GID -R $UHOME \
 && apk del build-deps \
 && apk add libxt=1.2.1-r0 libx11=1.8.4-r0 libstdc++=12.2.1_git20220924-r4 \
 && rm -rf $UHOME/bundle/YouCompleteMe/third_party/ycmd/clang_includes $UHOME/bundle/YouCompleteMe/third_party/ycmd/cpp /usr/lib/go /var/cache/* /var/log/* /var/tmp/* \
 && mkdir /var/cache/apk
USER $UNAME
#   Plugins
RUN cd $UHOME/bundle/ \
 && git clone --depth 1 https://github.com/pangloss/vim-javascript \
 && git clone --depth 1 https://github.com/scrooloose/nerdcommenter \
 && git clone --depth 1 https://github.com/godlygeek/tabular \
 && git clone --depth 1 https://github.com/Raimondi/delimitMate \
 && git clone --depth 1 https://github.com/nathanaelkane/vim-indent-guides \
 && git clone --depth 1 https://github.com/groenewege/vim-less \
 && git clone --depth 1 https://github.com/othree/html5.vim \
 && git clone --depth 1 https://github.com/elzr/vim-json \
 && git clone --depth 1 https://github.com/bling/vim-airline \
 && git clone --depth 1 https://github.com/easymotion/vim-easymotion \
 && git clone --depth 1 https://github.com/mbbill/undotree \
 && git clone --depth 1 https://github.com/majutsushi/tagbar \
 && git clone --depth 1 https://github.com/vim-scripts/EasyGrep \
 && git clone --depth 1 https://github.com/jlanzarotta/bufexplorer \
 && git clone --depth 1 https://github.com/kien/ctrlp.vim \
 && git clone --depth 1 https://github.com/scrooloose/nerdtree \
 && git clone --depth 1 https://github.com/jistr/vim-nerdtree-tabs \
 && git clone --depth 1 https://github.com/scrooloose/syntastic \
 && git clone --depth 1 https://github.com/tomtom/tlib_vim \
 && git clone --depth 1 https://github.com/marcweber/vim-addon-mw-utils \
 && git clone --depth 1 https://github.com/vim-scripts/taglist.vim \
 && git clone --depth 1 https://github.com/terryma/vim-expand-region \
 && git clone --depth 1 https://github.com/tpope/vim-fugitive \
 && git clone --depth 1 https://github.com/airblade/vim-gitgutter \
 && git clone --depth 1 https://github.com/fatih/vim-go \
 && git clone --depth 1 https://github.com/plasticboy/vim-markdown \
 && git clone --depth 1 https://github.com/michaeljsmith/vim-indent-object \
 && git clone --depth 1 https://github.com/terryma/vim-multiple-cursors \
 && git clone --depth 1 https://github.com/tpope/vim-repeat \
 && git clone --depth 1 https://github.com/tpope/vim-surround \
 && git clone --depth 1 https://github.com/vim-scripts/mru.vim \
 && git clone --depth 1 https://github.com/vim-scripts/YankRing.vim \
 && git clone --depth 1 https://github.com/tpope/vim-haml \
 && git clone --depth 1 https://github.com/SirVer/ultisnips \
 && git clone --depth 1 https://github.com/honza/vim-snippets \
 && git clone --depth 1 https://github.com/derekwyatt/vim-scala \
 && git clone --depth 1 https://github.com/christoomey/vim-tmux-navigator \
 && git clone --depth 1 https://github.com/ekalinin/Dockerfile.vim \
 && git clone --depth 1 https://github.com/altercation/vim-colors-solarized
#   Build default .vimrc
RUN mv -f $UHOME/.vimrc $UHOME/.vimrc~ \
 && curl -s https://raw.githubusercontent.com/amix/vimrc/master/vimrcs/basic.vim >> $UHOME/.vimrc~ \
 && curl -s https://raw.githubusercontent.com/amix/vimrc/master/vimrcs/extended.vim >> $UHOME/.vimrc~ \
 && cat $UHOME/my.vimrc >> $UHOME/.vimrc~ \
 && rm $UHOME/my.vimrc \
 && sed -i '/colorscheme peaksea/d' $UHOME/.vimrc~
#   Pathogen help tags generation
RUN vim -E -c 'execute pathogen#helptags()' -c q ; return 0
ENV TERM="xterm-256color"
#   List of Vim plugins to disable
ENV DISABLE=""
ENTRYPOINT ["sh", "/usr/local/bin/run"]
# Please add your HEALTHCHECK here!!!
