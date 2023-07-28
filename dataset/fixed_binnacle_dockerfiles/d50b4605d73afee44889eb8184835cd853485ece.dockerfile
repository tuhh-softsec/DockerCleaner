FROM alpine:edge
WORKDIR /tmp
RUN echo "http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk update \
 && apk upgrade \
 && apk add ghc=9.4.4-r1 cabal=3.10.1.0-r0 make=4.4.1-r0 gcc=12.2.1_git20220924-r9 musl-dev=1.2.3_git20230322-r0 linux-headers=6.2-r0 bash=5.2.15-r2 file=5.44-r2 curl=8.0.1-r1 bsd-compat-headers=0.7.2-r4 autoconf=2.71-r2 automake=1.16.5-r2 protobuf-dev=3.21.12-r0 zlib-dev=1.2.13-r0 openssl-dev=3.1.0-r2 g++=12.2.1_git20220924-r9 upx=4.0.2-r0
ENV dest_prefix="/usr"
#   libevent
ENV libevent_version="2.0.22"
ENV libevent_name="libevent-$libevent_version-stable"
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/$libevent_name.tar.gz https://github.com/libevent/libevent/releases/download/release-$libevent_version-stable/libevent-$libevent_version-stable.tar.gz
RUN tar xvzf /tmp/$libevent_name.tar.gz \
 && cd $libevent_name \
 && ./configure --prefix=$dest_prefix --disable-shared \
 && make \
 && make install \
 && rm -fr /tmp/$libevent_name.tar.gz /tmp/$libevent_name
#   ncurses
ENV ncurses_version="6.0"
ENV ncurses_name="ncurses-$ncurses_version"
RUN curl -LO ftp://ftp.gnu.org/gnu/ncurses/$ncurses_name.tar.gz -o /tmp/$ncurses_name.tar.gz \
 && tar xvzf /tmp/$ncurses_name.tar.gz \
 && cd $ncurses_name \
 && ./configure --prefix=$dest_prefix --without-cxx --without-cxx-bindings --enable-static \
 && make \
 && make install \
 && rm -fr /tmp/$ncurses_name.tar.gz /tmp/$ncurses_name
#   et tmux
ENV tmux_version="2.4"
ENV tmux_name="tmux-$tmux_version"
ENV tmux_url="$tmux_name/$tmux_name"
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/$tmux_name.tar.gz https://github.com/tmux/tmux/releases/download/$tmux_version/$tmux_name.tar.gz
RUN tar xvzf /tmp/$tmux_name.tar.gz \
 && cd $tmux_name \
 && ./configure --prefix=$dest_prefix CFLAGS="-I$dest_prefix/include -I$dest_prefix/include/ncurses" LDFLAGS="-static -L$dest_prefix/lib -L$dest_prefix/include/ncurses -L$dest_prefix/include" \
 && env CPPFLAGS="-I$dest_prefix/include -I$dest_prefix/include/ncurses" LDFLAGS="-static -L$dest_prefix/lib -L$dest_prefix/include/ncurses -L$dest_prefix/include" make \
 && make install \
 && rm -fr /tmp/$tmux_name.tar.gz /tmp/$tmux_name \
 && cp /usr/bin/tmux /usr/bin/tmux.stripped \
 && strip /usr/bin/tmux.stripped \
 && cp /usr/bin/tmux /usr/bin/tmux.upx \
 && cp /usr/bin/tmux.stripped /usr/bin/tmux.stripped.upx \
 && upx --best --ultra-brute /usr/bin/tmux.upx /usr/bin/tmux.stripped.upx
#   htop
ENV htop_version="2.0.2"
ENV htop_name="htop-$htop_version"
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/$htop_name.tar.gz http://hisham.hm/htop/releases/$htop_version/$htop_name.tar.gz
RUN tar xvzf /tmp/$htop_name.tar.gz \
 && cd $htop_name \
 && ./configure --enable-static --disable-shared --disable-unicode --prefix=$dest_prefix CFLAGS="-I$dest_prefix/include -I$dest_prefix/include/ncurses" LDFLAGS="--static -lpthread -L$dest_prefix/lib -L$dest_prefix/include/ncurses -L$dest_prefix/include" \
 && env CPPFLAGS="-I$dest_prefix/include -I$dest_prefix/include/ncurses" LDFLAGS="--static -lpthread -L$dest_prefix/lib -L$dest_prefix/include/ncurses -L$dest_prefix/include" make \
 && make install \
 && rm -fr /tmp/$htop_name.tar.gz /tmp/$htop_name \
 && cp $dest_prefix/bin/htop $dest_prefix/bin/htop.stripped \
 && strip $dest_prefix/bin/htop.stripped \
 && cp $dest_prefix/bin/htop $dest_prefix/bin/htop.upx \
 && cp $dest_prefix/bin/htop.stripped $dest_prefix/bin/htop.stripped.upx \
 && upx --best --ultra-brute $dest_prefix/bin/htop.upx $dest_prefix/bin/htop.stripped.upx
#   mobile shell
ENV mosh_version="1.3.0"
ENV mosh_name="mosh-$mosh_version"
ENV mosh_url="https://github.com/mobile-shell/mosh/archive/$mosh_name.tar.gz"
COPY $mosh_url /tmp/$mosh_name.tar.gz
RUN tar xvzf /tmp/$mosh_name.tar.gz \
 && cd /tmp/mosh-$mosh_name \
 && ./autogen.sh \
 && ./configure --enable-static --disable-shared --prefix=$dest_prefix CFLAGS="-I$dest_prefix/include -I$dest_prefix/include/ncurses" LDFLAGS="--static -lpthread -L$dest_prefix/lib -L$dest_prefix/include/ncurses -L$dest_prefix/include" \
 && make \
 && make install \
 && rm -fr /tmp/mosh-$mosh_name \
 && cp $dest_prefix/bin/mosh-client $dest_prefix/bin/mosh-client.stripped \
 && strip $dest_prefix/bin/mosh-client.stripped \
 && cp $dest_prefix/bin/mosh-server $dest_prefix/bin/mosh-server.stripped \
 && strip $dest_prefix/bin/mosh-server.stripped \
 && cp $dest_prefix/bin/mosh-client $dest_prefix/bin/mosh-client.upx \
 && cp $dest_prefix/bin/mosh-client.stripped $dest_prefix/bin/mosh-client.stripped.upx \
 && cp $dest_prefix/bin/mosh-server $dest_prefix/bin/mosh-server.upx \
 && cp $dest_prefix/bin/mosh-server.stripped $dest_prefix/bin/mosh-server.stripped.upx \
 && upx --best --ultra-brute $dest_prefix/bin/mosh-client.upx $dest_prefix/bin/mosh-client.stripped.upx $dest_prefix/bin/mosh-server.upx $dest_prefix/bin/mosh-server.stripped.upx
#   pandoc
ENV pandoc_version="1.19.1"
ENV cabaldir="/root/.cabal/bin"
WORKDIR /tmp
RUN cabal update \
 && cabal install hsb2hs \
 && cabal get pandoc-$pandoc_version
RUN cd /tmp/pandoc-$pandoc_version \
 && sed -i '/Executable pandoc/a \ \ ld-options: -static' pandoc.cabal \
 && cabal install --flags="embed_data_files" \
 && cp $cabaldir/pandoc $cabaldir/pandoc.stripped \
 && strip $cabaldir/pandoc.stripped \
 && cp $cabaldir/pandoc $cabaldir/pandoc.upx \
 && cp $cabaldir/pandoc.stripped $cabaldir/pandoc.stripped.upx \
 && upx --best --ultra-brute $cabaldir/pandoc.upx $cabaldir/pandoc.stripped.upx
#   oniguruma for jq regex support
ENV oni_version="5.9.6"
ENV oni="onig-$oni_version"
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/$oni.tar.gz https://github.com/kkos/oniguruma/releases/download/v$oni_version/onig-$oni_version.tar.gz
WORKDIR /tmp
RUN tar xvzf /tmp/$oni.tar.gz \
 && cd /tmp/$oni \
 && ./configure --enable-static --disable-shared --prefix=$dest_prefix \
 && make \
 && make install \
 && rm -fr /tmp/$oni
#   jq as well
ENV jq_version="1.5"
ENV jq="jq-$jq_version"
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/jq.tar.gz https://github.com/stedolan/jq/releases/download/jq-1.5/jq-1.5.tar.gz
WORKDIR /tmp
RUN tar xvzf /tmp/jq.tar.gz \
 && cd /tmp/$jq \
 && ./configure --enable-static --disable-shared --prefix=$dest_prefix CFLAGS="-I$dest_prefix/include" LDFLAGS="--static -L$dest_prefix/lib -L$dest_prefix/include" \
 && make \
 && make install \
 && rm -fr /tmp/$jq \
 && cp $dest_prefix/bin/jq $dest_prefix/bin/jq.stripped \
 && strip $dest_prefix/bin/jq.stripped \
 && cp $dest_prefix/bin/jq $dest_prefix/bin/jq.upx \
 && cp $dest_prefix/bin/jq.stripped $dest_prefix/bin/jq.stripped.upx \
 && upx --best --ultra-brute $dest_prefix/bin/jq.stripped.upx $dest_prefix/bin/jq.upx
CMD ["bash"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
