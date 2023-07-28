#  WARNING: This file is deprecated. Each implementation now has its
#  own Dockerfile.
FROM ubuntu:utopic
MAINTAINER Joel Martin <github@martintribe.org>
ENV DEBIAN_FRONTEND="noninteractive"
RUN echo "deb http://dl.bintray.com/sbt/debian /" > /etc/apt/sources.list.d/sbt.list
RUN apt-get update -y
#
#  General dependencies
#
VOLUME /mal
RUN apt-get install --no-install-recommends make wget curl git -y
#  Deps for compiled languages (C, Go, Rust, Nim, etc)
RUN apt-get install --no-install-recommends gcc pkg-config -y
#  Deps for Java-based languages (Clojure, Scala, Java)
RUN apt-get install --no-install-recommends openjdk-7-jdk -y
ENV MAVEN_OPTS="-Duser.home=/mal"
#  Deps for Mono-based languages (C#, VB.Net)
RUN apt-get install --no-install-recommends mono-runtime mono-mcs mono-vbnc -y
#  Deps for node.js languages (JavaScript, CoffeeScript, miniMAL, etc)
RUN apt-get install --no-install-recommends nodejs npm -y
RUN ln -sf nodejs /usr/bin/node
#
#  Implementation specific installs
#
#  GNU awk
RUN apt-get install --no-install-recommends gawk -y
#  Bash
RUN apt-get install --no-install-recommends bash -y
#  C
RUN apt-get install --no-install-recommends libglib2.0 libglib2.0-dev -y
RUN apt-get install --no-install-recommends libffi-dev libreadline-dev libedit2 libedit-dev -y
#  C++
RUN apt-get install --no-install-recommends g++-4.9 libreadline-dev -y
#  Clojure
COPY https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein /usr/local/bin/lein
RUN sudo chmod 0755 /usr/local/bin/lein
ENV LEIN_HOME="/mal/.lein"
ENV LEIN_JVM_OPTS="-Duser.home=/mal"
#  CoffeeScript
RUN npm install coffee-script -g
RUN touch /.coffee_history \
 && chmod go+w /.coffee_history
#  C#
RUN apt-get install --no-install-recommends mono-mcs -y
#  Elixir
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb \
 && dpkg -i erlang-solutions_1.0_all.deb
RUN apt-get update
RUN apt-get install --no-install-recommends elixir -y
#  Erlang R17 (so I can use maps)
RUN apt-get install --no-install-recommends build-essential libncurses5-dev libssl-dev -y
RUN cd /tmp \
 && wget http://www.erlang.org/download/otp_src_17.5.tar.gz \
 && tar -C /tmp -zxf /tmp/otp_src_17.5.tar.gz \
 && cd /tmp/otp_src_17.5 \
 && ./configure \
 && make \
 && make install \
 && rm -rf /tmp/otp_src_17.5 /tmp/otp_src_17.5.tar.gz
#  Rebar for building the Erlang implementation
RUN cd /tmp/ \
 && git clone -q https://github.com/rebar/rebar.git \
 && cd /tmp/rebar \
 && ./bootstrap \
 && cp rebar /usr/local/bin \
 && rm -rf /tmp/rebar
#  Forth
RUN apt-get install --no-install-recommends gforth -y
#  Go
RUN apt-get install --no-install-recommends golang -y
#  Guile
RUN apt-get install --no-install-recommends libunistring-dev libgc-dev autoconf libtool flex gettext texinfo libgmp-dev -y
RUN git clone git://git.sv.gnu.org/guile.git /tmp/guile \
 && cd /tmp/guile \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install
#  Haskell
RUN apt-get install --no-install-recommends ghc haskell-platform libghc-readline-dev libghc-editline-dev -y
#  Java
RUN apt-get install --no-install-recommends maven2 -y
#  JavaScript
#  Already satisfied above
#  Julia
RUN apt-get install --no-install-recommends software-properties-common -y
RUN apt-add-repository -y ppa:staticfloat/juliareleases
RUN apt-get update -y
RUN apt-get install --no-install-recommends julia -y
#  Lua
RUN apt-get install --no-install-recommends lua5.1 lua-rex-pcre luarocks -y
RUN luarocks install linenoise
#  Mal
#  N/A: self-hosted on other language implementations
#  GNU Make
#  Already satisfied as a based dependency for testing
#  miniMAL
RUN npm install minimal-lisp -g
#  Nim
RUN cd /tmp \
 && wget http://nim-lang.org/download/nim-0.17.0.tar.xz \
 && tar xvJf /tmp/nim-0.17.0.tar.xz \
 && cd nim-0.17.0 \
 && make \
 && sh install.sh /usr/local/bin \
 && rm -r /tmp/nim-0.17.0
#  OCaml
RUN apt-get install --no-install-recommends ocaml-batteries-included -y
#  perl
RUN apt-get install --no-install-recommends perl -y
#  PHP
RUN apt-get install --no-install-recommends php5-cli -y
#  PostScript/ghostscript
RUN apt-get install --no-install-recommends ghostscript -y
#  python
RUN apt-get install --no-install-recommends python -y
#  R
RUN apt-get install --no-install-recommends r-base-core -y
#  Racket
RUN apt-get install --no-install-recommends racket -y
#  Ruby
RUN apt-get install --no-install-recommends ruby -y
#  Rust
RUN curl -sf https://raw.githubusercontent.com/brson/multirust/master/blastoff.sh | sh
#  Scala
RUN apt-get install --no-install-recommends sbt -y --force-yes
RUN apt-get install --no-install-recommends scala -y
ENV SBT_OPTS="-Duser.home=/mal"
#  VB.Net
RUN apt-get install --no-install-recommends mono-vbnc -y
#  TODO: move up
#  Factor
RUN apt-get install --no-install-recommends libgtkglext1 -y
RUN cd /usr/lib/x86_64-linux-gnu/ \
 && wget http://downloads.factorcode.org/releases/0.97/factor-linux-x86-64-0.97.tar.gz \
 && tar xvzf factor-linux-x86-64-0.97.tar.gz \
 && ln -sf /usr/lib/x86_64-linux-gnu/factor/factor /usr/bin/factor \
 && rm factor-linux-x86-64-0.97.tar.gz
#  MATLAB is proprietary/licensed. Maybe someday with Octave.
#  Swift is XCode/OS X only
ENV SKIP_IMPLS="matlab swift"
ENV DEBIAN_FRONTEND="newt"
ENV HOME="/"
WORKDIR /mal
