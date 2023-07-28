FROM ubuntu:14.04
ENV DEBIAN_FRONTEND="noninteractive"
#   update and upgrade packages already installed
RUN :
RUN apt-get -y upgrade
#   install requirements for installing compilers and dependencies
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 unzip=6.0-9ubuntu1.5 software-properties-common=0.92.37.8 cabal-install=1.16.0.2-2 libghc-zlib-dev=0.5.4.1-1 libghc-zlib-bindings-dev=0.1.1.1-3build1 libghc-terminfo-dev=0.3.2.5-3 libpython-dev=2.7.5-5ubuntu3 clang-3.6=1:3.6-2ubuntu1~trusty2 -y )
RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-3.6 100 \
 && update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.6 100
#   add external Debian repositories and update packages
RUN apt-add-repository -y ppa:swi-prolog/stable \
 && :
#   install compiler and interpreter packages
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-7-jdk=7u211-2.6.17-0ubuntu0.1 ruby=1:1.9.3.4 nodejs=0.10.25~dfsg2-2ubuntu1.2 ghc=7.6.3-10 smlnj=110.76-1 php5-cli=5.5.9+dfsg-1ubuntu4.29 racket=5.3.6+dfsg1-1 swi-prolog=6.6.4-2ubuntu1 mono-complete=3.2.8+dfsg-4ubuntu1.1 fsharp=3.0.34+dfsg-3ubuntu1 gcc-multilib=4:4.8.2-1ubuntu6 nasm=2.10.09-1ubuntu0.1 clisp=1:2.49-9ubuntu1 erlang=1:16.b.3-dfsg-1ubuntu2.2 golang=2:1.2.1-2ubuntu1 lua5.2=5.2.3-1 mono-vbnc=3.0~pre20130627.4dcc70f-1 gfortran=4:4.8.2-1ubuntu6 fp-compiler=2.6.2-8 -y )
#   install Scala
ENV SCALA_VERSION="2.11.7"
RUN wget http://downloads.typesafe.com/scala/$SCALA_VERSION/scala-$SCALA_VERSION.deb \
 && dpkg -i scala-$SCALA_VERSION.deb \
 && rm scala-$SCALA_VERSION.deb
#   install Groovy
RUN curl -s http://get.sdkman.io | bash \
 && bash -c "source /root/.sdkman/bin/sdkman-init.sh \
 && yes | sdk install groovy"
#   install Rust
RUN wget https://static.rust-lang.org/rustup.sh \
 && bash rustup.sh -y --channel=stable \
 && rm rustup.sh
#   install Idris
RUN cabal update \
 && cabal install cabal \
 && cabal install idris
ENV PATH="/root/.cabal/bin:$PATH"
#   install Clojure
ENV CLOJURE_VERSION="1.7.0"
RUN wget -P /opt http://central.maven.org/maven2/org/clojure/clojure/$CLOJURE_VERSION/clojure-$CLOJURE_VERSION.jar \
 && echo "java -jar /opt/clojure-$CLOJURE_VERSION.jar $@" > /usr/local/bin/clojure \
 && chmod +x /usr/local/bin/clojure
#   build Joy
RUN mkdir /var/lib/joy \
 && cd /var/lib/joy \
 && wget http://webstat.latrobe.edu.au/url/www.kevinalbrecht.com/code/joy-mirror/joy.tar.gz \
 && tar zxvf joy.tar.gz \
 && make \
 && ln -s /var/lib/joy/joy /usr/local/bin
#   install Kotlin
ENV KOTLIN_VERSION="1.0.0-beta-2423"
RUN wget https://github.com/JetBrains/kotlin/releases/download/build-$KOTLIN_VERSION/kotlin-compiler-$KOTLIN_VERSION.zip \
 && unzip -d /opt kotlin-compiler-$KOTLIN_VERSION.zip \
 && ln -s /opt/kotlinc/bin/kotlinc /usr/local/bin \
 && ln -s /opt/kotlinc/bin/kotlin /usr/local/bin \
 && rm kotlin-compiler-$KOTLIN_VERSION.zip
#   install IO
RUN wget http://iobin.suspended-chord.info/linux/iobin-linux-x64-deb-current.zip \
 && unzip -d /io iobin-linux-x64-deb-current.zip \
 && dpkg -i /io/*.deb \
 && rm -r /io iobin-linux-x64-deb-current.zip
#   install Swift
ENV SWIFT_VERSION="2.2-SNAPSHOT-2015-12-01-b"
RUN mkdir -p /opt/swift \
 && wget https://swift.org/builds/ubuntu1404/swift-$SWIFT_VERSION/swift-$SWIFT_VERSION-ubuntu14.04.tar.gz \
 && tar zxvf swift-$SWIFT_VERSION-ubuntu14.04.tar.gz -C /opt/swift --strip-components=1 \
 && rm swift-$SWIFT_VERSION-ubuntu14.04.tar.gz
ENV PATH="/opt/swift/usr/bin:$PATH"
#   install Nim
ENV NIM_VERSION="0.14.2"
RUN mkdir /opt/nim \
 && cd /opt/nim \
 && wget http://nim-lang.org/download/nim-$NIM_VERSION.tar.xz \
 && tar xf nim-$NIM_VERSION.tar.xz \
 && rm nim-$NIM_VERSION.tar.xz \
 && cd nim-$NIM_VERSION \
 && ./build.sh \
 && cd .. ln -s /opt/nim/nim-$NIM_VERSION/bin/nim /usr/local/bin
#   configure Node package
RUN update-alternatives --install /usr/bin/node node /usr/bin/nodejs 10
#   config verbosity of Pascal compiler
RUN sed -i 's/^-l$//' /etc/fpc.cfg
#   cleanup
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
