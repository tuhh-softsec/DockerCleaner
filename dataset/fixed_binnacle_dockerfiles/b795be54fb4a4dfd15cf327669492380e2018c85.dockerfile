FROM ubuntu:16.04
RUN apt-get update -qq \
 && apt-get install --no-install-recommends bison=2:3.0.4.dfsg-1 g++=4:5.3.1-1ubuntu1 gcc=4:5.3.1-1ubuntu1 gdb=7.11.1-0ubuntu1~16.5 git=1:2.7.4-0ubuntu1.10 libevent-dev=2.0.21-stable-2ubuntu0.16.04.1 liblz4-dev=0.0~r131-2ubuntu2 libmecab-dev=0.996-1.2ubuntu1 libmsgpack-dev=0.5.7-3ubuntu1 libssl-dev=1.0.2g-1ubuntu4.20 libstemmer-dev=0+svn585-1 libzmq-dev=2.2.0+dfsg-7 libzstd-dev=0.5.1-1 make=4.1-6 mecab-naist-jdic=0.6.3.b-20111013-7 pkg-config=0.29.1-0ubuntu1 rapidjson-dev=0.12~git20141031-3 rsync=3.1.1-3ubuntu1.3 ruby=1:2.3.0+1 ruby-dev=1:2.3.0+1 sudo=1.8.16-0ubuntu1.10 tzdata=2021a-0ubuntu0.16.04 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -qq -y
RUN apt-get update -qq \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -qq -y \
 && add-apt-repository -y ppa:cutter-testing-framework/ppa \
 && apt-get update -qq \
 && apt-get install --no-install-recommends cutter-testing-framework -qq -y
RUN apt-get update -qq \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 curl=7.47.0-1ubuntu2.19 lsb-release=9.20160110ubuntu0.2 -qq -y \
 && curl https://dist.apache.org/repos/dist/dev/arrow/KEYS | apt-key add - \
 && echo "deb [arch=amd64] https://dl.bintray.com/apache/arrow/$( lsb_release --id --short | tr 'A-Z' 'a-z' ;)/ $( lsb_release --codename --short ;) main" > /etc/apt/sources.list.d/apache-arrow.list \
 && apt-get update -qq \
 && apt-get install --no-install-recommends libarrow-dev -qq -y
RUN gem install bundler --version 2.4.12
RUN useradd --user-group --create-home groonga
RUN echo "groonga ALL=(ALL:ALL) NOPASSWD:ALL" | EDITOR=tee visudo -f /etc/sudoers.d/groonga
COPY . /home/groonga/source
USER groonga
WORKDIR /home/groonga
RUN mkdir -p build
WORKDIR /home/groonga/build
RUN ../source/configure --prefix=/tmp/local --enable-debug --with-ruby --enable-mruby
RUN make -j$( nproc ;) > /dev/null
RUN mkdir -p /tmp/local/var/log/groonga/httpd/
RUN rsync -a --include "*.rb" --include "*/" --exclude "*" ../source/plugins/ plugins/
RUN mkdir -p test/command \
 && rsync -a --delete ../source/test/command/suite/ test/command/suite/
CMD BUILD_DIR=test/unit ../source/test/unit/run-test.sh \
 && BUILD_DIR=test/mruby ../source/test/mruby/run-test.rb \
 && BUILD_DIR=test/command_line ../source/test/command_line/run-test.rb \
 && BUILD_DIR=test/command ../source/test/command/run-test.sh test/command/suite --reporter mark --read-timeout 30 \
 && BUILD_DIR=test/command ../source/test/command/run-test.sh test/command/suite --reporter mark --read-timeout 30 --interface http \
 && BUILD_DIR=test/command ../source/test/command/run-test.sh test/command/suite --reporter mark --read-timeout 30 --testee groonga-httpd
# Please add your HEALTHCHECK here!!!
