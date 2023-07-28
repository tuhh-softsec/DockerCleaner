FROM ubuntu:18.04
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends bison=2:3.0.4.dfsg-1build1 g++=4:7.4.0-1ubuntu2.3 gcc=4:7.4.0-1ubuntu2.3 gdb=8.1.1-0ubuntu1 git=1:2.17.1-1ubuntu0.17 libevent-dev=2.1.8-stable-4build1 liblz4-dev=0.0~r131-2ubuntu3.1 libmecab-dev=0.996-5 libmsgpack-dev=2.1.5-1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libstemmer-dev=0+svn585-1build1 libzmq3-dev=4.2.5-1ubuntu0.2 libzstd-dev=1.3.3+dfsg-2ubuntu1.2 make=4.1-9.1ubuntu1 mecab-naist-jdic=0.6.3.b-20111013-8 pkg-config=0.29.1-0ubuntu2 rapidjson-dev=1.1.0+dfsg2-3 rsync=3.1.2-2.1ubuntu1.6 ruby=1:2.5.1 ruby-dev=1:2.5.1 sudo=1.8.21p2-3ubuntu1.5 tzdata=2022g-0ubuntu0.18.04 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -qq -y
#   RUN \
#     apt update -qq && \
#     apt install -qq -y software-properties-common && \
#     add-apt-repository -y ppa:cutter-testing-framework/ppa && \
#     apt update -qq && \
#     apt install -qq -y cutter-testing-framework
RUN apt-get update -qq \
 && apt-get install --no-install-recommends apt-transport-https=1.6.14 lsb-release=9.20170808ubuntu1 wget=1.19.4-1ubuntu2.2 -qq -y \
 && wget -O /usr/share/keyrings/apache-arrow-keyring.gpg https://dl.bintray.com/apache/arrow/$( lsb_release --id --short | tr 'A-Z' 'a-z' ;)/apache-arrow-keyring.gpg \
 && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/apache-arrow-keyring.gpg] https://dl.bintray.com/apache/arrow/$( lsb_release --id --short | tr 'A-Z' 'a-z' ;)/ $( lsb_release --codename --short ;) main" > /etc/apt/sources.list.d/apache-arrow.list \
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
CMD BUILD_DIR=test/mruby ../source/test/mruby/run-test.rb \
 && BUILD_DIR=test/command_line ../source/test/command_line/run-test.rb \
 && BUILD_DIR=test/command ../source/test/command/run-test.sh test/command/suite --reporter mark --read-timeout 30 \
 && BUILD_DIR=test/command ../source/test/command/run-test.sh test/command/suite --reporter mark --read-timeout 30 --interface http \
 && BUILD_DIR=test/command ../source/test/command/run-test.sh test/command/suite --reporter mark --read-timeout 30 --testee groonga-httpd
# Please add your HEALTHCHECK here!!!
