FROM ubuntu:16.04
RUN apt-get update -qq \
 && apt-get install --no-install-recommends -qq -y bison g++ gcc gdb git libevent-dev liblz4-dev libmecab-dev libmsgpack-dev libssl-dev libstemmer-dev libzmq-dev libzstd-dev make mecab-naist-jdic pkg-config rapidjson-dev rsync ruby ruby-dev sudo tzdata zlib1g-dev
RUN apt-get update -qq \
 && apt-get install --no-install-recommends -qq -y software-properties-common \
 && add-apt-repository -y ppa:cutter-testing-framework/ppa \
 && apt-get update -qq \
 && apt-get install --no-install-recommends -qq -y cutter-testing-framework
RUN apt-get update -qq \
 && apt-get install --no-install-recommends -qq -y apt-transport-https curl lsb-release \
 && curl https://dist.apache.org/repos/dist/dev/arrow/KEYS | apt-key add - \
 && echo "deb [arch=amd64] https://dl.bintray.com/apache/arrow/$( lsb_release --id --short | tr 'A-Z' 'a-z' ;)/ $( lsb_release --codename --short ;) main" > /etc/apt/sources.list.d/apache-arrow.list \
 && apt-get update -qq \
 && apt-get install --no-install-recommends -qq -y libarrow-dev
RUN gem install bundler
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
