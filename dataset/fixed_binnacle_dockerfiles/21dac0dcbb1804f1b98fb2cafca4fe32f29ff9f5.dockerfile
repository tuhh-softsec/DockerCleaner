FROM ubuntu:16.04
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 make=4.1-6 software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 -y
#   Install C
RUN apt-get update \
 && apt-get install --no-install-recommends gcc-4.7=4.7.4-3ubuntu12 gcc-4.8=4.8.5-4ubuntu2 gcc-4.9=4.9.3-13ubuntu2 gcc-5=5.4.0-6ubuntu1~16.04.12 gcc=4:5.3.1-1ubuntu1 -y
#   Install Isolate
WORKDIR /tmp
RUN git clone https://github.com/ioi/isolate.git \
 && cd isolate \
 && echo "num_boxes = 2147483647" >> default.cf \
 && make install
ENV BOX_ROOT="/var/local/lib/isolate"
#   Install C++
RUN apt-get install --no-install-recommends g++-4.7=4.7.4-3ubuntu12 g++-4.8=4.8.5-4ubuntu2 g++-4.9=4.9.3-13ubuntu2 g++-5=5.4.0-6ubuntu1~16.04.12 g++=4:5.3.1-1ubuntu1 -y
#   Install Java
RUN echo | add-apt-repository ppa:webupd8team/java \
 && apt-get update \
 && echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections \
 && apt-get install --no-install-recommends oracle-java6-installer oracle-java7-installer oracle-java8-installer -y
#   Install Ruby
RUN apt-get install --no-install-recommends autoconf=2.69-9 bison=2:3.0.4.dfsg-1 build-essential=12.1ubuntu2 libssl-dev=1.0.2g-1ubuntu4.20 libyaml-dev=0.1.6-3 libreadline6-dev=6.3-8ubuntu2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libncurses5-dev=6.0+20160213-1ubuntu1 libffi-dev=3.2.1-4 libgdbm3=1.8.3-13.1 libgdbm-dev=1.8.3-13.1 -y
WORKDIR /tmp
RUN wget http://gd.tuwien.ac.at/languages/ruby/ruby-1.9-stable.tar.gz \
 && tar -xzf ruby-1.9-stable.tar.gz \
 && cd ruby-1.9.3-p448 \
 && ./configure --prefix /usr/lib/ruby/1.9.3 \
 && make \
 && make install
WORKDIR /tmp
RUN wget https://cache.ruby-lang.org/pub/ruby/2.2/ruby-2.2.5.tar.gz \
 && tar -xzf ruby-2.2.5.tar.gz \
 && cd ruby-2.2.5 \
 && ./configure --prefix /usr/lib/ruby/2.2.5 \
 && make \
 && make install
WORKDIR /tmp
RUN wget http://ftp.ruby-lang.org/pub/ruby/2.3/ruby-2.3.1.tar.gz \
 && tar -xzf ruby-2.3.1.tar.gz \
 && cd ruby-2.3.1 \
 && ./configure --prefix /usr/lib/ruby/2.3.1 \
 && make \
 && make install
ENV PATH="/usr/lib/ruby/2.3.1/bin:$PATH"
#   Install Python
RUN apt-get install --no-install-recommends python=2.7.12-1~16.04 python3=3.5.1-3 -y
#   Install Pascal
RUN apt-get install --no-install-recommends fp-compiler-3.0.0=3.0.0+dfsg-2 -y
#   ADD HERE MORE COMPILERS ...
RUN cd /tmp \
 && rm -rf *
WORKDIR /root
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
