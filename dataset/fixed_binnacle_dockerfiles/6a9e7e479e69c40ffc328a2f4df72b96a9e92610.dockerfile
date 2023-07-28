#   This Dockerfile specifies the recipe for creating an image for the tests
#   to run in.
#
#   We install as many test dependencies here as we can, because these setup
#   steps can be cached.  They do *not* run every time we run the build.
#   The Docker image is only rebuilt when the Dockerfile (ie. this file)
#   changes.
#   Base Dockerfile for gRPC dev images
FROM debian:latest
#   Apt source for old Python versions.
RUN echo 'deb http://ppa.launchpad.net/fkrull/deadsnakes/ubuntu trusty main' > /etc/apt/sources.list.d/deadsnakes.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys DB82666C
#   Apt source for Oracle Java.
RUN echo 'deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main' > /etc/apt/sources.list.d/webupd8team-java-trusty.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886 \
 && echo "oracle-java7-installer shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
#   Apt source for Mono
RUN echo "deb http://download.mono-project.com/repo/debian wheezy main" | tee /etc/apt/sources.list.d/mono-xamarin.list \
 && echo "deb http://download.mono-project.com/repo/debian wheezy-libjpeg62-compat main" | tee -a /etc/apt/sources.list.d/mono-xamarin.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
#   Apt source for php
RUN echo "deb http://ppa.launchpad.net/ondrej/php/ubuntu trusty main" | tee /etc/apt/sources.list.d/various-php.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F4FCBB07
#   Install dotnet SDK based on https://www.microsoft.com/net/core#debian
#   (Ubuntu instructions need apt to support https)
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 libunwind8=1.6.2-3 gettext=0.21-11 -y --force-yes \
 && curl -sSL -o dotnet.tar.gz https://go.microsoft.com/fwlink/?LinkID=847105 \
 && mkdir -p /opt/dotnet \
 && tar zxf dotnet.tar.gz -C /opt/dotnet \
 && ln -s /opt/dotnet/dotnet /usr/local/bin
#   Install dependencies.  We start with the basic ones require to build protoc
#   and the C++ build
RUN apt-get clean \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf=2.71-3 autotools-dev=20220109.1 build-essential=12.9ubuntu3 bzip2=1.0.8-5build1 ccache=4.7.4-1 curl=7.88.1-7ubuntu1 gcc=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 libc6=2.37-0ubuntu2 libc6-dbg=2.37-0ubuntu2 libc6-dev=2.37-0ubuntu2 libgtest-dev=1.12.1-0.2 libtool=2.4.7-5 make=4.3-4.1build1 parallel=20221122+ds-2 time=1.9-0.2 wget=1.21.3-1ubuntu1 mono-devel=6.8.0.105+dfsg-3.3 referenceassemblies-pcl nunit=2.6.4+dfsg-1.1 maven=3.8.7-1 openjdk-7-jdk oracle-java7-installer python-setuptools python-pip python-dev python2.6-dev python3.3-dev python3.4-dev ruby=1:3.1 cmake=3.25.1-1 php5.6 php5.6-dev php5.6-xml php7.0 php7.0-dev php7.0-xml phpunit=9.6.3-1 valgrind=1:3.19.0-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 -y --force-yes \
 && apt-get clean
#  #################
#   C# dependencies
RUN wget www.nuget.org/NuGet.exe -O /usr/local/bin/nuget.exe
#  #################
#   Python dependencies
#   These packages exist in apt-get, but their versions are too old, so we have
#   to get updates from pip.
RUN pip install pip==23.1 --upgrade
RUN pip install virtualenv==20.21.0 tox==4.4.12 yattag==1.15.1
#  #################
#   Ruby dependencies
#   Install rvm
RUN gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
RUN curl -sSL https://get.rvm.io | bash -s stable
#   Install Ruby 2.1, Ruby 2.2 and JRuby 1.7
RUN /bin/bash -l -c "rvm install ruby-2.1"
RUN /bin/bash -l -c "rvm install ruby-2.2"
RUN /bin/bash -l -c "rvm install jruby-1.7"
RUN /bin/bash -l -c "echo 'gem: --no-ri --no-rdoc' > ~/.gemrc"
RUN /bin/bash -l -c "echo 'export PATH=/usr/local/rvm/bin:$PATH' >> ~/.bashrc"
RUN /bin/bash -l -c "gem install bundler --no-ri --no-rdoc"
#  #################
#   Java dependencies
#   This step requires compiling protoc. :(
ENV MAVEN_REPO="/var/maven_local_repository"
ENV MVN="mvn --batch-mode"
RUN cd /tmp \
 && git clone https://github.com/google/protobuf.git \
 && cd protobuf \
 && git reset --hard c2b3b3e04e7a023efe06f2107705b45428847800 \
 && ./autogen.sh \
 && ./configure \
 && make -j4 \
 && cd java \
 && $MVN install dependency:go-offline -Dmaven.repo.local=$MAVEN_REPO \
 && cd ../javanano \
 && $MVN install dependency:go-offline -Dmaven.repo.local=$MAVEN_REPO
#  #################
#   PHP dependencies.
RUN wget http://am1.php.net/get/php-5.5.38.tar.bz2/from/this/mirror
RUN mv mirror php-5.5.38.tar.bz2
RUN tar -xvf php-5.5.38.tar.bz2
RUN cd php-5.5.38 \
 && ./configure --enable-maintainer-zts --prefix=/usr/local/php-5.5-zts \
 && make \
 && make install \
 && cd ..
RUN cd php-5.5.38 \
 && make clean \
 && ./configure --prefix=/usr/local/php-5.5 \
 && make \
 && make install \
 && cd ..
RUN wget http://am1.php.net/get/php-5.6.30.tar.bz2/from/this/mirror
RUN mv mirror php-5.6.30.tar.bz2
RUN tar -xvf php-5.6.30.tar.bz2
RUN cd php-5.6.30 \
 && ./configure --enable-maintainer-zts --prefix=/usr/local/php-5.6-zts \
 && make \
 && make install \
 && cd ..
RUN cd php-5.6.30 \
 && make clean \
 && ./configure --prefix=/usr/local/php-5.6 \
 && make \
 && make install \
 && cd ..
RUN wget http://am1.php.net/get/php-7.0.18.tar.bz2/from/this/mirror
RUN mv mirror php-7.0.18.tar.bz2
RUN tar -xvf php-7.0.18.tar.bz2
RUN cd php-7.0.18 \
 && ./configure --enable-maintainer-zts --prefix=/usr/local/php-7.0-zts \
 && make \
 && make install \
 && cd ..
RUN cd php-7.0.18 \
 && make clean \
 && ./configure --prefix=/usr/local/php-7.0 \
 && make \
 && make install \
 && cd ..
RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php composer-setup.php
RUN mv composer.phar /usr/bin/composer
RUN php -r "unlink('composer-setup.php');"
RUN composer config -g -- disable-tls true
RUN composer config -g -- secure-http false
RUN cd /tmp \
 && rm -rf protobuf \
 && git clone https://github.com/google/protobuf.git \
 && cd protobuf \
 && git reset --hard 6b27c1f981a9a93918e4039f236ead27165a8e91 \
 && cd php \
 && ln -sfn /usr/local/php-5.5/bin/php /usr/bin/php \
 && ln -sfn /usr/local/php-5.5/bin/php-config /usr/bin/php-config \
 && ln -sfn /usr/local/php-5.5/bin/phpize /usr/bin/phpize \
 && composer install \
 && mv vendor /usr/local/vendor-5.5 \
 && ln -sfn /usr/local/php-5.6/bin/php /usr/bin/php \
 && ln -sfn /usr/local/php-5.6/bin/php-config /usr/bin/php-config \
 && ln -sfn /usr/local/php-5.6/bin/phpize /usr/bin/phpize \
 && composer install \
 && mv vendor /usr/local/vendor-5.6 \
 && ln -sfn /usr/local/php-7.0/bin/php /usr/bin/php \
 && ln -sfn /usr/local/php-7.0/bin/php-config /usr/bin/php-config \
 && ln -sfn /usr/local/php-7.0/bin/phpize /usr/bin/phpize \
 && composer install \
 && mv vendor /usr/local/vendor-7.0
#  #################
#   Go dependencies.
RUN apt-get install --no-install-recommends golang=2:1.20~0ubuntu1 -y
#  #################
#   Javascript dependencies.
RUN apt-get install --no-install-recommends npm=9.2.0~ds1-1 -y
#   On Debian/Ubuntu, nodejs binary is named 'nodejs' because the name 'node'
#   is taken by another legacy binary. We don't have that legacy binary and
#   npm expects the binary to be named 'node', so we just create a symbol
#   link here.
RUN ln -s `which nodejs ` /usr/bin/node
#  #################
#   Prepare ccache
RUN ln -s /usr/bin/ccache /usr/local/bin/gcc
RUN ln -s /usr/bin/ccache /usr/local/bin/g++
RUN ln -s /usr/bin/ccache /usr/local/bin/cc
RUN ln -s /usr/bin/ccache /usr/local/bin/c++
RUN ln -s /usr/bin/ccache /usr/local/bin/clang
RUN ln -s /usr/bin/ccache /usr/local/bin/clang++
#   Define the default command.
CMD ["bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
