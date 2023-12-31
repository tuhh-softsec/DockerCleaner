#  This Dockerfile specifies the recipe for creating an image for the tests
#  to run in.
#
#  We install as many test dependencies here as we can, because these setup
#  steps can be cached.  They do *not* run every time we run the build.
#  The Docker image is only rebuilt when the Dockerfile (ie. this file)
#  changes.
#  Base Dockerfile for gRPC dev images
FROM debian:latest
#  Apt source for old Python versions.
RUN echo 'deb http://ppa.launchpad.net/fkrull/deadsnakes/ubuntu trusty main' > /etc/apt/sources.list.d/deadsnakes.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys DB82666C
#  Apt source for Oracle Java.
RUN echo 'deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main' > /etc/apt/sources.list.d/webupd8team-java-trusty.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886 \
 && echo "oracle-java7-installer shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
#  Apt source for Mono
RUN echo "deb http://download.mono-project.com/repo/debian wheezy main" | tee /etc/apt/sources.list.d/mono-xamarin.list \
 && echo "deb http://download.mono-project.com/repo/debian wheezy-libjpeg62-compat main" | tee -a /etc/apt/sources.list.d/mono-xamarin.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
#  Apt source for php
RUN echo "deb http://ppa.launchpad.net/ondrej/php/ubuntu trusty main" | tee /etc/apt/sources.list.d/various-php.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F4FCBB07
#  Install dotnet SDK based on https://www.microsoft.com/net/core#debian
#  (Ubuntu instructions need apt to support https)
RUN apt-get update \
 && apt-get install --no-install-recommends curl libunwind8 gettext -y --force-yes \
 && curl -sSL -o dotnet.tar.gz https://go.microsoft.com/fwlink/?LinkID=809130 \
 && mkdir -p /opt/dotnet \
 && tar zxf dotnet.tar.gz -C /opt/dotnet \
 && ln -s /opt/dotnet/dotnet /usr/local/bin
#  Install dependencies.  We start with the basic ones require to build protoc
#  and the C++ build
RUN apt-get clean \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf autotools-dev build-essential bzip2 ccache curl gcc git libc6 libc6-dbg libc6-dev libgtest-dev libtool make parallel time wget mono-devel referenceassemblies-pcl nunit maven openjdk-7-jdk oracle-java7-installer python-setuptools python-pip python-dev python2.6-dev python3.3-dev python3.4-dev ruby cmake php5.5 php5.5-dev php5.5-xml php5.6 php5.6-dev php5.6-xml php7.0 php7.0-dev php7.0-xml phpunit valgrind libxml2-dev -y --force-yes \
 && apt-get clean
# #################
#  C# dependencies
RUN wget www.nuget.org/NuGet.exe -O /usr/local/bin/nuget.exe
# #################
#  Python dependencies
#  These packages exist in apt-get, but their versions are too old, so we have
#  to get updates from pip.
RUN pip install pip --upgrade
RUN pip install virtualenv tox yattag
# #################
#  Ruby dependencies
#  Install rvm
RUN gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
RUN curl -sSL https://get.rvm.io | bash -s stable
#  Install Ruby 2.1, Ruby 2.2 and JRuby 1.7
RUN /bin/bash -l -c "rvm install ruby-2.1"
RUN /bin/bash -l -c "rvm install ruby-2.2"
RUN /bin/bash -l -c "rvm install jruby-1.7"
RUN /bin/bash -l -c "echo 'gem: --no-ri --no-rdoc' > ~/.gemrc"
RUN /bin/bash -l -c "echo 'export PATH=/usr/local/rvm/bin:$PATH' >> ~/.bashrc"
RUN /bin/bash -l -c "gem install bundler --no-ri --no-rdoc"
# #################
#  Java dependencies
#  This step requires compiling protoc. :(
ENV MAVEN_REPO="/var/maven_local_repository"
ENV MVN="mvn --batch-mode"
RUN cd /tmp \
 && git clone https://github.com/google/protobuf.git \
 && cd protobuf \
 && ./autogen.sh \
 && ./configure \
 && make -j4 \
 && cd java \
 && $MVN install dependency:go-offline -Dmaven.repo.local=$MAVEN_REPO \
 && cd ../javanano \
 && $MVN install dependency:go-offline -Dmaven.repo.local=$MAVEN_REPO
# #################
#  PHP dependencies.
RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php composer-setup.php
RUN mv composer.phar /usr/bin/composer
RUN php -r "unlink('composer-setup.php');"
RUN cd /tmp \
 && cd protobuf \
 && ln -sfn /usr/bin/php5.5 /usr/bin/php \
 && ln -sfn /usr/bin/php-config5.5 /usr/bin/php-config \
 && ln -sfn /usr/bin/phpize5.5 /usr/bin/phpize \
 && composer install \
 && mv vendor /usr/local/vendor-5.5 \
 && ln -sfn /usr/bin/php5.6 /usr/bin/php \
 && ln -sfn /usr/bin/php-config5.6 /usr/bin/php-config \
 && ln -sfn /usr/bin/phpize5.6 /usr/bin/phpize \
 && composer install \
 && mv vendor /usr/local/vendor-5.6 \
 && ln -sfn /usr/bin/php7.0 /usr/bin/php \
 && ln -sfn /usr/bin/php-config7.0 /usr/bin/php-config \
 && ln -sfn /usr/bin/phpize7.0 /usr/bin/phpize \
 && composer install \
 && mv vendor /usr/local/vendor-7.0
RUN wget http://am1.php.net/get/php-5.5.38.tar.bz2/from/this/mirror
RUN mv mirror php-5.5.38.tar.bz2
RUN tar -xvf php-5.5.38.tar.bz2
RUN cd php-5.5.38 \
 && ./configure --enable-maintainer-zts --prefix=/usr/local/php-5.5-zts \
 && make \
 && make install
# #################
#  Go dependencies.
RUN apt-get install --no-install-recommends golang -y
# #################
#  Javascript dependencies.
RUN apt-get install --no-install-recommends npm -y
#  On Debian/Ubuntu, nodejs binary is named 'nodejs' because the name 'node'
#  is taken by another legacy binary. We don't have that legacy binary and
#  npm expects the binary to be named 'node', so we just create a symbol
#  link here.
RUN ln -s `which nodejs ` /usr/bin/node
# #################
#  Prepare ccache
RUN ln -s /usr/bin/ccache /usr/local/bin/gcc
RUN ln -s /usr/bin/ccache /usr/local/bin/g++
RUN ln -s /usr/bin/ccache /usr/local/bin/cc
RUN ln -s /usr/bin/ccache /usr/local/bin/c++
RUN ln -s /usr/bin/ccache /usr/local/bin/clang
RUN ln -s /usr/bin/ccache /usr/local/bin/clang++
#  Define the default command.
CMD ["bash"]
