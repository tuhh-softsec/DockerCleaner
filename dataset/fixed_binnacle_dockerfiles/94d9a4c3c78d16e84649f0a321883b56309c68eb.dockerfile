#   This Dockerfile specifies the recipe for creating an image for the tests
#   to run in.
#
#   We install as many test dependencies here as we can, because these setup
#   steps can be cached.  They do *not* run every time we run the build.
#   The Docker image is only rebuilt when the Dockerfile (ie. this file)
#   changes.
#   Base Dockerfile for gRPC dev images
FROM 32bit/debian:latest
#   Apt source for php
RUN echo "deb http://ppa.launchpad.net/ondrej/php/ubuntu trusty main" | tee /etc/apt/sources.list.d/various-php.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F4FCBB07
#   Install dependencies.  We start with the basic ones require to build protoc
#   and the C++ build
RUN apt-get clean \
 && apt-get update \
 && apt-get install --no-install-recommends autoconf=2.71-3 autotools-dev=20220109.1 build-essential=12.9ubuntu3 bzip2=1.0.8-5build1 ccache=4.7.4-1 curl=7.88.1-7ubuntu1 gcc=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 libc6=2.37-0ubuntu2 libc6-dbg=2.37-0ubuntu2 libc6-dev=2.37-0ubuntu2 libgtest-dev=1.12.1-0.2 libtool=2.4.7-5 make=4.3-4.1build1 parallel=20221122+ds-2 time=1.9-0.2 wget=1.21.3-1ubuntu1 unzip=6.0-27ubuntu1 python-setuptools python-pip python-dev cmake=3.25.1-1 php5.5 php5.5-dev php5.5-xml php5.6 php5.6-dev php5.6-xml php7.0 php7.0-dev php7.0-xml phpunit=9.6.3-1 valgrind=1:3.19.0-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 -y --force-yes \
 && apt-get clean
#  #################
#   PHP dependencies.
RUN php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');"
RUN php composer-setup.php
RUN mv composer.phar /usr/bin/composer
RUN php -r "unlink('composer-setup.php');"
RUN cd /tmp \
 && git clone https://github.com/google/protobuf.git \
 && cd protobuf/php \
 && git reset 46ae90dc5e145b12fffa7e053a908a9f3e066286 \
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
 && make install \
 && make clean \
 && cd ..
RUN cd php-5.5.38 \
 && ./configure --enable-bcmath --prefix=/usr/local/php-5.5-bc \
 && make \
 && make install \
 && make clean \
 && cd ..
#  #################
#   Python dependencies
#   These packages exist in apt-get, but their versions are too old, so we have
#   to get updates from pip.
RUN pip install pip==23.1 --upgrade
RUN pip install virtualenv==20.21.0 tox==4.4.12 yattag==1.15.1
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
