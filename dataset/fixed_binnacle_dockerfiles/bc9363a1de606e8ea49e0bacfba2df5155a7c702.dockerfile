FROM ubuntu:latest
LABEL maintainer="amann@st.informatik.tu-darmstadt.de"
#   settings
ENV DEBIAN_FRONTEND="noninteractive"
ENV LANG="C.UTF-8" \
    GRADLE_VERSION="4.0.2" \
    GRADLE_HOME="/usr/local/gradle"
ENV JAVA_HOME="/usr/lib/jvm/java-8-oracle"
ENV PATH="$PATH:$JAVA_HOME/bin"
#   Setup container environment
RUN apt-get clean \
 && apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.99.35 locales=2.37-0ubuntu2 ca-certificates=20230311 unzip=6.0-27ubuntu1 -y \
 && update-ca-certificates \
 && locale-gen en_US en_US.UTF-8 \
 && dpkg-reconfigure locales \
 && echo "APT::Get::Assume-Yes \"true\";\nAPT::Get::force-yes \"true\";" >> /etc/apt/apt.conf.d/90forceyes \
 && add-apt-repository ppa:webupd8team/java \
 && apt-get update \
 && echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && apt-get install --no-install-recommends oracle-java8-installer -y \
 && update-alternatives --set java "$JAVA_HOME/jre/bin/java" \
 && update-alternatives --set javac "$JAVA_HOME/bin/javac" \
 && update-alternatives --set javaws "$JAVA_HOME/jre/bin/javaws" \
 && apt-get clean \
 && apt-get autoclean \
 && apt-get autoremove
#   Setup pipeline environment
RUN apt-get update \
 && apt-get install --no-install-recommends python3-pip=23.0.1+dfsg-1 python3=3.11.2-1 python3-dev=3.11.2-1 python3-psutil=5.9.4-1build1 -y \
 && ln -s python3 /usr/bin/python \
 && ln -s pip3 /usr/bin/pip \
 && pip3 install -r https://raw.githubusercontent.com/stg-tud/MUBench/master/mubench.pipeline/requirements.txt \
 && apt-get install --no-install-recommends ant=1.10.13-1 git=1:2.39.2-1ubuntu1 graphviz=2.42.2-7build3 maven=3.8.7-1 subversion=1.14.2-4build2 -y \
 && wget -q http://pyyaml.org/download/libyaml/yaml-0.1.7.tar.gz \
 && tar xvf yaml-0.1.7.tar.gz -C /usr/local \
 && rm -f yaml-0.1.7.tar.gz \
 && cd /usr/local/yaml-0.1.7 \
 && ./configure \
 && make \
 && make install \
 && apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 -y \
 && git config --global user.email "bob@builder.com" \
 && git config --global user.name "Bob the Builder" \
 && wget -q https://services.gradle.org/distributions/gradle-$GRADLE_VERSION-bin.zip \
 && unzip -q gradle-$GRADLE_VERSION-bin.zip -d /usr/local \
 && rm -f gradle-$GRADLE_VERSION-bin.zip \
 && ln -s /usr/local/gradle-$GRADLE_VERSION/bin/gradle /usr/bin/gradle \
 && apt-get clean \
 && apt-get autoclean \
 && apt-get autoremove
#   Setup reviewsite environment
RUN LC_ALL=C.UTF-8 add-apt-repository -y ppa:ondrej/php \
 && apt-get update \
 && apt-get install --no-install-recommends php7.0 php7.0-xml php7.0-mbstring php7.0-sqlite php7.0-zip php7.0-curl -y \
 && wget -q https://getcomposer.org/installer -O composer-setup.php \
 && php composer-setup.php --install-dir=/usr/local/bin --filename=composer \
 && rm composer-setup.php \
 && apt-get clean \
 && apt-get autoclean \
 && apt-get autoremove
#   Setup test environment
RUN wget -q https://phar.phpunit.de/phpunit-6.1.0.phar -O /usr/local/bin/phpunit \
 && chmod +x /usr/local/bin/phpunit \
 && pip3 install nose \
 && apt-get install --no-install-recommends chromium-browser=1:85.0.4183.83-0ubuntu3 chromium-chromedriver=1:85.0.4183.83-0ubuntu3 -y \
 && wget -q https://selenium-release.storage.googleapis.com/3.12/selenium-server-standalone-3.12.0.jar \
 && mv selenium-server-standalone-3.12.0.jar /usr/local/bin/selenium-server-standalone-3.12.0.jar \
 && ln /usr/lib/chromium-browser/chromedriver /usr/local/bin/chromedriver
WORKDIR /mubench
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
