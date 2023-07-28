FROM appsvctest/php-build-5.6:latest AS php5.6-build
FROM appsvctest/php-build-7.0:latest AS php7.0-build
FROM appsvctest/php-build-7.2:latest AS php7.2-build
FROM appsvctest/python-3.6-build:latest AS python-3.6-build
FROM appsvctest/python-3.7-build:latest AS python-3.7-build
FROM buildpack-deps:jessie-curl AS main
MAINTAINER Nazim Lala <naziml@microsoft.com>
ENV DEBIAN_FRONTEND="noninteractive"
COPY mod_mono.tar /tmp
#   Install dependencies
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
 && echo "deb http://download.mono-project.com/repo/debian jessie/snapshots/5.0.0.100/. main" > /etc/apt/sources.list.d/mono-xamarin.list \
 && echo "deb http://deb.debian.org/debian/ jessie main" >> /etc/apt/sources.list \
 && echo "deb-src http://deb.debian.org/debian/ jessie main" >> /etc/apt/sources.list \
 && echo "deb http://security.debian.org/ jessie/updates main" >> /etc/apt/sources.list \
 && echo "deb-src http://security.debian.org/ jessie/updates main" >> /etc/apt/sources.list \
 && echo "deb http://archive.debian.org/debian jessie-backports main" >> /etc/apt/sources.list \
 && echo "deb-src http://archive.debian.org/debian jessie-backports main" >> /etc/apt/sources.list \
 && echo "Acquire::Check-Valid-Until \"false\";" > /etc/apt/apt.conf \
 && apt-get update \
 && apt-get install --no-install-recommends apt-utils=2.6.0 -y \
 && apt-get install --no-install-recommends unzip=6.0-27ubuntu1 -y \
 && apt-get install --no-install-recommends xz-utils=5.4.1-0.2 -y \
 && apt-get install --no-install-recommends mono-complete=6.8.0.105+dfsg-3.3 -y \
 && apt-get install --no-install-recommends mono-apache-server4=4.2-2.4 -y \
 && apt-get install --no-install-recommends libapache2-mod-mono=3.8-3 -y \
 && apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 -y \
 && apt-get install --no-install-recommends openssh-client=1:9.0p1-1ubuntu8 -y \
 && apt-get install --no-install-recommends vim=2:9.0.1000-4ubuntu2 tree=2.1.0-1 -y \
 && apt-get install --no-install-recommends tcptraceroute=1.5beta7+debian-4.1 -y \
 && wget -O /tmp/node-v4.4.7-linux-x64.tar.xz https://nodejs.org/dist/v4.4.7/node-v4.4.7-linux-x64.tar.xz \
 && wget -O /tmp/node-v4.5.0-linux-x64.tar.xz https://nodejs.org/dist/v4.5.0/node-v4.5.0-linux-x64.tar.xz \
 && wget -O /tmp/node-v6.2.2-linux-x64.tar.xz https://nodejs.org/dist/v6.2.2/node-v6.2.2-linux-x64.tar.xz \
 && wget -O /tmp/node-v6.6.0-linux-x64.tar.xz https://nodejs.org/dist/v6.6.0/node-v6.6.0-linux-x64.tar.xz \
 && wget -O /tmp/node-v6.9.3-linux-x64.tar.xz https://nodejs.org/dist/v6.9.3/node-v6.9.3-linux-x64.tar.xz \
 && wget -O /tmp/node-v6.10.3-linux-x64.tar.xz https://nodejs.org/dist/v6.10.3/node-v6.10.3-linux-x64.tar.xz \
 && wget -O /tmp/node-v6.11.0-linux-x64.tar.xz https://nodejs.org/dist/v6.11.0/node-v6.11.0-linux-x64.tar.xz \
 && wget -O /tmp/node-v8.0.0-linux-x64.tar.xz https://nodejs.org/dist/v8.0.0/node-v8.0.0-linux-x64.tar.xz \
 && wget -O /tmp/node-v8.1.0-linux-x64.tar.xz https://nodejs.org/dist/v8.1.0/node-v8.1.0-linux-x64.tar.xz \
 && wget -O /tmp/node-v8.2.1-linux-x64.tar.xz https://nodejs.org/dist/v8.2.1/node-v8.2.1-linux-x64.tar.xz \
 && wget -O /tmp/node-v8.8.1-linux-x64.tar.xz https://nodejs.org/dist/v8.8.1/node-v8.8.1-linux-x64.tar.xz \
 && wget -O /tmp/node-v8.9.4-linux-x64.tar.xz https://nodejs.org/dist/v8.9.4/node-v8.9.4-linux-x64.tar.xz \
 && wget -O /tmp/node-v8.11.2-linux-x64.tar.xz https://nodejs.org/dist/v8.11.2/node-v8.11.2-linux-x64.tar.xz \
 && wget -O /tmp/node-v9.4.0-linux-x64.tar.xz https://nodejs.org/dist/v9.4.0/node-v9.4.0-linux-x64.tar.xz \
 && wget -O /tmp/node-v10.1.0-linux-x64.tar.xz https://nodejs.org/dist/v10.1.0/node-v10.1.0-linux-x64.tar.xz \
 && wget -O /tmp/npm-2.15.8.zip https://github.com/npm/npm/archive/v2.15.8.zip \
 && wget -O /tmp/npm-2.15.9.zip https://github.com/npm/npm/archive/v2.15.9.zip \
 && wget -O /tmp/npm-3.9.5.zip https://github.com/npm/npm/archive/v3.9.5.zip \
 && wget -O /tmp/npm-3.10.3.zip https://github.com/npm/npm/archive/v3.10.3.zip \
 && wget -O /tmp/npm-3.10.10.zip https://github.com/npm/npm/archive/v3.10.10.zip \
 && wget -O /tmp/npm-5.0.3.zip https://github.com/npm/npm/archive/v5.0.3.zip \
 && wget -O /tmp/npm-5.4.2.zip https://github.com/npm/npm/archive/v5.4.2.zip \
 && wget -O /tmp/npm-5.6.0.zip https://github.com/npm/npm/archive/v5.6.0.zip \
 && wget -O /tmp/npm-6.0.1.zip https://github.com/npm/npm/archive/v6.0.1.zip \
 && ln -s ../mods-available/xml2enc.load /etc/apache2/mods-enabled \
 && a2enmod proxy \
 && a2enmod proxy_http \
 && a2enmod proxy_wstunnel \
 && a2enmod rewrite \
 && a2enmod proxy_ajp \
 && a2enmod deflate \
 && a2enmod headers \
 && a2enmod proxy_balancer \
 && a2enmod proxy_connect \
 && a2enmod proxy_html \
 && tar -xf /tmp/mod_mono.tar \
 && cp mod_mono.so.0.0.0 /usr/lib/apache2/modules/mod_mono.so.0.0.0
#   Install npm and node
RUN mkdir -p /opt/npm/2.15.8/node_modules \
 && unzip -q /tmp/npm-2.15.8.zip -d /opt/npm/2.15.8/node_modules \
 && mv /opt/npm/2.15.8/node_modules/npm-2.15.8 /opt/npm/2.15.8/node_modules/npm \
 && ln -s /opt/npm/2.15.8/node_modules/npm/bin/npm /opt/npm/2.15.8/npm \
 && chmod -R 755 /opt/npm/2.15.8/node_modules/npm/bin \
 && mkdir -p /opt/npm/2.15.9/node_modules \
 && unzip -q /tmp/npm-2.15.9.zip -d /opt/npm/2.15.9/node_modules \
 && mv /opt/npm/2.15.9/node_modules/npm-2.15.9 /opt/npm/2.15.9/node_modules/npm \
 && ln -s /opt/npm/2.15.9/node_modules/npm/bin/npm /opt/npm/2.15.9/npm \
 && chmod -R 755 /opt/npm/2.15.9/node_modules/npm/bin \
 && mkdir -p /opt/npm/3.9.5/node_modules \
 && unzip -q /tmp/npm-3.9.5.zip -d /opt/npm/3.9.5/node_modules \
 && mv /opt/npm/3.9.5/node_modules/npm-3.9.5 /opt/npm/3.9.5/node_modules/npm \
 && ln -s /opt/npm/3.9.5/node_modules/npm/bin/npm /opt/npm/3.9.5/npm \
 && chmod -R 755 /opt/npm/3.9.5/node_modules/npm/bin \
 && mkdir -p /opt/npm/3.10.3/node_modules \
 && unzip -q /tmp/npm-3.10.3.zip -d /opt/npm/3.10.3/node_modules \
 && mv /opt/npm/3.10.3/node_modules/npm-3.10.3 /opt/npm/3.10.3/node_modules/npm \
 && ln -s /opt/npm/3.10.3/node_modules/npm/bin/npm /opt/npm/3.10.3/npm \
 && chmod -R 755 /opt/npm/3.10.3/node_modules/npm/bin \
 && mkdir -p /opt/npm/3.10.10/node_modules \
 && unzip -q /tmp/npm-3.10.10.zip -d /opt/npm/3.10.10/node_modules \
 && mv /opt/npm/3.10.10/node_modules/npm-3.10.10 /opt/npm/3.10.10/node_modules/npm \
 && ln -s /opt/npm/3.10.10/node_modules/npm/bin/npm /opt/npm/3.10.10/npm \
 && chmod -R 755 /opt/npm/3.10.10/node_modules/npm/bin \
 && mkdir -p /opt/npm/5.0.3/node_modules \
 && unzip -q /tmp/npm-5.0.3.zip -d /opt/npm/5.0.3/node_modules \
 && mv /opt/npm/5.0.3/node_modules/npm-5.0.3 /opt/npm/5.0.3/node_modules/npm \
 && ln -s /opt/npm/5.0.3/node_modules/npm/bin/npm /opt/npm/5.0.3/npm \
 && chmod -R 755 /opt/npm/5.0.3/node_modules/npm/bin \
 && mkdir -p /opt/npm/5.4.2/node_modules \
 && unzip -q /tmp/npm-5.4.2.zip -d /opt/npm/5.4.2/node_modules \
 && mv /opt/npm/5.4.2/node_modules/npm-5.4.2 /opt/npm/5.4.2/node_modules/npm \
 && ln -s /opt/npm/5.4.2/node_modules/npm/bin/npm /opt/npm/5.4.2/npm \
 && chmod -R 755 /opt/npm/5.4.2/node_modules/npm/bin \
 && mkdir -p /opt/npm/5.6.0/node_modules \
 && unzip -q /tmp/npm-5.6.0.zip -d /opt/npm/5.6.0/node_modules \
 && mv /opt/npm/5.6.0/node_modules/npm-5.6.0 /opt/npm/5.6.0/node_modules/npm \
 && ln -s /opt/npm/5.6.0/node_modules/npm/bin/npm /opt/npm/5.6.0/npm \
 && chmod -R 755 /opt/npm/5.6.0/node_modules/npm/bin \
 && mkdir -p /opt/npm/6.0.1/node_modules \
 && unzip -q /tmp/npm-6.0.1.zip -d /opt/npm/6.0.1/node_modules \
 && mv /opt/npm/6.0.1/node_modules/npm-6.0.1 /opt/npm/6.0.1/node_modules/npm \
 && ln -s /opt/npm/6.0.1/node_modules/npm/bin/npm /opt/npm/6.0.1/npm \
 && chmod -R 755 /opt/npm/6.0.1/node_modules/npm/bin \
 && chown -R root:root /opt/npm \
 && mkdir -p /opt/nodejs \
 && tar xfJ /tmp/node-v4.4.7-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v4.4.7-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v4.4.7-linux-x64 /opt/nodejs/4.4.7 \
 && echo "2.15.8" > /opt/nodejs/4.4.7/npm.txt \
 && mkdir -p /opt/nodejs \
 && tar xfJ /tmp/node-v4.5.0-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v4.5.0-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v4.5.0-linux-x64 /opt/nodejs/4.5.0 \
 && echo "2.15.9" > /opt/nodejs/4.5.0/npm.txt \
 && tar xfJ /tmp/node-v6.2.2-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v6.2.2-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v6.2.2-linux-x64 /opt/nodejs/6.2.2 \
 && echo "3.9.5" > /opt/nodejs/6.2.2/npm.txt \
 && tar xfJ /tmp/node-v6.6.0-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v6.6.0-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v6.6.0-linux-x64 /opt/nodejs/6.6.0 \
 && echo "3.10.3" > /opt/nodejs/6.6.0/npm.txt \
 && tar xfJ /tmp/node-v6.9.3-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v6.9.3-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v6.9.3-linux-x64 /opt/nodejs/6.9.3 \
 && echo "3.10.10" > /opt/nodejs/6.9.3/npm.txt \
 && tar xfJ /tmp/node-v6.10.3-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v6.10.3-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v6.10.3-linux-x64 /opt/nodejs/6.10.3 \
 && echo "3.10.10" > /opt/nodejs/6.10.3/npm.txt \
 && tar xfJ /tmp/node-v6.11.0-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v6.11.0-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v6.11.0-linux-x64 /opt/nodejs/6.11.0 \
 && echo "3.10.10" > /opt/nodejs/6.11.0/npm.txt \
 && tar xfJ /tmp/node-v8.0.0-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v8.0.0-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v8.0.0-linux-x64 /opt/nodejs/8.0.0 \
 && echo "5.0.3" > /opt/nodejs/8.0.0/npm.txt \
 && tar xfJ /tmp/node-v8.1.0-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v8.1.0-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v8.1.0-linux-x64 /opt/nodejs/8.1.0 \
 && echo "5.0.3" > /opt/nodejs/8.1.0/npm.txt \
 && tar xfJ /tmp/node-v8.2.1-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v8.2.1-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v8.2.1-linux-x64 /opt/nodejs/8.2.1 \
 && echo "5.4.2" > /opt/nodejs/8.2.1/npm.txt \
 && tar xfJ /tmp/node-v8.8.1-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v8.8.1-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v8.8.1-linux-x64 /opt/nodejs/8.8.1 \
 && echo "5.4.2" > /opt/nodejs/8.8.1/npm.txt \
 && tar xfJ /tmp/node-v8.9.4-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v8.9.4-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v8.9.4-linux-x64 /opt/nodejs/8.9.4 \
 && echo "5.6.0" > /opt/nodejs/8.9.4/npm.txt \
 && tar xfJ /tmp/node-v8.11.2-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v8.11.2-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v8.11.2-linux-x64 /opt/nodejs/8.11.2 \
 && echo "5.6.0" > /opt/nodejs/8.11.2/npm.txt \
 && tar xfJ /tmp/node-v9.4.0-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v9.4.0-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v9.4.0-linux-x64 /opt/nodejs/9.4.0 \
 && echo "5.6.0" > /opt/nodejs/9.4.0/npm.txt \
 && tar xfJ /tmp/node-v10.1.0-linux-x64.tar.xz -C /opt/nodejs \
 && rm -f node-v10.1.0-linux-x64.tar.xz \
 && mv /opt/nodejs/node-v10.1.0-linux-x64 /opt/nodejs/10.1.0 \
 && echo "6.0.1" > /opt/nodejs/10.1.0/npm.txt \
 && chown -R root:root /opt/nodejs \
 && ln -s /opt/nodejs/6.11.0/bin/node /opt/nodejs/node \
 && ln -s /opt/nodejs/6.11.0/npm.txt /opt/nodejs/npm.txt \
 && rm -f /usr/bin/node \
 && ln -s /opt/nodejs/6.11.0/bin/node /usr/bin/node \
 && rm -f /opt/nodejs/6.11.0/bin/npm \
 && ln -s /opt/npm/3.10.10/node_modules/npm/bin/npm /opt/nodejs/npm \
 && ln -s /opt/npm/3.10.10/node_modules /opt/nodejs/node_modules \
 && rm -rf /usr/bin/npm \
 && ln -s /opt/npm/3.10.10/node_modules/npm/bin/npm /usr/bin/npm \
 && ln -s /opt/npm/3.10.10/node_modules/npm/bin/npm-cli.js /usr/bin/npm-cli.js \
 && ln -s /opt/npm/3.10.10/node_modules /usr/bin/node_modules
ENV PATH="$PATH:/opt/nodejs/6.11.0/bin"
#   Upgrade mono
RUN echo "deb http://download.mono-project.com/repo/debian beta/. main" | tee /etc/apt/sources.list.d/mono-xamarin-c7-ms.list \
 && apt-get update \
 && apt-get install --no-install-recommends libmono-system-net-http-webrequest4.0-cil=6.8.0.105+dfsg-3.3 -y
#   Ruby installations
#   Dependencies for various ruby and rubygem installations
RUN apt-get install --no-install-recommends libreadline-dev=8.2-1.3 bzip2=1.0.8-5build1 build-essential=12.9ubuntu3 libssl-dev=3.0.8-1ubuntu1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libpq-dev=15.2-1 libsqlite3-dev=3.40.1-1 curl=7.88.1-7ubuntu1 patch=2.7.6-7build2 gawk=1:5.2.1-2 g++=4:12.2.0-3ubuntu1 gcc=4:12.2.0-3ubuntu1 make=4.3-4.1build1 libc6-dev=2.37-0ubuntu2 patch=2.7.6-7build2 libreadline6-dev libyaml-dev=0.2.5-1 sqlite3=3.40.1-1 autoconf=2.71-3 libgdbm-dev=1.23-3 libncurses5-dev=6.4-2 automake=1:1.16.5-1.3 libtool=2.4.7-5 bison=2:3.8.2+dfsg-1build1 pkg-config=1.8.1-1ubuntu2 libffi-dev=3.4.4-1 bison=2:3.8.2+dfsg-1build1 libxslt-dev libxml2-dev=2.9.14+dfsg-1.1build2 libmysqlclient-dev=8.0.32-0ubuntu4 -y
#   rbenv
RUN git clone https://github.com/rbenv/rbenv.git /usr/local/.rbenv
RUN chmod -R 777 /usr/local/.rbenv
ENV RBENV_ROOT="/usr/local/.rbenv"
ENV PATH="$RBENV_ROOT/bin:/usr/local:$PATH"
RUN git clone https://github.com/rbenv/ruby-build.git /usr/local/.rbenv/plugins/ruby-build
RUN chmod -R 777 /usr/local/.rbenv/plugins/ruby-build
RUN /usr/local/.rbenv/plugins/ruby-build/install.sh
#   Install ruby 2.3.3 (default), 2.3.8, 2.4.5
ENV RUBY_CONFIGURE_OPTS="--disable-install-doc"
ENV RUBY_CFLAGS="-O3"
RUN eval "$( rbenv init - ;)" \
 && export WEBSITES_DEFAULT_RUBY_VERSION="2.3.3" \
 && rbenv install $WEBSITES_DEFAULT_RUBY_VERSION \
 && rbenv install "2.3.8" \
 && rbenv install "2.4.5" \
 && rbenv install "2.5.5" \
 && rbenv install "2.6.2" \
 && rbenv rehash \
 && rbenv global $WEBSITES_DEFAULT_RUBY_VERSION \
 && ls /usr/local -a \
 && rbenv local $WEBSITES_DEFAULT_RUBY_VERSION \
 && gem install bundler --version 2.4.12 \
 && rbenv local 2.3.8 \
 && gem install bundler --version 2.4.12 \
 && rbenv local 2.4.5 \
 && gem install bundler --version 2.4.12 \
 && rbenv local 2.5.5 \
 && gem install bundler --version 2.4.12 \
 && rbenv local 2.6.2 \
 && gem install bundler --version 2.4.12 \
 && rbenv local $WEBSITES_DEFAULT_RUBY_VERSION \
 && chmod -R 777 /usr/local/.rbenv/versions \
 && chmod -R 777 /usr/local/.rbenv/version
RUN eval "$( rbenv init - ;)" \
 && rbenv global $WEBSITES_DEFAULT_RUBY_VERSION \
 && bundle config --global build.nokogiri -- --use-system-libraries
#   Because Nokogiri tries to build libraries on its own otherwise
ENV NOKOGIRI_USE_SYSTEM_LIBRARIES="true"
#   SQL Server gem support
RUN apt-get install --no-install-recommends unixodbc-dev=2.3.11-2 freetds-dev=1.3.17+ds-2 freetds-bin=1.3.17+ds-2 -y
#   Install .NET Core
RUN apt-get update \
 && apt-get install --no-install-recommends libc6=2.37-0ubuntu2 libcurl3 libgcc1 libgssapi-krb5-2=1.20.1-1build1 libicu52 liblttng-ust0 libssl1.0.0 libstdc++6=13-20230320-1ubuntu1 libunwind8=1.6.2-3 libuuid1=2.38.1-4ubuntu1 zlib1g=1:1.2.13.dfsg-1ubuntu4 -y \
 && rm -rf /var/lib/apt/lists/*
#   Install .NET Core SDK
ENV DOTNET_SDK_VERSION="2.1.300"
ENV DOTNET_SDK_DOWNLOAD_URL="https://dotnetcli.blob.core.windows.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-sdk-$DOTNET_SDK_VERSION-linux-x64.tar.gz"
ENV DOTNET_SDK_DOWNLOAD_SHA="80a6bfb1db5862804e90f819c1adeebe3d624eae0d6147e5d6694333f0458afd7d34ce73623964752971495a310ff7fcc266030ce5aef82d5de7293d94d13770"
RUN curl -SL $DOTNET_SDK_DOWNLOAD_URL --output dotnet.tar.gz \
 && echo "$DOTNET_SDK_DOWNLOAD_SHA dotnet.tar.gz" | sha512sum -c - \
 && mkdir -p /usr/share/dotnet \
 && tar -zxf dotnet.tar.gz -C /usr/share/dotnet \
 && rm dotnet.tar.gz \
 && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet
#   Moving the package cache to the container image will require user packages to be repopulated
#   for each new build done in a new container instance, but it's *much* faster than
#   using a location on the user's site volume and does not require them to use their storage, and
#   lets us warm the caches with standard packages
ENV NUGET_PACKAGES="/var/nuget"
#   Trigger the population of the local package cache and NuGetFallbackFolder
#   We install the 1.x web templates just so we can create and restore 1.0 and 1.1 MVC apps here
#   See https://github.com/aspnet/Tooling/blob/master/missing-template.md
#   Note that after populating the cache we need to chmod 777 every directory underneath
#   so the Kudu user can access them; this can be removed if we run Kudu as root later.
ENV NUGET_XMLDOC_MODE="skip"
ENV DOTNET_SKIP_FIRST_TIME_EXPERIENCE="1"
RUN mkdir /var/nuget \
 && chmod a+rw /var/nuget \
 && dotnet new -i "Microsoft.DotNet.Web.ProjectTemplates.1.x::1.0.0-*" \
 && mkdir warmup \
 && cd warmup \
 && dotnet new \
 && cd .. \
 && rm -rf warmup \
 && mkdir warmup \
 && cd warmup \
 && dotnet new mvc -f netcoreapp1.1 \
 && dotnet restore \
 && cd .. \
 && rm -rf warmup \
 && mkdir warmup \
 && cd warmup \
 && dotnet new mvc -f netcoreapp1.0 \
 && dotnet restore \
 && cd .. \
 && rm -rf warmup \
 && mkdir warmup \
 && cd warmup \
 && dotnet new mvc \
 && dotnet restore \
 && cd .. \
 && rm -rf warmup \
 && rm -rf /tmp/NuGetScratch \
 && find /var/nuget -type d -exec chmod 777 {}
#   install apache2
RUN apt-get install --no-install-recommends apache2=2.4.55-1ubuntu2 -y
RUN service apache2 restart
#   Section PHP
RUN apt-get update ; apt-get install --no-install-recommends libcurl4-openssl-dev=7.88.1-7ubuntu1 libssl-dev=3.0.8-1ubuntu1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libpng-dev=1.6.39-2 libpq-dev=15.2-1 libc-client-dev libsqlite3-dev=3.40.1-1 libgmp-dev=2:6.2.1+dfsg1-1.1ubuntu1 libmcrypt-dev=2.5.8-7 libldap2-dev=2.6.3+dfsg-1~exp1ubuntu2 libtidy-dev=2:5.6.0-11build2 libkrb5-dev=1.20.1-1build1 libicu-dev=72.1-3ubuntu1 libedit-dev=3.1-20221030-2 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt-dev unixodbc-dev=2.3.11-2 libmagickwand-dev=8:6.9.11.60+dfsg-1.6 dpkg-dev=1.21.21ubuntu1 file=1:5.44-3 g++=4:12.2.0-3ubuntu1 gcc=4:12.2.0-3ubuntu1 libc-dev libsodium-dev=1.0.18-1build2 libsodium18 -y \
 && ln -s /usr/lib/x86_64-linux-gnu/libldap.so /usr/lib/libldap.so
RUN mkdir -p /usr/local/php/5.6
COPY --from=php5.6-build /usr/local/php/5.6 /usr/local/php/5.6
RUN mkdir -p /usr/local/php/7.0
COPY --from=php7.0-build /usr/local/php/7.0 /usr/local/php/7.0
RUN mkdir -p /usr/local/php/7.2
COPY --from=php7.2-build /usr/local/php/7.2 /usr/local/php/7.2
ENV PATH="/usr/local/php/7.2/bin:$PATH"
ENV PHP_5_6_PATH="/usr/local/php/5.6/bin"
ENV PHP_7_0_PATH="/usr/local/php/7.0/bin"
ENV PHP_7_2_PATH="/usr/local/php/7.2/bin"
#   PHP Composer
RUN php -r "copy('https://getcomposer.org/installer', '/tmp/composer-setup.php');" \
 && php /tmp/composer-setup.php --install-dir=/usr/local/bin --filename=composer --quiet \
 && rm -f /tmp/composer-setup.php
ENV PHP_VERSION="7.2"
ENV COMPOSER_VENDOR_DIR="/home/site/wwwroot/vendor"
#   Golang and musl for cross compiling for alpine.
ENV GOLANG_VERSION="1.10"
ENV GOLANG_DOWNLOAD_URL="https://golang.org/dl/go$GOLANG_VERSION.linux-amd64.tar.gz"
ENV GOLANG_DOWNLOAD_SHA="b5a64335f1490277b585832d1f6c7f8c6c11206cba5cd3f771dcb87b98ad1a33"
RUN curl -SL $GOLANG_DOWNLOAD_URL --output golang.tar.gz \
 && echo "$GOLANG_DOWNLOAD_SHA golang.tar.gz" | sha256sum -c - \
 && tar -zxf golang.tar.gz -C /usr/local \
 && rm golang.tar.gz \
 && apt-get install --no-install-recommends musl-tools=1.2.3-1 -y
ENV PATH="/usr/local/go/bin:$PATH"
#   Download Dep executable
ENV DEP_VERSION="0.4.1"
ENV DEP_DOWNLOAD_URL="https://github.com/golang/dep/releases/download/v$DEP_VERSION/dep-linux-amd64"
ENV DEP_DOWNLOAD_SHA="31144e465e52ffbc0035248a10ddea61a09bf28b00784fd3fdd9882c8cbb2315"
RUN curl -fL -o /usr/local/go/bin/dep $DEP_DOWNLOAD_URL \
 && echo "$DEP_DOWNLOAD_SHA /usr/local/go/bin/dep" | sha256sum -c - \
 && chmod +x /usr/local/go/bin/dep
#   Section PYTHON
RUN set -ex \
 && savedAptMark="$( apt-mark showmanual ;)" \
 && apt-get update \
 && apt-get install --no-install-recommends dpkg-dev=1.21.21ubuntu1 gcc=4:12.2.0-3ubuntu1 libbz2-dev=1.0.8-5build1 libc6-dev=2.37-0ubuntu2 libexpat1-dev=2.5.0-1 libffi-dev=3.4.4-1 libgdbm-dev=1.23-3 liblzma-dev=5.4.1-0.2 libncursesw5-dev=6.4-2 libreadline-dev=8.2-1.3 libsqlite3-dev=3.40.1-1 libssl-dev=3.0.8-1ubuntu1 apt-transport-https=2.6.0 lsb-release=12.0-1ubuntu1 ca-certificates=20230311 make=4.3-4.1build1 tk-dev=8.6.13 tcl-dev=8.6.13 uuid-dev=2.38.1-4ubuntu1 wget=1.21.3-1ubuntu1 xz-utils=5.4.1-0.2 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 $( command -v gpg > /dev/null || echo 'gnupg dirmngr' ;) -y
COPY --from=python-3.6-build /usr/local/python/3.6.6 /usr/local/python/3.6.6
COPY --from=python-3.7-build /usr/local/openssl /usr/local/openssl
COPY --from=python-3.7-build /usr/local/python/3.7.0 /usr/local/python/3.7.0
ENV PATH="/usr/local/python/3.7.0/bin:$PATH"
ENV PYTHON_3_7_PATH="/usr/local/python/3.7.0/bin"
ENV PYTHON_3_6_PATH="/usr/local/python/3.6.6/bin"
#   Install Kudu
COPY kudu.conf /tmp
COPY apache2.conf /tmp
COPY Kudu.82.10506.3895.zip /tmp/Kudu.zip
ENV KUDU_VERSION="82.10506.3895"
RUN npm install kudusync@0.3.0 -g \
 && cp /tmp/kudu.conf /etc/apache2/sites-available/kudu.conf \
 && cp /tmp/apache2.conf /etc/apache2/apache2.conf \
 && unzip -q -o /tmp/Kudu.zip \
 && mkdir /opt/Kudu \
 && cp -rf ./$KUDU_VERSION/* /opt/Kudu \
 && rm -f /tmp/Kudu.zip \
 && rm -f /tmp/apache2.conf \
 && rm -f /tmp/kudu.conf \
 && rm -rf ./$KUDU_VERSION \
 && cat /opt/Kudu/Web.config | sed 's| <location path="." inheritInChildApplications="false">| <location path="~/../../../opt/Kudu" inheritInChildApplications="false">|' > /opt/Kudu/Web.config2 \
 && mv /opt/Kudu/Web.config2 /opt/Kudu/Web.config \
 && chmod 755 /opt/Kudu/bin/kudu.exe \
 && chmod 755 /opt/Kudu/bin/node_modules/.bin/kuduscript \
 && chmod 755 /opt/Kudu/bin/Scripts/starter.sh \
 && mkdir -p /opt/Kudu/local \
 && chmod 755 /opt/Kudu/local
#   Install pm2 and pm2-logrotate
RUN npm install pm2@latest -g \
 && pm2 install pm2-logrotate \
 && pm2 set pm2-logrotate:retain 7
#   Install LogAnalyzer
COPY LogAnalyzer.zip /tmp
RUN mkdir /opt/LogAnalyzer \
 && unzip /tmp/LogAnalyzer.zip -d /opt/LogAnalyzer \
 && rm -f /tmp/LogAnalyzer.zip
COPY startup.sh /
RUN chmod 755 /startup.sh \
 && mkdir -p /home/LogFiles
#   Remove extraneous config mono config load that starts up
#   an unnecessary mono process
RUN sed -i '$d' /etc/apache2/mods-enabled/mod_mono.conf
#   Install TunnelExtension
COPY TunnelExtension.zip /tmp
RUN mkdir /opt/tunnelext \
 && unzip /tmp/TunnelExtension.zip -d /opt/tunnelext \
 && chmod 777 /opt/tunnelext/tunnelwatcher.sh
#   Install webssh
COPY webssh.zip /tmp
RUN mkdir /opt/webssh \
 && unzip /tmp/webssh.zip -d /opt/webssh
#   Replace ssh with wrapper script for CIFS mount permissions workaround
COPY ssh /tmp
RUN mv /usr/bin/ssh /usr/bin/ssh.original \
 && mv /tmp/ssh /usr/bin/ssh \
 && chown root:root /usr/bin/ssh \
 && chmod 755 /usr/bin/ssh
COPY hostingstart.html /home/site/wwwroot/hostingstart.html
COPY KuduStaticFiles/503.html /opt/Kudu/503.html
COPY KuduStaticFiles/_Layout.cshtml /opt/Kudu/_Layout.cshtml
COPY KuduStaticFiles/AppService.png /opt/Kudu/Content/Images/AppService.png
COPY KuduStaticFiles/linux_favicon.ico /opt/Kudu/linux_favicon.ico
COPY deploy.bash.python.template /opt/Kudu/bin/node_modules/kuduscript/lib/templates/deploy.bash.python.template
COPY deploy.bash.ruby.template /opt/Kudu/bin/node_modules/kuduscript/lib/templates/deploy.bash.ruby.template
RUN chmod -R 777 /home \
 && rm -rf /tmp/*
EXPOSE 8181/tcp
ENV PORT="8181"
ENTRYPOINT ["/startup.sh"]
CMD ["1002", "kudu_group", "1001", "kudu_user", "localsite"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
