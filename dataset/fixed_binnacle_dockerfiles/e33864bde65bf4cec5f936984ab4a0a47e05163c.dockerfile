FROM ubuntu:16.04
#   Release parameters
ENV GOOGLEAPIS_HASH="ab437f2bb2100360f8d119530b0a020228baa4cc"
ENV GAPIC_GENERATOR_HASH="4c2bc4396eed69826456ab3bc2fe47c71c235bb5"
#   Define version number below. The ARTMAN_VERSION line is parsed by
#   .circleci/config.yml and setup.py, please keep the format.
ENV ARTMAN_VERSION="0.29.0"
ENV DEBIAN_FRONTEND="noninteractive"
#   Set the locale
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="C"
#   OpenJDK repository
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y \
 && rm -rf /var/lib/apt/lists/* \
 && add-apt-repository ppa:openjdk-r/ppa
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 vim=2:7.4.1689-3ubuntu1.5 openssh-client=1:7.2p2-4ubuntu2.10 unzip=6.0-20ubuntu1.1 php=1:7.0+35ubuntu6.1 python3-pip=8.1.1-2ubuntu0.6 openjdk-8-jdk-headless=8u292-b10-0ubuntu1~16.04.1 nodejs=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 ruby=1:2.3.0+1 ruby-dev=1:2.3.0+1 autoconf=2.69-9 autogen=1:5.18.7-3 libtool=2.4.6-0.1 autotools-dev=20150820.1 automake=1:1.15-4ubuntu1 make=4.1-6 g++=4:5.3.1-1ubuntu1 pandoc=1.16.0.2~dfsg-1 libc6=2.23-0ubuntu11.3 libcurl3=7.47.0-1ubuntu2.19 libgcc1=1:6.0.1-0ubuntu1 libgssapi-krb5-2=1.13.2+dfsg-5ubuntu2.2 liblttng-ust0=2.7.1-1 libssl1.0.0=1.0.2g-1ubuntu4.20 libstdc++6=5.4.0-6ubuntu1~16.04.12 libunwind8=1.1-4.1 libuuid1=2.27.1-6ubuntu3.10 zlib1g=1:1.2.8.dfsg-2ubuntu4.3 -y \
 && rm -rf /var/lib/apt/lists/*
#   Install all required protoc versions, and install protobuf Python package.
COPY install_protoc.sh /
RUN bash install_protoc.sh
#   Install grpc_csharp_plugin
RUN curl -L https://www.nuget.org/api/v2/package/Grpc.Tools/1.17.1 -o temp.zip \
 && unzip -p temp.zip tools/linux_x64/grpc_csharp_plugin > /usr/local/bin/grpc_csharp_plugin \
 && chmod +x /usr/local/bin/grpc_csharp_plugin \
 && rm temp.zip
#   Setup JAVA_HOME, this is useful for docker commandline
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64/"
RUN export JAVA_HOME
#   Ubuntu apt uses "nodejs" as the executable, but everything else expects
#   the executable to be spelled "node".
RUN ln -s /usr/bin/nodejs /usr/local/bin/node
#   Install Go.
RUN mkdir -p /golang \
 && cd /golang \
 && curl https://dl.google.com/go/go1.11.linux-amd64.tar.gz > go.tar.gz \
 && (echo 'b3fcf280ff86558e0559e185b601c9eade0fd24c900b4c63cd14d1d38613e499 go.tar.gz' | sha256sum -c ) \
 && tar xzf go.tar.gz \
 && rm go.tar.gz \
 && cd /
ENV PATH="$PATH:/golang/go/bin"
#   Download the go protobuf support.
ENV GOPATH="/go"
ENV PATH="$GOPATH/bin:$PATH"
RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" "$GOPATH/pkg" \
 && chmod -R 777 "$GOPATH" \
 && go get -u github.com/golang/protobuf/protoc-gen-go \
 && go clean -cache -testcache -modcache
#   Setup tools for codegen of Ruby
RUN gem install rake --version 13.0.6 --no-ri --no-rdoc \
 && gem install rubocop --version 1.50.1 --no-ri --no-rdoc \
 && gem install bundler --version 2.4.12 --no-ri --no-rdoc \
 && gem install rake --version 13.0.6 --no-ri --no-rdoc \
 && gem install grpc-tools --version 1.53.0 --no-ri --no-rdoc
#   Install grpc_php_plugin
RUN git clone -b v1.17.1 --recurse-submodules --depth=1 https://github.com/grpc/grpc.git /temp/grpc \
 && cd /temp/grpc \
 && make -j $( nproc ;) grpc_php_plugin \
 && mv ./bins/opt/grpc_php_plugin /usr/local/bin/ \
 && cd / \
 && rm -r /temp/grpc
#   Install PHP formatting tools
RUN curl -L https://github.com/FriendsOfPHP/PHP-CS-Fixer/releases/download/v2.9.1/php-cs-fixer.phar -o /usr/local/bin/php-cs-fixer \
 && chmod a+x /usr/local/bin/php-cs-fixer \
 && cd /
RUN curl -L https://squizlabs.github.io/PHP_CodeSniffer/phpcbf.phar -o /usr/local/bin/phpcbf \
 && chmod a+x /usr/local/bin/phpcbf \
 && cd /
#   Used to add docstrings to the Python protoc output.
RUN pip3 install protoc-docs-plugin==0.3.0
#   Install .NET Core SDK
ENV DOTNET_SDK_VERSION="1.0.4"
ENV DOTNET_SDK_DOWNLOAD_URL="https://dotnetcli.blob.core.windows.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-dev-ubuntu.16.04-x64.$DOTNET_SDK_VERSION.tar.gz"
RUN curl -SL $DOTNET_SDK_DOWNLOAD_URL --output dotnet.tar.gz \
 && mkdir -p /usr/share/dotnet \
 && tar -zxf dotnet.tar.gz -C /usr/share/dotnet \
 && rm dotnet.tar.gz \
 && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet
#   Install googleapis.
RUN git clone --single-branch https://github.com/googleapis/googleapis \
 && cd googleapis \
 && git checkout $GOOGLEAPIS_HASH \
 && cd .. \
 && rm -rf /googleapis/.git/
#   Install toolkit.
RUN git clone --single-branch https://github.com/googleapis/gapic-generator toolkit \
 && cd toolkit/ \
 && git checkout $GAPIC_GENERATOR_HASH \
 && ./gradlew fatJar createToolPaths \
 && cd .. \
 && rm -rf /toolkit/.git/
ENV TOOLKIT_HOME="/toolkit"
#   Setup git config used by github commit pushing.
RUN git config --global user.email googleapis-publisher@google.com \
 && git config --global user.name "Google API Publisher"
#   Setup artman user config
#   Note: This is somewhat brittle as it relies on a specific path
#   outside of or inside Docker.
#
#   This should probably be fixed to have the smoke test itself provide
#   the configuration.
#   TODO (lukesneeringer): Fix this.
RUN mkdir -p /root/
COPY artman-user-config-in-docker.yaml /root/.artman/config.yaml
#   Install artman.
COPY . /artman
ARG install_artman_from_source=false
RUN if [ "$install_artman_from_source" = true ] ; then pip3 install -e /artman ; else pip3 install googleapis-artman==$ARTMAN_VERSION ;rm -r /artman ; fi
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
