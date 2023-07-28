FROM alpine:latest AS base
#   We install and then uninstall quality to cache the dependencies
#   while we still have the build tools installed but still be able to
#   install the very latest quality gem later on without having the disk
#   space impact of two versions.
RUN apk update \
 && apk add ruby=3.1.4-r0 ruby-irb ruby-dev=3.1.4-r0 make=4.3-r1 gcc=12.2.1_git20220924-r4 libc-dev=0.7.2-r3 git=2.38.4-r1 icu-dev=72.1-r1 zlib-dev=1.2.13-r0 g++=12.2.1_git20220924-r4 cmake=3.24.4-r0 openssl-dev=3.0.8-r3 coreutils=9.1-r0 --no-cache \
 && gem install bigdecimal --version 3.1.4 --no-ri --no-rdoc \
 && gem uninstall quality \
 && strip /usr/lib/ruby/gems/2.5.0/extensions/x86_64-linux/2.5.0/rugged-*/rugged/rugged.so \
 && apk del ruby-irb ruby-dev make gcc libc-dev icu-dev zlib-dev g++ cmake openssl-dev nghttp2 curl pax-utils \
 && apk add libssl1.1=1.1.1t-r2 icu-libs=72.1-r1 --no-cache \
 && rm -fr /usr/lib/ruby/gems/2.5.0/gems/rugged-0.27.4/vendor/libgit2/build/src /usr/lib/ruby/gems/2.5.0/gems/rugged-0.27.4/vendor/libgit2/src /usr/lib/ruby/gems/2.5.0/gems/rugged-0.27.4/ext/rugged /usr/lib/ruby/gems/2.5.0/gems/rugged-0.27.4/vendor/libgit2/build/libgit2.a /usr/lib/ruby/gems/2.5.0/gems/rugged-0.27.4/lib/rugged/rugged.so /usr/lib/ruby/gems/2.5.0/gems/unf_ext-0.0.7.5/ext/unf_ext/unf /usr/lib/ruby/gems/2.5.0/gems/kramdown-1.17.0/test /usr/lib/ruby/gems/2.5.0/gems/ruby_parser-3.11.0/lib/*.y /usr/lib/ruby/gems/2.5.0/gems/ruby_parser-3.11.0/lib/*.yy /usr/lib/ruby/gems/2.5.0/gems/ruby_parser-3.11.0/lib/*.rex /usr/lib/ruby/gems/2.5.0/cache /usr/lib/ruby/gems/2.5.0/gems/erubis-2.7.0/doc-api /usr/lib/ruby/gems/2.5.0/gems/reek-5.0.2/spec /usr/lib/ruby/gems/2.5.0/gems/kwalify-0.7.2/doc-api \
 && echo "Done"
RUN mkdir /usr/quality
COPY sample-project/.pronto.yml /usr/quality/.pronto.yml
COPY sample-project/Rakefile /usr/quality/Rakefile
COPY entrypoint.sh /
FROM base AS latest
VOLUME /usr/app
WORKDIR /usr/app
ENTRYPOINT ["/entrypoint.sh"]
ARG quality_gem_version
RUN gem install --no-ri --no-rdoc
CMD ["quality"]
FROM base AS python-base
#
#   Install flake8 and pycodestyle
#
RUN apk add python3=3.10.11-r0 py3-pip=22.3.1-r1 --no-cache \
 && pip3 install flake8 \
 && apk del py3-pip \
 && pip3 uninstall -y pip
RUN apk update \
 && apk add ruby-dev=3.1.4-r0 gcc=12.2.1_git20220924-r4 make=4.3-r1 g++=12.2.1_git20220924-r4 cmake=3.24.4-r0 --no-cache \
 && gem install io-console --version 0.6.0 --no-ri --no-rdoc \
 && apk del ruby-dev gcc make g++ cmake
FROM python-base AS python
VOLUME /usr/app
WORKDIR /usr/app
ENTRYPOINT ["/entrypoint.sh"]
ARG quality_gem_version
RUN gem install --no-ri --no-rdoc
CMD ["quality"]
FROM python-base AS shellcheck-builder
#
#   Install shellcheck
#
#   https://github.com/mitchty/alpine-ghc
COPY mitch.tishmack@gmail.com-55881c97.rsa.pub /etc/apk/keys/mitch.tishmack@gmail.com-55881c97.rsa.pub
RUN echo "https://s3-us-west-2.amazonaws.com/alpine-ghc/8.0" >> /etc/apk/repositories \
 && apk add ghc=9.0.2-r1 cabal=3.8.1.0-r2 stack --no-cache
#   https://github.com/NLKNguyen/alpine-shellcheck/blob/master/builder/Dockerfile
RUN apk add build-base=0.5-r3 git=2.38.4-r1 wget=1.21.3-r2 --no-cache
RUN mkdir -p /usr/src/shellcheck
WORKDIR /usr/src/shellcheck
RUN git clone https://github.com/koalaman/shellcheck .
RUN cabal update \
 && cabal install
ENV PATH="/root/.cabal/bin:$PATH"
FROM python-base AS shellcheck-base
COPY --from=4 /root/.cabal/bin /usr/local/bin
RUN apk update \
 && apk add ruby=3.1.4-r0 ruby-dev=3.1.4-r0 --no-cache
RUN gem install --no-ri --no-rdoc
FROM shellcheck-base AS shellcheck
VOLUME /usr/app
WORKDIR /usr/app
ENTRYPOINT ["/entrypoint.sh"]
ARG quality_gem_version
RUN gem install --no-ri --no-rdoc
CMD ["quality"]
FROM shellcheck-base AS jumbo-base
#   https://github.com/sgerrand/alpine-pkg-glibc
RUN apk add ca-certificates=20220614-r4 wget=1.21.3-r2 --no-cache \
 && wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub \
 && wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.28-r0/glibc-2.28-r0.apk \
 && apk add glibc-2.28-r0.apk
ENV LANG="C.UTF-8"
#   To upgrade:
#   1. Check https://jdk.java.net/13/ for latest build - see 'Alpine Linux/x64' link
#   2. See if there's an update here: https://github.com/docker-library/openjdk/blob/master/13/jdk/alpine/Dockerfile
ENV JAVA_HOME="/opt/openjdk-13"
ENV PATH="$JAVA_HOME/bin:$PATH"
#   https://jdk.java.net/
ENV JAVA_VERSION="13-ea+19"
ENV JAVA_URL="https://download.java.net/java/early_access/alpine/19/binaries/openjdk-13-ea+19_linux-x64-musl_bin.tar.gz"
ENV JAVA_SHA256="010ea985fba7e3d89a9170545c4e697da983cffc442b84e65dba3baa771299a5"
#   "For Alpine Linux, builds are produced on a reduced schedule and may not be in sync with the other platforms."
RUN set -eux ; wget -O /openjdk.tgz "$JAVA_URL" ; echo "$JAVA_SHA256 */openjdk.tgz" | sha256sum -c - ; mkdir -p "$JAVA_HOME" ; tar --extract --file /openjdk.tgz --directory "$JAVA_HOME" --strip-components 1 ; rm /openjdk.tgz ; java -Xshare:dump ; java --version ; javac --version
#   https://docs.oracle.com/javase/10/tools/jshell.htm
#   https://docs.oracle.com/javase/10/jshell/
#   https://en.wikipedia.org/wiki/JShell
#   https://github.com/frol/docker-alpine-scala/blob/master/Dockerfile
ENV SCALA_VERSION="2.12.0-M5" \
    SCALA_HOME="/usr/share/scala"
#   NOTE: bash is used by scala/scalac scripts, and it cannot be easily replaced with ash.
RUN apk add wget=1.21.3-r2 ca-certificates=20220614-r4 --no-cache --virtual=.build-dependencies \
 && apk add bash=5.2.15-r0 --no-cache \
 && cd "/tmp" \
 && wget "https://downloads.typesafe.com/scala/${SCALA_VERSION}/scala-${SCALA_VERSION}.tgz" \
 && tar xzf "scala-${SCALA_VERSION}.tgz" \
 && mkdir "${SCALA_HOME}" \
 && rm "/tmp/scala-${SCALA_VERSION}/bin/"*.bat \
 && mv "/tmp/scala-${SCALA_VERSION}/bin" "/tmp/scala-${SCALA_VERSION}/lib" "${SCALA_HOME}" \
 && ln -s "${SCALA_HOME}/bin/"* "/usr/bin/" \
 && apk del .build-dependencies \
 && rm -rf "/tmp/"*
#  https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle-batch_2.10/0.5.0/scalastyle_2.10-0.5.0.jar" && \
ENV SCALASTYLE_JAR="scalastyle_2.10-0.8.0-batch.jar"
COPY etc/scalastyle_config.xml /usr/src/scalastyle_config.xml
RUN cd /usr/lib \
 && wget "https://oss.sonatype.org/content/repositories/releases/org/scalastyle/scalastyle_2.10/0.8.0/${SCALASTYLE_JAR}" \
 && echo '#!/bin/bash' > /bin/scalastyle \
 && echo "java -jar `pwd `/${SCALASTYLE_JAR}" --config "/usr/src/scalastyle_config.xml" '${@}' >> /bin/scalastyle \
 && chmod +x /bin/scalastyle
FROM jumbo-base AS jumbo
VOLUME /usr/app
WORKDIR /usr/app
ENTRYPOINT ["/entrypoint.sh"]
ARG quality_gem_version
RUN gem install --no-ri --no-rdoc
CMD ["quality"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
