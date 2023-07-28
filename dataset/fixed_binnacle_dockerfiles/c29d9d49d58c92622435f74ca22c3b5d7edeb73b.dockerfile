FROM jupyter/minimal-notebook:1af3089901bb
#  FROM jupyter/tensorflow-notebook:1af3089901bb
USER root
#   Set WORKDIR
WORKDIR /root
#   These apt-get packages are the only unpinned dependencies, so they are presumably
#   the only elements in the image build process that introduce the risk of
#   non-reproducibility. That is, they could in theory change in a way that
#   broke backward compatibility, and caused subsequent builds of the docker image
#   to function differently.
#   Install related packages and set LLVM 3.8 as the compiler
RUN apt-get update -q \
 && apt-get install --no-install-recommends make libc6-dev clang-3.8 curl libedit-dev libpython2.7 libicu-dev libssl-dev libxml2 tzdata git libcurl4-openssl-dev pkg-config -q -y \
 && update-alternatives --quiet --install /usr/bin/clang clang /usr/bin/clang-3.8 100 \
 && update-alternatives --quiet --install /usr/bin/clang++ clang++ /usr/bin/clang++-3.8 100 \
 && rm -r /var/lib/apt/lists/*
#   Everything up to here should cache nicely between Swift versions, assuming dev dependencies change little
ARG SWIFT_PLATFORM=ubuntu16.04
ARG SWIFT_BRANCH=swift-4.1-release
ARG SWIFT_VERSION=swift-4.1-RELEASE
ENV SWIFT_PLATFORM="$SWIFT_PLATFORM" \
    SWIFT_BRANCH="$SWIFT_BRANCH" \
    SWIFT_VERSION="$SWIFT_VERSION"
#   Download GPG keys, signature and Swift package, then unpack, cleanup and execute permissions for foundation libs
RUN SWIFT_URL=https://swift.org/builds/$SWIFT_BRANCH/$( echo "$SWIFT_PLATFORM" | tr -d . ;)/$SWIFT_VERSION/$SWIFT_VERSION-$SWIFT_PLATFORM.tar.gz \
 && curl -fSsL $SWIFT_URL -o swift.tar.gz \
 && curl -fSsL $SWIFT_URL.sig -o swift.tar.gz.sig \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && set -e ; for key in 8513444E2DA36B7C1659AF4D7638F1FB2B2B08C4 A3BAFD3556A59079C06894BD63BC1CFE91D306C6 5E4DF843FB065D7F7E24FBA2EF5430F071E1B235; do gpg --quiet --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done \
 && gpg --batch --verify --quiet swift.tar.gz.sig swift.tar.gz \
 && tar -xzf swift.tar.gz --directory / --strip-components=1 \
 && rm -r "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz \
 && chmod -R o+r /usr/lib/swift
#   Print Installed Swift Version
RUN swift --version
#   RUN apt-get update && \
#   apt-get -y install build-essential
#   Install ZMQ
RUN cd /tmp/ \
 && curl -L -O https://github.com/zeromq/zeromq4-1/releases/download/v4.1.4/zeromq-4.1.4.tar.gz \
 && tar xf /tmp/zeromq-4.1.4.tar.gz \
 && cd /tmp/zeromq-4.1.4 \
 && ./configure --without-libsodium \
 && make \
 && make install \
 && ldconfig
#   Build swift kernel executable as root in /kernels/iSwift
RUN mkdir -p /kernels/iSwift
#   copy only the Swift package itself and iSwiftKernel, so that we don't
#   trigger image rebuilds when we edit docs or pieces of the Dockerfile
#   itself which are irrelevant to the image
COPY Includes /kernels/iSwift/Includes/
COPY Package.swift /kernels/iSwift/
COPY Sources iSwiftKernel /kernels/iSwift/Sources/
COPY iSwiftKernel /kernels/iSwift/iSwiftKernel/
WORKDIR /kernels/iSwift
RUN swift package update
RUN swift build
#   But install the kernelspec into jupyter as the NB_USER
USER ${NB_USER}
RUN jupyter kernelspec install --user /kernels/iSwift/iSwiftKernel
#   Change the Swift kernel executable to be onwed by NB_USER, so we can run it
USER root
RUN chown -R ${NB_USER} /kernels/iSwift
USER $NB_USER
USER ${NB_USER}
WORKDIR /home/${NB_USER}
# Please add your HEALTHCHECK here!!!
