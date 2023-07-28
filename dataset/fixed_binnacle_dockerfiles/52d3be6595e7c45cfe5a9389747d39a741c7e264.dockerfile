#  ###############################################################################
#   Set up environment variables, OS packages, and scripts that are common to the
#   build and distribution layers in this Dockerfile
FROM alpine:3.9 AS base
#   Must be one of 'gmp' or 'simple'; used to build GHC with support for either
#   'integer-gmp' (with 'libgmp') or 'integer-simple'
#
#   Default to building with 'integer-gmp' and 'libgmp' support
ARG GHC_BUILD_TYPE
#   Must be a valid GHC version number, only tested with 8.4.4, 8.6.4, and 8.6.5
#
#   Default to GHC version 8.6.5 (latest at the time of writing)
ARG GHC_VERSION=8.6.5
#   Add ghcup's bin directory to the PATH so that the versions of GHC it builds
#   are available in the build layers
ENV GHCUP_INSTALL_BASE_PREFIX="/"
ENV PATH="/.ghcup/bin:$PATH"
#   Use the latest version of ghcup (at the time of writing)
ENV GHCUP_VERSION="0.0.7"
ENV GHCUP_SHA256="b4b200d896eb45b56c89d0cfadfcf544a24759a6ffac029982821cc96b2faedb  ghcup"
#   Install the basic required dependencies to run 'ghcup' and 'stack'
RUN apk upgrade --no-cache \
 && apk add curl=7.64.0-r5 gcc=8.3.0-r0 git=2.20.4-r0 libc-dev=0.7.1-r0 xz=5.2.4-r0 --no-cache \
 && if [ "${GHC_BUILD_TYPE}" = "gmp" ] ; then echo "Installing 'libgmp'" \
 && apk add gmp-dev=6.1.2-r1 --no-cache ; fi
#   Download, verify, and install ghcup
RUN echo "Downloading and installing ghcup" \
 && cd /tmp \
 && wget -P /tmp/ "https://gitlab.haskell.org/haskell/ghcup/raw/${GHCUP_VERSION}/ghcup" \
 && if ! echo -n "${GHCUP_SHA256}" | sha256sum -c - ; then echo "ghcup-${GHCUP_VERSION} checksum failed" >&2 \
 && exit 1 ; fi ; mv /tmp/ghcup /usr/bin/ghcup \
 && chmod +x /usr/bin/ghcup
#  ###############################################################################
#   Intermediate layer that builds GHC
FROM base AS build-ghc
#   Carry build args through to this stage
ARG GHC_BUILD_TYPE=gmp
ARG GHC_VERSION=8.6.5
RUN echo "Install OS packages necessary to build GHC" \
 && apk add autoconf=2.69-r2 automake=1.16.1-r0 binutils-gold=2.31.1-r2 build-base=0.5-r1 coreutils=8.30-r0 cpio=2.12-r3 ghc=8.4.3-r0 linux-headers=4.18.13-r1 libffi-dev=3.2.1-r6 llvm5=5.0.2-r0 musl-dev=1.1.20-r6 ncurses-dev=6.1_p20190105-r0 perl=5.26.3-r1 python3=3.6.9-r3 py3-sphinx=1.8.3-r0 zlib-dev=1.2.11-r1 --no-cache
COPY docker/build-gmp.mk /tmp/build-gmp.mk
COPY docker/build-simple.mk /tmp/build-simple.mk
RUN if [ "${GHC_BUILD_TYPE}" = "gmp" ] ; then echo "Using 'integer-gmp' build config" \
 && apk add gmp-dev=6.1.2-r1 --no-cache \
 && mv /tmp/build-gmp.mk /tmp/build.mk \
 && rm /tmp/build-simple.mk ; elif [ "${GHC_BUILD_TYPE}" = "simple" ] ; then echo "Using 'integer-simple' build config" \
 && mv /tmp/build-simple.mk /tmp/build.mk \
 && rm tmp/build-gmp.mk ; else echo "Invalid argument \[ GHC_BUILD_TYPE=${GHC_BUILD_TYPE} \]" \
 && exit 1 ; fi
RUN echo "Compiling and installing GHC" \
 && LD=ld.gold SPHINXBUILD=/usr/bin/sphinx-build-3 ghcup -v compile -j $( nproc ;) -c /tmp/build.mk ${GHC_VERSION} ghc-8.4.3 \
 && rm /tmp/build.mk \
 && echo "Uninstalling GHC bootstrapping compiler" \
 && apk del ghc \
 && ghcup set ${GHC_VERSION}
#  ###############################################################################
#   Intermediate layer that assembles 'stack' tooling
FROM base AS build-tooling
ENV STACK_VERSION="1.9.3"
ENV STACK_SHA256="c9bf6d371b51de74f4bfd5b50965966ac57f75b0544aebb59ade22195d0b7543  stack-${STACK_VERSION}-linux-x86_64-static.tar.gz"
#   Download, verify, and install stack
RUN echo "Downloading and installing stack" \
 && cd /tmp \
 && wget -P /tmp/ "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz" \
 && if ! echo -n "${STACK_SHA256}" | sha256sum -c - ; then echo "stack-${STACK_VERSION} checksum failed" >&2 \
 && exit 1 ; fi ; tar -xvzf /tmp/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz \
 && cp -L /tmp/stack-${STACK_VERSION}-linux-x86_64-static/stack /usr/bin/stack \
 && rm /tmp/stack-${STACK_VERSION}-linux-x86_64-static.tar.gz \
 && rm -rf /tmp/stack-${STACK_VERSION}-linux-x86_64-static
#  ###############################################################################
#   Assemble the final image
FROM base
#   Carry build args through to this stage
ARG GHC_BUILD_TYPE=gmp
ARG GHC_VERSION=8.6.5
COPY --from=build-ghc /.ghcup /.ghcup
COPY --from=build-tooling /usr/bin/stack /usr/bin/stack
#   NOTE: 'stack --docker' needs bash + usermod/groupmod (from shadow)
RUN apk add bash=4.4.19-r1 shadow=4.5-r2 --no-cache
RUN ghcup set ${GHC_VERSION} \
 && stack config set system-ghc --global true
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
