#   adapted from github.com/bitwalker/alpine-elixir
#   and github.com/bitwalker/alpine-erlang:latest
#   2017-01-15 update to elixir 1.4.0
FROM gliderlabs/alpine:3.4
MAINTAINER Your Name <name@your-domain.com>
#   Important!  Update this no-op ENV variable when this Dockerfile
#   is updated with the current date. It will force refresh of all
#   of the base images and things like `apt-get update` won't be using
#   old cached versions when the Dockerfile is built.
ENV REFRESHED_AT="2017-01-15" \
    LANG="en_US.UTF-8" \
    HOME="/opt/app/" \
    TERM="xterm" \
    ERLANG_VER="19.1.6" \
    ELIXIR_VER="1.4.0"
WORKDIR /tmp/erlang-build
#   Install updates and build tools (we're going to reuse these so make them a layer of their own)
RUN mkdir -p ${HOME} \
 && adduser -s /bin/sh -u 1001 -G root -h ${HOME} -S -D default \
 && chown -R 1001:0 ${HOME} \
 && echo "@edge http://nl.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories \
 && apk --no-cache --update upgrade \
 && apk add ca-certificates openssl-dev ncurses-dev unixodbc-dev zlib-dev git autoconf build-base make perl-dev --no-cache \
 && update-ca-certificates --fresh
#   Install Erlang
RUN git clone -b OTP-${ERLANG_VER} --single-branch --depth 1 https://github.com/erlang/otp.git . \
 && export ERL_TOP=/tmp/erlang-build \
 && export PATH=$ERL_TOP/bin:$PATH \
 && export CPPFlAGS="-D_BSD_SOURCE $CPPFLAGS" \
 && ./otp_build autoconf \
 && ./configure --prefix=/usr --sysconfdir=/etc --mandir=/usr/share/man --infodir=/usr/share/info --without-javac --without-wx --without-debugger --without-observer --without-jinterface --without-common_test --without-cosEvent --without-cosEventDomain --without-cosFileTransfer --without-cosNotification --without-cosProperty --without-cosTime --without-cosTransactions --without-dialyzer --without-et --without-gs --without-ic --without-megaco --without-orber --without-percept --without-typer --enable-threads --enable-shared-zlib --enable-ssl=dynamic-ssl-lib --enable-hipe \
 && make -j4 \
 && make install
#   Move on to Elixir
WORKDIR /tmp/elixir-build
RUN git clone https://github.com/elixir-lang/elixir \
 && cd elixir \
 && git checkout v${ELIXIR_VER} \
 && make \
 && make install \
 && mix local.hex --force \
 && mix local.rebar --force
WORKDIR ${HOME}
CMD ["/bin/sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
