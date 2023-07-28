FROM alpine:3.10
MAINTAINER Paul Schoenfelder <paulschoenfelder@gmail.com>
#   Important!  Update this no-op ENV variable when this Dockerfile
#   is updated with the current date. It will force refresh of all
#   of the base images and things like `apt-get update` won't be using
#   old cached versions when the Dockerfile is built.
ENV REFRESHED_AT="2019-06-21" \
    LANG="en_US.UTF-8" \
    HOME="/opt/app/" \
    TERM="xterm" \
    ERLANG_VERSION="22.0.4"
WORKDIR /tmp/erlang-build
#   Install Erlang
RUN mkdir -p "${HOME}" \
 && adduser -s /bin/sh -u 1001 -G root -h "${HOME}" -S -D default \
 && chown -R 1001:0 "${HOME}" \
 && echo "@main http://dl-cdn.alpinelinux.org/alpine/v3.10/main" >> /etc/apk/repositories \
 && echo "@community http://dl-cdn.alpinelinux.org/alpine/v3.10/community" >> /etc/apk/repositories \
 && echo "@edge http://dl-cdn.alpinelinux.org/alpine/edge/main" >> /etc/apk/repositories \
 && apk --no-cache --update --available upgrade \
 && apk add pcre@edge --no-cache --update \
 && apk add bash=5.0.0-r0 ca-certificates=20191127-r2 openssl-dev=1.1.1k-r0 ncurses-dev=6.1_p20190518-r2 unixodbc-dev=2.3.7-r1 zlib-dev=1.2.11-r1 --no-cache --update \
 && apk add dpkg-dev=1.19.7-r0 dpkg=1.19.7-r0 binutils=2.32-r1 git=2.22.5-r0 autoconf=2.69-r2 build-base=0.5-r1 perl-dev=5.28.3-r0 --no-cache --virtual .erlang-build \
 && git clone -b OTP-$ERLANG_VERSION --single-branch --depth 1 https://github.com/erlang/otp.git . \
 && export ERL_TOP=/tmp/erlang-build \
 && export PATH=$ERL_TOP/bin:$PATH \
 && export CPPFlAGS="-D_BSD_SOURCE $CPPFLAGS" \
 && ./otp_build autoconf \
 && ./configure --prefix=/usr --build="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" --sysconfdir=/etc --mandir=/usr/share/man --infodir=/usr/share/info --without-javac --without-wx --without-debugger --without-observer --without-jinterface --without-cosEvent --without-cosEventDomain --without-cosFileTransfer --without-cosNotification --without-cosProperty --without-cosTime --without-cosTransactions --without-et --without-gs --without-ic --without-megaco --without-orber --without-percept --without-typer --enable-threads --enable-shared-zlib --enable-ssl=dynamic-ssl-lib --enable-hipe \
 && make -j4 \
 && make install \
 && cd $HOME \
 && rm -rf /tmp/erlang-build \
 && update-ca-certificates --fresh \
 && /usr/bin/erl -eval "beam_lib:strip_release('/usr/lib/erlang/lib')" -s init stop > /dev/null \
 && (/usr/bin/strip /usr/lib/erlang/erts-*/bin/* || true ) \
 && rm -rf /usr/lib/erlang/usr/ \
 && rm -rf /usr/lib/erlang/misc/ \
 && for DIR in /usr/lib/erlang/erts* /usr/lib/erlang/lib/*; do rm -rf ${DIR}/src/*.erl ;rm -rf ${DIR}/doc ;rm -rf ${DIR}/man ;rm -rf ${DIR}/examples ;rm -rf ${DIR}/emacs ;rm -rf ${DIR}/c_src ; done \
 && rm -rf /usr/lib/erlang/erts-*/lib/ \
 && rm /usr/lib/erlang/erts-*/bin/dialyzer \
 && rm /usr/lib/erlang/erts-*/bin/erlc \
 && rm /usr/lib/erlang/erts-*/bin/typer \
 && rm /usr/lib/erlang/erts-*/bin/ct_run \
 && apk del --force .erlang-build
WORKDIR ${HOME}
CMD ["/bin/sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
