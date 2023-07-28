#   Alpine Linux is not officially supported by the RabbitMQ team -- use at your own risk!
FROM alpine:3.8
RUN apk add bash=4.4.19-r1 procps=3.3.15-r0 'su-exec>=0.2' --no-cache
#   Default to a PGP keyserver that pgp-happy-eyeballs recognizes, but allow for substitutions locally
ARG PGP_KEYSERVER=ha.pool.sks-keyservers.net
#   If you are building this image locally and are getting `gpg: keyserver receive failed: No data` errors,
#   run the build with a different PGP_KEYSERVER, e.g. docker build --tag rabbitmq:3.7 --build-arg PGP_KEYSERVER=pgpkeys.eu 3.7/ubuntu
#   For context, see https://github.com/docker-library/official-images/issues/4252
#   Using the latest OpenSSL LTS release, with support until September 2023 - https://www.openssl.org/source/
ENV OPENSSL_VERSION="1.1.1c"
ENV OPENSSL_SOURCE_SHA256="f6fb3079ad15076154eda9413fed42877d668e7069d9b87396d0804fdb3f4c90"
#   https://www.openssl.org/community/omc.html
ENV OPENSSL_PGP_KEY_IDS="0x8657ABB260F056B1E5190839D9C4D26D0E604491 0x5B2545DAB21995F4088CEFAA36CEE4DEB00CFE33 0xED230BEC4D4F2518B9D7DF41F0DB4D21C1D35231 0xC1F33DD8CE1D4CC613AF14DA9195C48241FBF7DD 0x7953AC1FBC3DC8B3B292393ED5E9E43F7DF9EE8C 0xE5E52560DD91C556DDBDA5D02064C53641C25E5D"
#   Use the latest stable Erlang/OTP release (https://github.com/erlang/otp/tags)
ENV OTP_VERSION="22.0.4"
#   TODO add PGP checking when the feature will be added to Erlang/OTP's build system
#   http://erlang.org/pipermail/erlang-questions/2019-January/097067.html
ENV OTP_SOURCE_SHA256="71b2fe49ed5ac386ebc189dd2e5f4b95b11b4427936be0e3c5695a903ea9ffcd"
#   Install dependencies required to build Erlang/OTP from source
#   http://erlang.org/doc/installation_guide/INSTALL.html
#   autoconf: Required to configure Erlang/OTP before compiling
#   dpkg-dev: Required to set up host & build type when compiling Erlang/OTP
#   gnupg: Required to verify OpenSSL artefacts
#   libncurses5-dev: Required for Erlang/OTP new shell & observer_cli - https://github.com/zhongwencool/observer_cli
RUN set -eux ; apk add autoconf=2.69-r2 ca-certificates=20191127-r2 dpkg-dev=1.18.24-r0 dpkg=1.18.24-r0 gcc=6.4.0-r9 gnupg=2.2.19-r0 libc-dev=0.7.1-r0 linux-headers=4.4.6-r2 make=4.2.1-r2 ncurses-dev=6.1_p20180818-r1 --no-cache --virtual .build-deps ; OPENSSL_SOURCE_URL="https://www.openssl.org/source/openssl-$OPENSSL_VERSION.tar.gz" ; OPENSSL_PATH="/usr/local/src/openssl-$OPENSSL_VERSION" ; OPENSSL_CONFIG_DIR=/usr/local/etc/ssl ; mkdir /usr/local/src ; wget --output-document "$OPENSSL_PATH.tar.gz.asc" "$OPENSSL_SOURCE_URL.asc" ; wget --output-document "$OPENSSL_PATH.tar.gz" "$OPENSSL_SOURCE_URL" ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $OPENSSL_PGP_KEY_IDS; do gpg --batch --keyserver "$PGP_KEYSERVER" --recv-keys "$key" ; done ; gpg --batch --verify "$OPENSSL_PATH.tar.gz.asc" "$OPENSSL_PATH.tar.gz" ; gpgconf --kill all ; rm -rf "$GNUPGHOME" ; echo "$OPENSSL_SOURCE_SHA256 *$OPENSSL_PATH.tar.gz" | sha256sum -c - ; mkdir -p "$OPENSSL_PATH" ; tar --extract --file "$OPENSSL_PATH.tar.gz" --directory "$OPENSSL_PATH" --strip-components 1 ; cd "$OPENSSL_PATH" ; MACHINE="$( dpkg-architecture --query DEB_BUILD_GNU_CPU ;)" RELEASE="4.x.y-z" SYSTEM='Linux' BUILD='???' ./config --openssldir="$OPENSSL_CONFIG_DIR" ; make -j "$( getconf _NPROCESSORS_ONLN ;)" ; make install_sw install_ssldirs ; cd .. ; rm -rf "$OPENSSL_PATH"* ; rmdir "$OPENSSL_CONFIG_DIR/certs" "$OPENSSL_CONFIG_DIR/private" ; ln -sf /etc/ssl/certs /etc/ssl/private "$OPENSSL_CONFIG_DIR" ; openssl version ; OTP_SOURCE_URL="https://github.com/erlang/otp/archive/OTP-$OTP_VERSION.tar.gz" ; OTP_PATH="/usr/local/src/otp-$OTP_VERSION" ; mkdir -p "$OTP_PATH" ; wget --output-document "$OTP_PATH.tar.gz" "$OTP_SOURCE_URL" ; echo "$OTP_SOURCE_SHA256 *$OTP_PATH.tar.gz" | sha256sum -c - ; tar --extract --file "$OTP_PATH.tar.gz" --directory "$OTP_PATH" --strip-components 1 ; cd "$OTP_PATH" ; export ERL_TOP="$OTP_PATH" ; ./otp_build autoconf ; CFLAGS="$( dpkg-buildflags --get CFLAGS ;)" ; export CFLAGS ; hostArch="$( dpkg-architecture --query DEB_HOST_GNU_TYPE ;)" ; buildArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; dpkgArch="$( dpkg --print-architecture ;)" ; dpkgArch="${dpkgArch##*-}" ; ./configure --host="$hostArch" --build="$buildArch" --disable-dynamic-ssl-lib --disable-hipe --disable-sctp --disable-silent-rules --enable-clock-gettime --enable-hybrid-heap --enable-kernel-poll --enable-shared-zlib --enable-smp-support --enable-threads --with-microstate-accounting=extra --without-common_test --without-debugger --without-dialyzer --without-diameter --without-edoc --without-erl_docgen --without-erl_interface --without-et --without-eunit --without-ftp --without-hipe --without-jinterface --without-megaco --without-observer --without-odbc --without-reltool --without-ssh --without-tftp --without-wx ; make -j "$( getconf _NPROCESSORS_ONLN ;)" GEN_OPT_FLGS="-O2 -fno-strict-aliasing" ; make install ; cd .. ; rm -rf "$OTP_PATH"* /usr/local/lib/erlang/lib/*/examples /usr/local/lib/erlang/lib/*/src ; runDeps="$( scanelf --needed --nobanner --format '%n#p' --recursive /usr/local | tr ',' '\n' | sort -u | awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' ;)" ; apk add $runDeps --no-cache --virtual .otp-run-deps ; apk del --no-network .build-deps ; openssl version ; erl -noshell -eval 'io:format("~p~n~n~p~n~n", [crypto:supports(), ssl:versions()]), init:stop().'
ENV RABBITMQ_DATA_DIR="/var/lib/rabbitmq"
#   Create rabbitmq system user & group, fix permissions & allow root user to connect to the RabbitMQ Erlang VM
RUN set -eux ; addgroup -g 101 -S rabbitmq ; adduser -u 100 -S -h "$RABBITMQ_DATA_DIR" -G rabbitmq rabbitmq ; mkdir -p "$RABBITMQ_DATA_DIR" /etc/rabbitmq /tmp/rabbitmq-ssl /var/log/rabbitmq ; chown -fR rabbitmq:rabbitmq "$RABBITMQ_DATA_DIR" /etc/rabbitmq /tmp/rabbitmq-ssl /var/log/rabbitmq ; chmod 777 "$RABBITMQ_DATA_DIR" /etc/rabbitmq /tmp/rabbitmq-ssl /var/log/rabbitmq ; ln -sf "$RABBITMQ_DATA_DIR/.erlang.cookie" /root/.erlang.cookie
#   Use the latest stable RabbitMQ release (https://www.rabbitmq.com/download.html)
ENV RABBITMQ_VERSION="3.8.0-beta.4"
#   https://www.rabbitmq.com/signatures.html#importing-gpg
ENV RABBITMQ_PGP_KEY_ID="0x0A9AF2115F4687BD29803A206B73A36E6026DFCA"
ENV RABBITMQ_HOME="/opt/rabbitmq"
#   Add RabbitMQ to PATH, send all logs to TTY
ENV PATH="$RABBITMQ_HOME/sbin:$PATH" \
    RABBITMQ_LOGS="-" \
    RABBITMQ_SASL_LOGS="-"
#   Install RabbitMQ
RUN set -eux ; apk add ca-certificates=20191127-r2 gnupg=2.2.19-r0 xz=5.2.4-r0 --no-cache --virtual .build-deps ; RABBITMQ_SOURCE_URL="https://github.com/rabbitmq/rabbitmq-server/releases/download/v$RABBITMQ_VERSION/rabbitmq-server-generic-unix-$RABBITMQ_VERSION.tar.xz" ; RABBITMQ_PATH="/usr/local/src/rabbitmq-$RABBITMQ_VERSION" ; wget --output-document "$RABBITMQ_PATH.tar.xz.asc" "$RABBITMQ_SOURCE_URL.asc" ; wget --output-document "$RABBITMQ_PATH.tar.xz" "$RABBITMQ_SOURCE_URL" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver "$PGP_KEYSERVER" --recv-keys "$RABBITMQ_PGP_KEY_ID" ; gpg --batch --verify "$RABBITMQ_PATH.tar.xz.asc" "$RABBITMQ_PATH.tar.xz" ; gpgconf --kill all ; rm -rf "$GNUPGHOME" ; mkdir -p "$RABBITMQ_HOME" ; tar --extract --file "$RABBITMQ_PATH.tar.xz" --directory "$RABBITMQ_HOME" --strip-components 1 ; rm -rf "$RABBITMQ_PATH"* ; grep -qE '^SYS_PREFIX=\$\{RABBITMQ_HOME\}$' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; sed -i 's/^SYS_PREFIX=.*$/SYS_PREFIX=/' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; grep -qE '^SYS_PREFIX=$' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; chown -R rabbitmq:rabbitmq "$RABBITMQ_HOME" ; apk del .build-deps ; [ ! -e "$RABBITMQ_DATA_DIR/.erlang.cookie" ] ; su-exec rabbitmq rabbitmqctl help ; su-exec rabbitmq rabbitmqctl list_ciphers ; su-exec rabbitmq rabbitmq-plugins list ; rm "$RABBITMQ_DATA_DIR/.erlang.cookie"
#   Added for backwards compatibility - users can simply COPY custom plugins to /plugins
RUN ln -sf /opt/rabbitmq/plugins /plugins
#   set home so that any `--user` knows where to put the erlang cookie
ENV HOME="$RABBITMQ_DATA_DIR"
#   Hint that the data (a.k.a. home dir) dir should be separate volume
VOLUME $RABBITMQ_DATA_DIR
#   warning: the VM is running with native name encoding of latin1 which may cause Elixir to malfunction as it expects utf8. Please ensure your locale is set to UTF-8 (which can be verified by running "locale" in your shell)
#   Setting all environment variables that control language preferences, behaviour differs - https://www.gnu.org/software/gettext/manual/html_node/The-LANGUAGE-variable.html#The-LANGUAGE-variable
#   https://docs.docker.com/samples/library/ubuntu/#locales
ENV LANG="C.UTF-8" \
    LANGUAGE="C.UTF-8" \
    LC_ALL="C.UTF-8"
COPY docker-entrypoint.sh /usr/local/bin/
ENTRYPOINT ["docker-entrypoint.sh"]
EXPOSE 4369/tcp 5671/tcp 5672/tcp 25672/tcp
CMD ["rabbitmq-server"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
