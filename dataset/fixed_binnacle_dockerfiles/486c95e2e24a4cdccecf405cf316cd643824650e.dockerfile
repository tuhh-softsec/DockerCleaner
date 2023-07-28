#   The official Canonical Ubuntu Bionic image is ideal from a security perspective,
#   especially for the enterprises that we, the RabbitMQ team, have to deal with
FROM ubuntu:18.04
RUN set -eux ; apt-get update ; apt-get install --no-install-recommends gosu=1.10-1ubuntu0.18.04.1 -y ; rm -rf /var/lib/apt/lists/* ; gosu nobody true
#   Default to a PGP keyserver that pgp-happy-eyeballs recognizes, but allow for substitutions locally
ARG PGP_KEYSERVER=ha.pool.sks-keyservers.net
#   If you are building this image locally and are getting `gpg: keyserver receive failed: No data` errors,
#   run the build with a different PGP_KEYSERVER, e.g. docker build --tag rabbitmq:3.7 --build-arg PGP_KEYSERVER=pgpkeys.eu 3.7/ubuntu
#   For context, see https://github.com/docker-library/official-images/issues/4252
#   Using the latest OpenSSL LTS release, with support until September 2023 - https://www.openssl.org/source/
ENV OPENSSL_VERSION="%%OPENSSL_VERSION%%"
ENV OPENSSL_SOURCE_SHA256="%%OPENSSL_SOURCE_SHA256%%"
#   https://www.openssl.org/community/omc.html
ENV OPENSSL_PGP_KEY_IDS="%%OPENSSL_PGP_KEY_IDS%%"
#   Use the latest stable Erlang/OTP release (https://github.com/erlang/otp/tags)
ENV OTP_VERSION="%%OTP_VERSION%%"
#   TODO add PGP checking when the feature will be added to Erlang/OTP's build system
#   http://erlang.org/pipermail/erlang-questions/2019-January/097067.html
ENV OTP_SOURCE_SHA256="%%OTP_SOURCE_SHA256%%"
#   Install dependencies required to build Erlang/OTP from source
#   http://erlang.org/doc/installation_guide/INSTALL.html
#   autoconf: Required to configure Erlang/OTP before compiling
#   dpkg-dev: Required to set up host & build type when compiling Erlang/OTP
#   gnupg: Required to verify OpenSSL artefacts
#   libncurses5-dev: Required for Erlang/OTP new shell & observer_cli - https://github.com/zhongwencool/observer_cli
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends autoconf=2.69-11 ca-certificates=20211016ubuntu0.18.04.1 dpkg-dev=1.19.0.5ubuntu2.4 gcc=4:7.4.0-1ubuntu2.3 gnupg=2.2.4-1ubuntu1.6 libncurses5-dev=6.1-1ubuntu1.18.04 make=4.1-9.1ubuntu1 wget=1.19.4-1ubuntu2.2 --yes ; rm -rf /var/lib/apt/lists/* ; OPENSSL_SOURCE_URL="https://www.openssl.org/source/openssl-$OPENSSL_VERSION.tar.gz" ; OPENSSL_PATH="/usr/local/src/openssl-$OPENSSL_VERSION" ; OPENSSL_CONFIG_DIR=/usr/local/etc/ssl ; wget --progress dot:giga --output-document "$OPENSSL_PATH.tar.gz.asc" "$OPENSSL_SOURCE_URL.asc" ; wget --progress dot:giga --output-document "$OPENSSL_PATH.tar.gz" "$OPENSSL_SOURCE_URL" ; export GNUPGHOME="$( mktemp -d ;)" ; for key in $OPENSSL_PGP_KEY_IDS; do gpg --batch --keyserver "$PGP_KEYSERVER" --recv-keys "$key" ; done ; gpg --batch --verify "$OPENSSL_PATH.tar.gz.asc" "$OPENSSL_PATH.tar.gz" ; gpgconf --kill all ; rm -rf "$GNUPGHOME" ; echo "$OPENSSL_SOURCE_SHA256 *$OPENSSL_PATH.tar.gz" | sha256sum --check --strict - ; mkdir -p "$OPENSSL_PATH" ; tar --extract --file "$OPENSSL_PATH.tar.gz" --directory "$OPENSSL_PATH" --strip-components 1 ; cd "$OPENSSL_PATH" ; debMultiarch="$( dpkg-architecture --query DEB_HOST_MULTIARCH ;)" ; MACHINE="$( dpkg-architecture --query DEB_BUILD_GNU_CPU ;)" RELEASE="4.x.y-z" SYSTEM='Linux' BUILD='???' ./config --openssldir="$OPENSSL_CONFIG_DIR" --libdir="lib/$debMultiarch" ; make -j "$( getconf _NPROCESSORS_ONLN ;)" ; make install_sw install_ssldirs ; cd .. ; rm -rf "$OPENSSL_PATH"* ; echo '/usr/local/lib' > /etc/ld.so.conf.d/000-openssl-libc.conf; ldconfig ; rmdir "$OPENSSL_CONFIG_DIR/certs" "$OPENSSL_CONFIG_DIR/private" ; ln -sf /etc/ssl/certs /etc/ssl/private "$OPENSSL_CONFIG_DIR" ; openssl version ; OTP_SOURCE_URL="https://github.com/erlang/otp/archive/OTP-$OTP_VERSION.tar.gz" ; OTP_PATH="/usr/local/src/otp-$OTP_VERSION" ; mkdir -p "$OTP_PATH" ; wget --progress dot:giga --output-document "$OTP_PATH.tar.gz" "$OTP_SOURCE_URL" ; echo "$OTP_SOURCE_SHA256 *$OTP_PATH.tar.gz" | sha256sum --check --strict - ; tar --extract --file "$OTP_PATH.tar.gz" --directory "$OTP_PATH" --strip-components 1 ; cd "$OTP_PATH" ; export ERL_TOP="$OTP_PATH" ; ./otp_build autoconf ; CFLAGS="$( dpkg-buildflags --get CFLAGS ;)" ; export CFLAGS ; hostArch="$( dpkg-architecture --query DEB_HOST_GNU_TYPE ;)" ; buildArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" ; dpkgArch="$( dpkg --print-architecture ;)" ; dpkgArch="${dpkgArch##*-}" ; ./configure --host="$hostArch" --build="$buildArch" --disable-dynamic-ssl-lib --disable-hipe --disable-sctp --disable-silent-rules --enable-clock-gettime --enable-hybrid-heap --enable-kernel-poll --enable-shared-zlib --enable-smp-support --enable-threads --with-microstate-accounting=extra --without-common_test --without-debugger --without-dialyzer --without-diameter --without-edoc --without-erl_docgen --without-erl_interface --without-et --without-eunit --without-ftp --without-hipe --without-jinterface --without-megaco --without-observer --without-odbc --without-reltool --without-ssh --without-tftp --without-wx ; make -j "$( getconf _NPROCESSORS_ONLN ;)" GEN_OPT_FLGS="-O2 -fno-strict-aliasing" ; make install ; cd .. ; rm -rf "$OTP_PATH"* /usr/local/lib/erlang/lib/*/examples /usr/local/lib/erlang/lib/*/src ; apt-mark auto '.*' > /dev/null; [ -z "$savedAptMark" ] || apt-mark manual $savedAptMark ; find /usr/local -type f -executable -exec ldd '{}' ';' | awk '/=>/ { print $(NF-1) }' | sort -u | xargs -r dpkg-query --search | cut -d: -f1 | sort -u | xargs -r apt-mark manual ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; openssl version ; erl -noshell -eval 'io:format("~p~n~n~p~n~n", [crypto:supports(), ssl:versions()]), init:stop().'
ENV RABBITMQ_DATA_DIR="/var/lib/rabbitmq"
#   Create rabbitmq system user & group, fix permissions & allow root user to connect to the RabbitMQ Erlang VM
RUN set -eux ; groupadd --gid 999 --system rabbitmq ; useradd --uid 999 --system --home-dir "$RABBITMQ_DATA_DIR" --gid rabbitmq rabbitmq ; mkdir -p "$RABBITMQ_DATA_DIR" /etc/rabbitmq /tmp/rabbitmq-ssl /var/log/rabbitmq ; chown -fR rabbitmq:rabbitmq "$RABBITMQ_DATA_DIR" /etc/rabbitmq /tmp/rabbitmq-ssl /var/log/rabbitmq ; chmod 777 "$RABBITMQ_DATA_DIR" /etc/rabbitmq /tmp/rabbitmq-ssl /var/log/rabbitmq ; ln -sf "$RABBITMQ_DATA_DIR/.erlang.cookie" /root/.erlang.cookie
#   Use the latest stable RabbitMQ release (https://www.rabbitmq.com/download.html)
ENV RABBITMQ_VERSION="%%RABBITMQ_VERSION%%"
#   https://www.rabbitmq.com/signatures.html#importing-gpg
ENV RABBITMQ_PGP_KEY_ID="0x0A9AF2115F4687BD29803A206B73A36E6026DFCA"
ENV RABBITMQ_HOME="/opt/rabbitmq"
#   Add RabbitMQ to PATH, send all logs to TTY
ENV PATH="$RABBITMQ_HOME/sbin:$PATH" \
    RABBITMQ_LOGS="-" \
    RABBITMQ_SASL_LOGS="-"
#   Install RabbitMQ
RUN set -eux ; savedAptMark="$( apt-mark showmanual ;)" ; apt-get update ; apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 gnupg=2.2.4-1ubuntu1.6 wget=1.19.4-1ubuntu2.2 xz-utils=5.2.2-1.3ubuntu0.1 --yes ; rm -rf /var/lib/apt/lists/* ; RABBITMQ_SOURCE_URL="https://github.com/rabbitmq/rabbitmq-server/releases/download/v$RABBITMQ_VERSION/rabbitmq-server-generic-unix-$RABBITMQ_VERSION.tar.xz" ; RABBITMQ_PATH="/usr/local/src/rabbitmq-$RABBITMQ_VERSION" ; wget --progress dot:giga --output-document "$RABBITMQ_PATH.tar.xz.asc" "$RABBITMQ_SOURCE_URL.asc" ; wget --progress dot:giga --output-document "$RABBITMQ_PATH.tar.xz" "$RABBITMQ_SOURCE_URL" ; export GNUPGHOME="$( mktemp -d ;)" ; gpg --batch --keyserver "$PGP_KEYSERVER" --recv-keys "$RABBITMQ_PGP_KEY_ID" ; gpg --batch --verify "$RABBITMQ_PATH.tar.xz.asc" "$RABBITMQ_PATH.tar.xz" ; gpgconf --kill all ; rm -rf "$GNUPGHOME" ; mkdir -p "$RABBITMQ_HOME" ; tar --extract --file "$RABBITMQ_PATH.tar.xz" --directory "$RABBITMQ_HOME" --strip-components 1 ; rm -rf "$RABBITMQ_PATH"* ; grep -qE '^SYS_PREFIX=\$\{RABBITMQ_HOME\}$' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; sed -i 's/^SYS_PREFIX=.*$/SYS_PREFIX=/' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; grep -qE '^SYS_PREFIX=$' "$RABBITMQ_HOME/sbin/rabbitmq-defaults" ; chown -R rabbitmq:rabbitmq "$RABBITMQ_HOME" ; apt-mark auto '.*' > /dev/null; apt-mark manual $savedAptMark ; apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false ; [ ! -e "$RABBITMQ_DATA_DIR/.erlang.cookie" ] ; gosu rabbitmq rabbitmqctl help ; gosu rabbitmq rabbitmqctl list_ciphers ; gosu rabbitmq rabbitmq-plugins list ; rm "$RABBITMQ_DATA_DIR/.erlang.cookie"
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
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
