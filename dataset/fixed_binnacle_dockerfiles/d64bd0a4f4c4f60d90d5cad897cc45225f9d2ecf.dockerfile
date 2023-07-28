#  !/usr/bin/docker build --force-rm --rm -f
FROM alpine:3.6
MAINTAINER Achim <achim@owasp.org>
LABEL VERSION="17.06.17" \
      DESCRIPTION="Build O-Saft docker image (with Peter Mosman's openssl)" \
      SYNOPSIS="docker build --force-rm --rm -f ./Dockerfile -t owasp/o-saft:17.06.17 -t owasp/o-saft ." \
      DETAILS="Please see https://github.com/OWASP/O-Saft/raw/master/o-saft-docker" \
      SOURCE0="https://github.com/OWASP/O-Saft/raw/master/Dockerfile" \
      SOURCE1="https://github.com/OWASP/O-Saft/raw/master/o-saft.tgz" \
      SOURCE2="https://github.com/PeterMosmans/openssl/archive/1.0.2-chacha.tar.gz" \
      SOURCE3="http://search.cpan.org/CPAN/authors/id/S/SU/SULLR/IO-Socket-SSL-2.049.tar.gz" \
      SID="@(#) Dockerfile.alpine:3.6 1.1 17/07/25 23:01:19" \
      AUTHOR="Achim Hoffmann"
ENV o-saft-docker-build="\"Dockerfile 17.06.17\"                                     "
ENV OSAFT_DIR="/O-Saft"
ENV OPENSSL_DIR="/openssl"
ENV OPENSSL_VERSION="1.0.2-chacha"
ENV TERM="xterm"
ENV PATH="${OSAFT_DIR}:${OSAFT_DIR}/contrib:${OPENSSL_DIR}/bin:$PATH"
#   Install required packages
RUN apk update \
 && apk add wget=1.20.3-r0 perl=5.24.4-r2 perl-net-dns=1.10-r0 perl-net-ssleay=1.81-r1 ncurses=6.0_p20171125-r1 --no-cache
WORKDIR /
#   Install O-Saft
RUN mkdir $OSAFT_DIR \
 && adduser -D -h ${OSAFT_DIR} osaft \
 && wget --no-check-certificate https://github.com/OWASP/O-Saft/raw/master/o-saft.tgz -O o-saft.tgz \
 && echo "a55702d69314b8eda52e921e35b89aef9eef02b14c870b3077fbddf3af91320d o-saft.tgz" | sha256sum -c \
 && tar -xzf o-saft.tgz \
 && chown -R root:root $OSAFT_DIR \
 && chown -R osaft:osaft $OSAFT_DIR/contrib \
 && chown osaft:osaft $OSAFT_DIR/.o-saft.pl \
 && mv $OSAFT_DIR/.o-saft.pl $OSAFT_DIR/.o-saft.pl-orig \
 && sed -e "s:^#--openssl=.*:--openssl=$OPENSSL_DIR/bin/openssl:" < $OSAFT_DIR/.o-saft.pl-orig > $OSAFT_DIR/.o-saft.pl \
 && chmod 666 $OSAFT_DIR/.o-saft.pl \
 && rm -f o-saft.tgz
#   Pull and build IO::Socket::SSL (July/2017: missing in alpine's perl)
RUN mkdir /src_iosocket \
 && wget --no-check-certificate http://search.cpan.org/CPAN/authors/id/S/SU/SULLR/IO-Socket-SSL-2.049.tar.gz -O iosocket.tgz \
 && tar -xzf iosocket.tgz -C /src_iosocket --strip-components=1 \
 && cd /src_iosocket \
 && apk add make=4.2.1-r0 --no-cache \
 && echo n | perl Makefile.PL \
 && make \
 && make test \
 && make install \
 && apk del --purge make \
 && cd / \
 && rm -r /src_iosocket iosocket.tgz
#   Pull and build enhanced openssl
RUN mkdir $OPENSSL_DIR /src_openssl \
 && wget --no-check-certificate https://github.com/PeterMosmans/openssl/archive/1.0.2-chacha.tar.gz -O openssl.tgz \
 && tar -xzf openssl.tgz -C /src_openssl --strip-components=1 \
 && cd /src_openssl \
 && apk add musl-dev=1.1.16-r15 gcc=6.3.0-r4 make=4.2.1-r0 zlib-dev=1.2.11-r0 --no-cache \
 && ./config --prefix=$OPENSSL_DIR --openssldir=$OPENSSL_DIR/ssl enable-zlib enable-ssl3 enable-rc5 enable-rc2 enable-GOST enable-cms enable-md2 enable-mdc2 enable-ec enable-ec2m enable-ecdh enable-ecdsa enable-seed enable-idea enable-camellia enable-rfc3779 enable-ec_nistp_64_gcc_128 -static experimental-jpake -DOPENSSL_USE_BUILD_DATE \
 && make depend \
 && make \
 && make report \
 && make install \
 && echo -e "# number of ciphers $OPENSSL_DIR/bin/openssl: " \
 && $OPENSSL_DIR/bin/openssl ciphers -V ALL:COMPLEMENTOFALL:aNULL | wc -l \
 && apk del --purge musl-dev gcc make zlib-dev \
 && cd / \
 && rm -r /src_openssl openssl.tgz
#   Install traditional openssl
#   RUN apk add --no-cache openssl
#   Install Tcl/Tk support
#   RUN apk add --no-cache tcl tk xvfb
WORKDIR $OSAFT_DIR
USER osaft
#  RUN     o-saft-docker usage
ENTRYPOINT ["perl", "/O-Saft/o-saft.pl"]
CMD ["--help=docker"]
#   vim:set ft=dockerfile:
# Please add your HEALTHCHECK here!!!