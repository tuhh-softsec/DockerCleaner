ARG ALPINE_VERSION=3.9.4
#   mod_perl is only available in Alpine edge as a @testing package.
#   Unfortunately, as the edge version can have another version of Perl
#   than the stable version of Alpine Linux. In that case, mod_perl won't load.
#   To work around this, we compile mod_perl on our own. This should be removed once
#   apache2-mod-perl-dev gets moved to the community or the main repository.
FROM alpine:${ALPINE_VERSION} AS build_modperl
RUN set -x \
 && apk add alpine-sdk coreutils cmake --no-cache \
 && adduser -G abuild -g "Alpine Package Builder" -s /bin/ash -D builder \
 && echo "builder ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers \
 && mkdir /packages \
 && chown builder:abuild /packages
USER builder
RUN set -x \
 && cd /packages \
 && git clone --depth=10000 https://git.alpinelinux.org/aports \
 && cd aports/testing/apache2-mod-perl \
 && abuild-keygen -a -i \
 && abuild -R \
 && echo $( sed -E -n 's/^pkgver=(.+)$/\1/p' /packages/aports/testing/apache2-mod-perl/APKBUILD ;)-r$( sed -E -n 's/^pkgrel=(.+)$/\1/p' /packages/aports/testing/apache2-mod-perl/APKBUILD ;) > /home/builder/packages/testing/x86_64/ver.txt
#   Base stage that adds the Perl runtime and some build utilities to httpd:alpine
FROM alpine:${ALPINE_VERSION} AS modperl
COPY --from=build_modperl /home/builder/packages/testing/x86_64 /packages
RUN set -x \
 && echo -e '@edge http://dl-cdn.alpinelinux.org/alpine/edge/main\n@edgecommunity http://dl-cdn.alpinelinux.org/alpine/edge/community\n@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing' >> /etc/apk/repositories \
 && apk add tzdata git curl wget perl perl-dev openssl openssl-dev make gcc libc-dev zlib-dev -U --no-cache \
 && apk add /packages/apache2-mod-perl-$( cat /packages/ver.txt ;).apk --allow-untrusted \
 && rm -rf /var/cache/apk/* /packages/
#   Install cpm to install cpanfile dependencies
RUN curl -o- -L --compressed https://git.io/cpm | perl - install App::cpm -g \
 && rm -rf ~/.perl-cpm
#   Stage that just adds some runtime packages that will be used in the runnable stage and the builder stage
FROM modperl AS alpinemodperl
RUN set -x \
 && apk add imagemagick6 graphviz tesseract-ocr imagemagick zbar@testing --update --no-cache
#   Stage for installing/compiling cpanfile dependencies
FROM alpinemodperl AS builder
COPY --from=build_modperl /home/builder/packages/testing/x86_64 /packages
RUN apk add alpine-sdk imagemagick6 imagemagick6-dev graphviz graphviz-dev tesseract-ocr tesseract-ocr-dev imagemagick zbar@testing zbar-dev@testing apache2-dev --update --no-cache \
 && apk add /packages/apache2-mod-perl-dev-$( cat /packages/ver.txt ;).apk --allow-untrusted \
 && wget https://github.com/mchehab/zbar/archive/0.22.2.tar.gz \
 && tar xfz 0.22.2.tar.gz \
 && rm 0.22.2.tar.gz \
 && cd zbar-0.22.2/perl \
 && perl Makefile.PL \
 && ln -s MYMETA.yml META.yml \
 && cpm install -L /tmp/local . \
 && cd ../.. \
 && rm -rf zbar-0.22.2
WORKDIR /tmp
#   Dependency of libapreq2-2.13, which is not installed automatically.
RUN cpm install ExtUtils::XSBuilder::ParseSource
#   Install Product Opener from the workdir.
COPY ./cpanfile /tmp/cpanfile
#   Add ProductOpener runtime dependencies from cpan
RUN cpm install --without-test
#   Separate stage/layer that can be used to run unit tests
FROM builder AS tester
RUN cpm install --with-test
#   Helper stage, so that we don't try to install GeoIP upon every rebuild of the source
FROM alpinemodperl AS alpinemodperlgeoip
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /tmp/GeoLite2-Country.tar.gz https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.tar.gz
RUN set -x \
 && tar xfz /tmp/GeoLite2-Country.tar.gz -C /usr/local/share \
 && rm /tmp/GeoLite2-Country.tar.gz \
 && mv /usr/local/share/GeoLite2-Country_* /usr/local/share/GeoLite2-Country \
 && mkdir -p /usr/local/apache2/conf/sites-enabled \
 && echo 'IncludeOptional conf/sites-enabled/*.conf' >> /usr/local/apache2/conf/httpd.conf
FROM alpinemodperlgeoip AS runnable
#   Copy Perl libraries from the builder image
COPY --from=builder /tmp/local/ /opt/perl/local/
EXPOSE 80/tcp
FROM runnable AS withsrc
#   Install Product Opener from the workdir
COPY . /opt/product-opener/
WORKDIR /opt/product-opener
# Please add your HEALTHCHECK here!!!
