FROM ruby:2.6.3-alpine AS builder
ENV PREFIX="/usr/local"
#   Runtime dependencies
RUN apk add cairo=1.16.0-r3 freetype=2.10.0-r1 fribidi=1.0.5-r2 glib=2.60.4-r0 graphite2=1.3.13-r1 icu-libs=64.2-r1 libbz2=1.0.6-r7 libgcc=8.3.0-r0 libltdl=2.4.6-r6 libgomp=8.3.0-r0 pngquant=2.12.3-r0 sudo=1.9.5p2-r0 tini=0.18.0-r0
#   Build dependencies
RUN apk add autoconf=2.69-r2 automake=1.16.1-r0 build-base=0.5-r1 bzip2-dev=1.0.6-r7 cairo-dev=1.16.0-r3 fribidi-dev=1.0.5-r2 freetype-dev=2.10.0-r1 ghostscript-dev=9.27-r5 glib-dev=2.60.4-r0 gobject-introspection-dev=1.60.2-r0 graphite2-dev=1.3.13-r1 icu-dev=64.2-r1 libtool=2.4.6-r6
ENV JEMALLOC_VERSION="3.6.0"
RUN mkdir /tmp/jemalloc \
 && cd /tmp/jemalloc \
 && wget -O jemalloc.tar.bz2 https://github.com/jemalloc/jemalloc/releases/download/$JEMALLOC_VERSION/jemalloc-$JEMALLOC_VERSION.tar.bz2 \
 && tar xjf jemalloc.tar.bz2 \
 && cd jemalloc-$JEMALLOC_VERSION \
 && ./configure \
 && make -j \
 && mv lib/libjemalloc.so.1 /usr/lib \
 && rm -rf /tmp/jemalloc
ENV HARFBUZZ_VERSION="2.4.0"
RUN mkdir /tmp/harfbuzz \
 && cd /tmp/harfbuzz \
 && wget -O harfbuzz.tar.bz2 https://www.freedesktop.org/software/harfbuzz/release/harfbuzz-$HARFBUZZ_VERSION.tar.bz2 \
 && tar xjf harfbuzz.tar.bz2 \
 && cd harfbuzz-$HARFBUZZ_VERSION \
 && ./configure --prefix=$PREFIX --with-glib --with-gobject --with-graphite2 --with-icu \
 && make -j all \
 && make -j install \
 && rm -rf /tmp/harfbuzz
ENV RAQM_VERSION="0.5.0"
RUN mkdir /tmp/raqm \
 && cd /tmp/raqm \
 && wget -O raqm.tar.gz https://github.com/HOST-Oman/libraqm/releases/download/v$RAQM_VERSION/raqm-$RAQM_VERSION.tar.gz \
 && tar xzf raqm.tar.gz \
 && cd raqm-$RAQM_VERSION \
 && ./configure --prefix=$PREFIX \
 && make -j all \
 && make -j install \
 && rm -rf /tmp/raqm
ENV LIBPNG_VERSION="1.6.37"
RUN mkdir /tmp/libpng \
 && cd /tmp/libpng \
 && wget -O libpng.tar.gz https://prdownloads.sourceforge.net/libpng/libpng-$LIBPNG_VERSION.tar.gz?downlolad \
 && tar xzf libpng.tar.gz \
 && cd libpng-$LIBPNG_VERSION \
 && ./configure --prefix=$PREFIX \
 && make -j all \
 && make -j install \
 && rm -rf /tmp/libpng
ENV IMAGE_MAGICK_VERSION="7.0.8-42"
RUN mkdir /tmp/imagemagick \
 && cd /tmp/imagemagick \
 && wget -O ImageMagick.tar.gz https://github.com/ImageMagick/ImageMagick/archive/$IMAGE_MAGICK_VERSION.tar.gz \
 && tar xzf ImageMagick.tar.gz \
 && cd ImageMagick-${IMAGE_MAGICK_VERSION} \
 && ./configure --prefix=$PREFIX --enable-static --enable-bounds-checking --enable-hugepages --with-modules --without-magick-plus-plus \
 && make -j all \
 && make -j install \
 && rm -rf /tmp/imagemagick
COPY policy.xml /usr/local/etc/ImageMagick-7/
COPY Gemfile /var/www/letter-avatars/Gemfile
COPY Gemfile.lock /var/www/letter-avatars/Gemfile.lock
COPY fonts/Roboto-Medium /var/www/letter-avatars/Roboto-Medium
COPY fonts/NotoSansDisplay-Medium.ttf /var/www/letter-avatars/NotoSansDisplay-Medium.ttf
COPY fonts/NotoSansMono-Medium.ttf /var/www/letter-avatars/NotoSansMono-Medium.ttf
COPY fonts/NotoSansMonoCJKsc-Regular.otf /var/www/letter-avatars/NotoSansMonoCJKsc-Regular.otf
COPY fonts/NotoSansArabic-Medium.ttf /var/www/letter-avatars/NotoSansArabic-Medium.ttf
COPY fonts/NotoSansDevanagari-Medium.ttf /var/www/letter-avatars/NotoSansDevanagari-Medium.ttf
COPY fonts/NotoSansBengali-Medium.ttf /var/www/letter-avatars/NotoSansBengali-Medium.ttf
COPY fonts/NotoSansJavanese-Regular.ttf /var/www/letter-avatars/NotoSansJavanese-Regular.ttf
COPY fonts/NotoSansTelugu-Regular.ttf /var/www/letter-avatars/NotoSansTelugu-Regular.ttf
COPY fonts/NotoSansThai-Medium.ttf /var/www/letter-avatars/NotoSansThai-Medium.ttf
COPY fonts/NotoSansHebrew-Medium.ttf /var/www/letter-avatars/NotoSansHebrew-Medium.ttf
COPY fonts/NotoSansArmenian-Medium.ttf /var/www/letter-avatars/NotoSansArmenian-Medium.ttf
RUN adduser -s /bin/bash -u 9001 -D web \
 && cd /var/www/letter-avatars \
 && chown -R web . \
 && sudo -E -u web bundle install --deployment --verbose
RUN apk del autoconf automake build-base bzip2-dev cairo-dev fribidi-dev freetype-dev ghostscript-dev glib-dev gobject-introspection-dev graphite2-dev icu-dev libtool \
 && rm -rf /var/cache/apk/*
FROM builder
COPY config.ru /var/www/letter-avatars/config.ru
COPY lib /var/www/letter-avatars/lib
COPY unicorn.conf.rb /var/www/letter-avatars/unicorn.conf.rb
ENTRYPOINT ["/sbin/tini", "--", "sudo", "-E", "-u", "web", "/bin/sh", "-c", "cd", "/var/www/letter-avatars", "&&", "exec", "bundle", "exec", "unicorn", "-E", "production", "-c", "/var/www/letter-avatars/unicorn.conf.rb"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
