FROM technosoft2000/alpine-base:3.9-1
MAINTAINER Technosoft2000 <technosoft2000@gmx.net>
LABEL image.version="1.3.2" \
      image.description="Docker image for Calibre Web, based on docker image of Alpine" \
      image.date="2019-04-18" \
      url.docker="https://hub.docker.com/r/technosoft2000/calibre-web" \
      url.github="https://github.com/Technosoft2000/docker-calibre-web" \
      url.support="https://cytec.us/forum"
#  Set basic environment settings
ENV VERSION="1.3.2" \
    LANG="en_US.UTF-8" \
    LANGUAGE="en_US.UTF-8" \
    LC_ALL="en_US.UTF-8" \
    PUSER="calibre" \
    PGROUP="calibre" \
    APP_NAME="Calibre-Web" \
    APP_HOME="/calibre-web" \
    APP_REPO="https://github.com/janeczku/calibre-web.git" \
    APP_BRANCH="master" \
    AMAZON_KG_TAR="kindlegen_linux_2.6_i386_v2_9.tar.gz" \
    AMAZON_KG_URL="http://kindlegen.s3.amazonaws.com/kindlegen_linux_2.6_i386_v2_9.tar.gz" \
    CALIBRE_PATH="/books" \
    PKG_DEV="build-base python-dev openssl-dev libffi-dev libxml2-dev libxslt-dev" \
    PKG_PYTHON="ca-certificates py-pip python py-libxml2 py-libxslt py-lxml libev" \
    PKG_IMAGES_DEV="curl file fontconfig-dev freetype-dev lcms2-dev  libjpeg-turbo-dev libpng-dev libtool libwebp-dev perl-dev tiff-dev xz zlib-dev" \
    PKG_IMAGES="fontconfig freetype lcms2 libjpeg-turbo libltdl libpng  libwebp libxml2 tiff zlib" \
    PKG_GS_DEV="libjpeg-turbo-dev libpng-dev jasper-dev expat-dev  zlib-dev tiff-dev freetype-dev lcms2-dev gtk+3.0-dev  cups-dev libtool jbig2dec-dev openjpeg-dev" \
    PKG_GS="jasper expat jbig2dec openjpeg" \
    MAGICK_HOME="/usr"
#  Install GNU libc (aka glibc)
#  https://github.com/sgerrand/alpine-pkg-glibc
COPY LOCALE.md /init/
RUN ALPINE_GLIBC_BASE_URL="https://github.com/sgerrand/alpine-pkg-glibc/releases/download" \
 && ALPINE_GLIBC_PACKAGE_VERSION="2.29-r0" \
 && ALPINE_GLIBC_BASE_PACKAGE_FILENAME="glibc-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_BIN_PACKAGE_FILENAME="glibc-bin-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && ALPINE_GLIBC_I18N_PACKAGE_FILENAME="glibc-i18n-$ALPINE_GLIBC_PACKAGE_VERSION.apk" \
 && mkdir -p /tmp \
 && mkdir -p /var/cache/apk \
 && apk add --no-cache --virtual=.build-dependencies wget ca-certificates \
 && apk add --no-cache parallel \
 && wget "https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub" -O "/etc/apk/keys/sgerrand.rsa.pub" \
 && wget "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" \
 && apk add --no-cache "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" \
 && cat /init/LOCALE.md | parallel "echo generate locale {}; /usr/glibc-compat/bin/localedef --force --inputfile {} --charmap UTF-8 {}.UTF-8;" \
 && apk del .build-dependencies \
 && rm "/etc/apk/keys/sgerrand.rsa.pub" \
 && rm "/root/.wget-hsts" \
 && rm "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME"
COPY ghostscript /init/ghostscript/
RUN echo "--- Update the package list ------------------------------------------------" \
 && apk -U upgrade \
 && echo "--- Install applications via package manager -------------------------------" \
 && apk -U add --no-cache $PKG_DEV $PKG_PYTHON $PKG_IMAGES_DEV $PKG_IMAGES $PKG_GS_DEV $PKG_GS \
 && echo "--- Upgrade pip to the latest version --------------------------------------" \
 && pip install pip --upgrade \
 && echo "--- Install python packages via pip ----------------------------------------" \
 && pip install setuptools pyopenssl Babel Flask-Babel Flask-Login Flask-Principal singledispatch backports_abc Flask iso-639 PyPDF2 pytz requests SQLAlchemy tornado Wand unidecode Pillow google-api-python-client gevent greenlet httplib2 oauth2client uritemplate pyasn1-modules pyasn1 PyDrive PyYAML rsa six goodreads python-Levenshtein lxml Flask-Dance --no-cache-dir --upgrade \
 && echo "--- Get Ghostscript 9.26 and build it --------------------------------------" \
 && mkdir -p /tmp/ghostscript \
 && curl -o /tmp/ghostscript-src.tar.gz -L "https://github.com/ArtifexSoftware/ghostpdl-downloads/releases/download/gs926/ghostscript-9.26.tar.gz" \
 && tar xf /tmp/ghostscript-src.tar.gz -C /tmp/ghostscript --strip-components=1 \
 && cp /init/ghostscript/* /tmp/ghostscript \
 && cd /tmp/ghostscript \
 && patch cups/gdevcups.c fix-sprintf.patch \
 && patch base/gdevsclass.c fix-put_image-methode.patch \
 && patch base/stdio_.h fix-stdio.patch \
 && patch base/lib.mak ghostscript-system-zlib.patch \
 && ./configure \
 && make so all \
 && make soinstall \
 && make install \
 && echo "--- Get ImageMagic 6 and build it ------------------------------------------" \
 && IMAGEMAGICK_VER=$( curl --silent http://www.imagemagick.org/download/digest.rdf | grep ImageMagick-6.*tar.xz | sed 's/\(.*\).tar.*/\1/' | sed 's/^.*ImageMagick-/ImageMagick-/' ;) \
 && mkdir -p /tmp/imagemagick \
 && curl -o /tmp/imagemagick-src.tar.xz -L "http://www.imagemagick.org/download/${IMAGEMAGICK_VER}.tar.xz" \
 && tar xf /tmp/imagemagick-src.tar.xz -C /tmp/imagemagick --strip-components=1 \
 && cd /tmp/imagemagick \
 && sed -i -e 's:DOCUMENTATION_PATH="${DATA_DIR}/doc/${DOCUMENTATION_RELATIVE_PATH}":DOCUMENTATION_PATH="/usr/share/doc/imagemagick":g' configure \
 && ./configure --infodir=/usr/share/info --mandir=/usr/share/man --prefix=/usr --sysconfdir=/etc --with-gs-font-dir=/usr/share/fonts/Type1 --with-gslib --with-lcms2 --with-modules --without-threads --without-x --with-tiff --with-xml \
 && make \
 && make install \
 && find / -name '.packlist' -o -name 'perllocal.pod' -o -name '*.bs' -delete \
 && rm -rf /tmp/*
#  Install calibre binary
#  enhancement from jim3ma/docker-calibre-web
#  needed for calibre ebook-convert command line tool
#  https://github.com/jim3ma/docker-calibre-web
#  https://manual.calibre-ebook.com/generated/en/ebook-convert.html
ENV LD_LIBRARY_PATH="/usr/lib:/opt/calibre/lib" \
    PATH="$PATH:/opt/calibre" \
    LC_ALL="C" \
    CALIBRE_INSTALLER_SOURCE_CODE_URL="https://raw.githubusercontent.com/kovidgoyal/calibre/master/setup/linux-installer.py"
RUN apk update \
 && apk add --no-cache --upgrade bash ca-certificates gcc libxcomposite mesa-gl python qt5-qtbase-x11 xdg-utils xz wget \
 && wget -O- ${CALIBRE_INSTALLER_SOURCE_CODE_URL} | python -c "import sys; main=lambda:sys.stderr.write('Download failed\n'); exec(sys.stdin.read()); main(install_dir='/opt', isolated=True)" \
 && rm -rf /tmp/calibre-installer-cache \
 && apk del --purge $PKG_DEV $PKG_IMAGES_DEV \
 && mkdir -p $APP_HOME/app \
 && rm -rf /tmp \
 && rm -rf /var/cache/apk/*
#  set the working directory for the APP
WORKDIR $APP_HOME/app
#  copy files to the image (info.txt and scripts)
COPY *.txt /init/
COPY *.sh /init/
#  copy Calibre related files (e.g. metadata.db)
COPY calibre-init /init/calibre-init
#  copy patched version of ImageMagick policy.xml
COPY imagemagick/policy.xml /etc/ImageMagick-6
#  Set volumes for the Calibre Web folder structure
VOLUME /books
VOLUME $APP_HOME/app
VOLUME $APP_HOME/config
VOLUME $APP_HOME/kindlegen
#  Expose ports
EXPOSE 8083/tcp
