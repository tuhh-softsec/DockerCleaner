FROM ubuntu:14.04
ENV PATH="\"$PATH:/opt/Fulcrum\""
ENV GITHUB_FEED="\"https://api.github.com/repos/fulcrumapp/fulcrum-desktop/releases/latest\""
#   System Dependencies
RUN apt-get update -y \
 && apt-get install --no-install-recommends apt-transport-https=1.0.1ubuntu2.24 curl=7.35.0-1ubuntu2.20 software-properties-common=0.92.37.8 python-software-properties=0.92.37.8 -y \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && add-apt-repository -y ppa:ubuntugis/ppa \
 && curl -sL https://deb.nodesource.com/setup_7.x | bash - \
 && apt-get update -y \
 && apt-get install --no-install-recommends libjson0=0.11-3ubuntu1.2 libjson0-dev=0.11-3ubuntu1.2 libsqlite3-dev=3.8.2-1ubuntu2.2 libproj-dev=4.8.0-2ubuntu2 libgeos-dev=3.4.2-4ubuntu1 libgeos++-dev=3.4.2-4ubuntu1 libspatialite-dev=4.1.1-5ubuntu1 libgeotiff-dev=1.4.0-1ubuntu2 libgdal-dev=1.10.1+dfsg-5ubuntu1 gdal-bin=1.10.1+dfsg-5ubuntu1 libmapnik-dev=2.2.0+ds1-6build2 mapnik-utils=2.2.0+ds1-6build2 python-dev=2.7.5-5ubuntu3 python-setuptools=3.3-1ubuntu2 python-pip=1.5.4-1ubuntu4 python-gdal=1.10.1+dfsg-5ubuntu1 python-mapnik=2.2.0+ds1-6build2 libprotobuf-dev=2.5.0-9ubuntu1 protobuf-compiler=2.5.0-9ubuntu1 nodejs=0.10.25~dfsg2-2ubuntu1.2 yarn gdebi-core=0.9.5.3ubuntu3 build-essential=11.6ubuntu6 libssl-dev=1.0.1f-1ubuntu2.27 libpq-dev=9.3.24-0ubuntu0.14.04 libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt1-dev=1.1.28-2ubuntu0.2 imagemagick=8:6.7.7.10-6ubuntu3.13 libmagickwand-dev=8:6.7.7.10-6ubuntu3.13 git=1:1.9.1-1ubuntu0.10 libyaml-dev=0.1.4-3ubuntu3.1 sqlite3=3.8.2-1ubuntu2.2 autoconf=2.69-6 libgmp-dev=2:5.1.3+dfsg-1ubuntu1 libgdbm-dev=1.8.3-12build1 libncurses5-dev=5.9+20140118-1ubuntu1 automake=1:1.14.1-2ubuntu1 make=3.81-8.2ubuntu3 bison=2:3.0.2.dfsg-2 flex=2.5.35-10.1ubuntu2 libtool=2.4.2-1.7ubuntu1 xz-utils=5.1.1alpha+20120614-2ubuntu2 libffi-dev=3.1~rc1+r3.0.13-12ubuntu0.2 libgmp-dev=2:5.1.3+dfsg-1ubuntu1 libreadline6-dev=6.3-4ubuntu2 postgresql-client=9.3+154ubuntu1.1 libx11-xcb1=2:1.6.2-1ubuntu2.1 -y
#   Install wkhtmltopdf
RUN apt-get install --no-install-recommends zip=3.0-8 xfonts-encodings=1:1.0.4-1ubuntu1 xfonts-utils=1:7.7+1 xfonts-base=1:1.0.3 xfonts-75dpi=1:1.0.3 -y
RUN apt-get install --no-install-recommends xfonts-intl-european=1.2.1-8 xfonts-intl-asian=1.2.1-8 xfonts-intl-arabic=1.2.1-8 xfonts-intl-chinese=1.2.1-8 xfonts-intl-chinese-big=1.2.1-8 xfonts-intl-japanese=1.2.1-8 xfonts-intl-japanese-big=1.2.1-8 xfonts-intl-phonetic=1.2.1-8 xfonts-wqy=0.9.9-8 xfonts-thai=1:1.2.6-2 fonts-khmeros-core=5.0-7ubuntu1 ttf-indic-fonts-core=1:0.5.14ubuntu1 ttf-indic-fonts=1:0.5.14ubuntu1 ttf-bengali-fonts=1:0.5.14ubuntu1 ttf-devanagari-fonts=1:0.5.14ubuntu1 ttf-gujarati-fonts=1:0.5.14ubuntu1 ttf-kannada-fonts=1:0.5.14ubuntu1 ttf-malayalam-fonts=1:0.5.14ubuntu1 ttf-oriya-fonts=1:0.5.14ubuntu1 ttf-punjabi-fonts=1:0.5.14ubuntu1 ttf-tamil-fonts=1:0.5.14ubuntu1 ttf-telugu-fonts=1:0.5.14ubuntu1 ttf-wqy-microhei=0.2.0-beta-2 fonts-wqy-microhei=0.2.0-beta-2 fonts-lao=0.0.20060226-9 fonts-droid=1:4.3-3ubuntu1.2 -y
ENV WKHTMLTOPDF_URL="\"http://zhm.s3.amazonaws.com/wkhtmltopdf/wkhtmltox-0.12.2.1_linux-trusty-amd64.deb\""
ENV WKHTMLTOPDF_PATH="/opt/wkhtmltopdf-build/wkhtmltopdf.deb"
RUN mkdir -p `dirname $WKHTMLTOPDF_PATH `
RUN curl $WKHTMLTOPDF_URL > $WKHTMLTOPDF_PATH
RUN fc-cache -f -v
RUN dpkg --install $WKHTMLTOPDF_PATH
RUN rm /opt/wkhtmltopdf-build/wkhtmltopdf.deb
RUN VERSION=$( curl -s "$GITHUB_FEED" | grep '"name":' | head -n 1 | sed -e '1s/ "name": "//' | sed -e '1s/",//' ;) \
 && DEB_FILE="Fulcrum_${VERSION}_amd64.deb" \
 && DOWNLOAD_URL="https://github.com/fulcrumapp/fulcrum-desktop/releases/download/v${VERSION}/${DEB_FILE}" \
 && curl -L $DOWNLOAD_URL -o /tmp/$DEB_FILE \
 && gdebi -n /tmp/$DEB_FILE \
 && rm /usr/local/bin/fulcrum \
 && ln -s /opt/Fulcrum/scripts/* /usr/local/bin \
 && rm /tmp/$DEB_FILE
CMD 'fulcrum'
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
