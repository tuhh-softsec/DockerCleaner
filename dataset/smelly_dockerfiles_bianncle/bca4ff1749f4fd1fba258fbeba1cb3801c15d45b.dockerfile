FROM boomtownroi/git:latest
MAINTAINER BoomTown CNS Team <consumerteam@boomtownroi.com>
ENV NODE_VERSION="9.11.2"
ENV YARN_VERSION="1.15.2"
ENV HUB_VERSION="2.2.9"
ENV PHANTOMJS_BIN="/usr/local/n/lib/node_modules/phantomjs-prebuilt/lib/phantom/bin/phantomjs"
RUN add-apt-repository -y ppa:nginx/stable &; 2> /dev/null
RUN apt-get update \
 && apt-get install build-essential curl dialog gawk jq imagemagick libfontconfig libssl-dev mysql-client php python rsync ssh-client wget zip php-fpm php-mysql php-curl php-gd php-mbstring php-intl php-pear php-imagick php-imap php-mcrypt php-memcached php-pspell php-recode php-tidy php-xmlrpc php-xsl php-xdebug -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#  Install node
#  (This has been copy/pasted from https://github.com/nodejs/docker-node/blob/947280600648b70e067d35415d6812fd03127def/8/Dockerfile
#  since we extend from a different base image.)
#  gpg keys listed at https://github.com/nodejs/node#release-keys
RUN set -ex \
 && for key in 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 B9AE9905FFD7803F25714661B63B535A4C206CA9 77984A986EBC2AA786BC0F66B01FBB92821C587A 8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600 4ED778F539E3634C779C87C6D7062848A1AB005C A48C2BEE680E841632CD4E44F07496B3EB3C1762 B9E2F5981AA6E0CD28160D9FF13993A75599653C; do gpg --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done
#  NODE_VERSION already defined
RUN ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='x64' ;;(ppc64el) ARCH='ppc64le' ;;(s390x) ARCH='s390x' ;;(arm64) ARCH='arm64' ;;(armhf) ARCH='armv7l' ;;(i386) ARCH='x86' ;;(*) echo "unsupported architecture" ; exit 1 ;; esac \
 && curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-$ARCH.tar.xz" \
 && curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION-linux-$ARCH.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xJf "node-v$NODE_VERSION-linux-$ARCH.tar.xz" -C /usr/local --strip-components=1 --no-same-owner \
 && rm "node-v$NODE_VERSION-linux-$ARCH.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt \
 && ln -s /usr/local/bin/node /usr/local/bin/nodejs
#  END copy/paste job
#  Install composer
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/bin --filename=composer
#  Install hub
RUN cd / \
 && wget https://github.com/github/hub/releases/download/v$HUB_VERSION/hub-linux-amd64-$HUB_VERSION.tgz \
 && tar -xvf hub-linux-amd64-$HUB_VERSION.tgz \
 && rm -f hub-linux-amd64-$HUB_VERSION.tgz \
 && cp hub-linux-amd64-$HUB_VERSION/bin/hub /bin/hub \
 && cp -r hub-linux-amd64-$HUB_VERSION/etc /etc \
 && rm -rf hub-linux-amd64-$HUB_VERSION
#  Make sure we clear npm's cache so it won't infest future builds
RUN npm install gulp@3.9.1 phantomjs-prebuilt@2.1.7 webpack-merge yarn@$YARN_VERSION -g --unsafe-perm \
 && npm config set color false \
 && npm cache clear --force
RUN wget https://phar.phpunit.de/phpunit-5.7.phar \
 && chmod +x phpunit-5.7.phar \
 && mv phpunit-5.7.phar /bin/phpunit
RUN curl -O https://raw.githubusercontent.com/wp-cli/builds/gh-pages/phar/wp-cli.phar \
 && chmod +x wp-cli.phar \
 && mv wp-cli.phar /bin/wp
