FROM ubuntu:xenial
#  ###################
#   Install node and dependencies
#   From: https://github.com/nodejs/docker-node/blob/master/6.11/Dockerfile
RUN apt-get update \
 && apt-get install --no-install-recommends gnupg=1.4.20-1ubuntu3.3 curl=7.47.0-1ubuntu2.19 ca-certificates=20210119~16.04.1 xz-utils=5.1.1alpha+20120614-2ubuntu2 wget=1.17.1-1ubuntu1.5 libgtk2.0-0=2.24.30-1ubuntu1.16.04.2 libgconf-2-4=3.2.6-3ubuntu6 -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean
RUN groupadd --gid 1000 node \
 && useradd --uid 1000 --gid node --shell /bin/bash --create-home node
#   gpg keys listed at https://github.com/nodejs/node#release-team
RUN set -ex \
 && for key in 9554F04D7259F04124DE6B476D5A82AC7E37093B 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 FD3A5288F042B6850C66B31F09FE44734EB7990E 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 DD8F2338BAE7501E3DD5AC78C273792F7D83545D B9AE9905FFD7803F25714661B63B535A4C206CA9 C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 56730D5401028683275BD23C23EFEFE93C4CFFFE; do gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --batch --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done
ENV NPM_CONFIG_LOGLEVEL="info"
ENV NODE_VERSION="6.11.3"
RUN ARCH= \
 && dpkgArch="$( dpkg --print-architecture ;)" \
 && case "${dpkgArch##*-}" in (amd64) ARCH='x64' ;;(ppc64el) ARCH='ppc64le' ;;(s390x) ARCH='s390x' ;;(arm64) ARCH='arm64' ;;(*) echo "unsupported architecture" ; exit 1 ;; esac \
 && curl -SLO "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-$ARCH.tar.xz" \
 && curl -SLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " node-v$NODE_VERSION-linux-$ARCH.tar.xz$" SHASUMS256.txt | sha256sum -c - \
 && tar -xJf "node-v$NODE_VERSION-linux-$ARCH.tar.xz" -C /usr/local --strip-components=1 \
 && rm "node-v$NODE_VERSION-linux-$ARCH.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt \
 && ln -s /usr/local/bin/node /usr/local/bin/nodejs
#  ###################
#   Download fonts
RUN apt-get update -y \
 && apt-get install --no-install-recommends fontconfig=2.11.94-0ubuntu1.1 fonts-ipafont-gothic=00303-13ubuntu1 fonts-ipafont-mincho=00303-13ubuntu1 subversion=1.9.3-2ubuntu1.3 -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean \
 && cd /usr/share/fonts/truetype \
 && for font in https://github.com/google/fonts/trunk/apache/droidsansmono https://github.com/google/fonts/trunk/apache/droidsans https://github.com/google/fonts/trunk/apache/droidserif https://github.com/google/fonts/trunk/apache/roboto https://github.com/google/fonts/trunk/apache/opensans https://github.com/google/fonts/trunk/ofl/gravitasone https://github.com/google/fonts/trunk/ofl/oldstandardtt https://github.com/google/fonts/trunk/ofl/ptsansnarrow https://github.com/google/fonts/trunk/ofl/raleway https://github.com/google/fonts/trunk/ofl/overpass; do svn checkout $font ; done \
 && mkdir /usr/share/fonts/user \
 && fc-cache -fv \
 && apt-get --auto-remove -y remove subversion
#  ###################
#   Download mathjax (same version as plotly.js extras/)
RUN curl -L https://github.com/plotly/plotly.js/archive/master.tar.gz | tar -xvzf - --strip-components=3 plotly.js-master/dist/extras/mathjax
#  ###################
#   Copy and set up Orca
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list' \
 && apt-get update -y \
 && apt-get install --no-install-recommends google-chrome-stable xvfb=2:1.18.4-0ubuntu0.12 poppler-utils=0.41.0-0ubuntu1.16 git=1:2.7.4-0ubuntu1.10 -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean
COPY package.json /var/www/image-exporter/
COPY bin /var/www/image-exporter/bin
COPY src /var/www/image-exporter/src
WORKDIR /var/www/image-exporter
RUN npm install \
 && mkdir build
#  ###################
#   Install and configure monit
COPY deployment/monitrc /etc
RUN cd /opt \
 && wget -q -O - https://mmonit.com/monit/dist/binary/5.25.1/monit-5.25.1-linux-x64.tar.gz | tar xvzf - \
 && ln -s monit-* monit \
 && chmod 600 /etc/monitrc
#  ###################
#   Install latest stable Inkscape
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -y \
 && add-apt-repository -y ppa:inkscape.dev/stable \
 && apt-get update \
 && apt-get install --no-install-recommends inkscape=0.92.4+68~ubuntu16.04.1 -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean
#   Copy Inkscape defaults
COPY deployment/preferences.xml /root/.config/inkscape/
#  ###################
#   Download geo-assets (same version as plotly.js extras/)
RUN wget https://raw.githubusercontent.com/plotly/plotly.js/master/dist/plotly-geo-assets.js -O /plotly-geo-assets.js
#  ###################
#   Configure ImageMagick policy
COPY deployment/ImageMagickPolicy.xml /etc/ImageMagick-6/policy.xml
#  ###################
#   Add entrypoint script
COPY deployment/entrypoint.sh /
#   Add server script
COPY deployment/run_server /
#   Symlink to entrypoint
RUN ln -s /entrypoint.sh /usr/bin/orca
EXPOSE 9091/tcp
ENTRYPOINT ["/entrypoint.sh"]
CMD ["--mathjax", "/mathjax/MathJax.js", "--topojson", "/plotly-geo-assets.js"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
