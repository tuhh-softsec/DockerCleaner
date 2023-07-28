#   Built with arch: amd64 flavor: lxde image: ubuntu:18.04 localbuild: 1
#
#  ###############################################################################
#   base system
#  ###############################################################################
FROM ubuntu:18.04 AS system
RUN sed -i 's#http://archive.ubuntu.com/#http://tw.archive.ubuntu.com/#' /etc/apt/sources.list
#   built-in packages
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 curl=7.58.0-2ubuntu3.24 apache2-utils=2.4.29-1ubuntu4.27 -y \
 && apt-get update \
 && apt-get install --no-install-recommends supervisor=3.3.1-1.1 nginx=1.14.0-0ubuntu1.11 sudo=1.8.21p2-3ubuntu1.5 net-tools=1.60+git20161116.90da8a0-1ubuntu1 zenity=3.28.1-1 xz-utils=5.2.2-1.3ubuntu0.1 dbus-x11=1.12.2-1ubuntu1.4 x11-utils=7.7+3build1 alsa-utils=1.1.3-1ubuntu1 mesa-utils=8.4.0-1 libgl1-mesa-dri=20.0.8-0ubuntu1~18.04.1 -y --allow-unauthenticated \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
#   install debs error if combine together
RUN add-apt-repository -y ppa:fcwu-tw/apps \
 && apt-get update \
 && apt-get install --no-install-recommends xvfb=2:1.19.6-1ubuntu4.14 x11vnc=0.9.16-1 vim-tiny=2:8.0.1453-1ubuntu1.11 firefox=111.0.1+build2-0ubuntu0.18.04.1 chromium-browser=111.0.5563.64-0ubuntu0.18.04.5 ttf-ubuntu-font-family=1:0.83-2 ttf-wqy-zenhei -y --allow-unauthenticated \
 && add-apt-repository -r ppa:fcwu-tw/apps \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get update \
 && apt-get install --no-install-recommends lxde=10 gtk2-engines-murrine=0.98.2-2ubuntu1 gnome-themes-standard=3.28-1ubuntu1 gtk2-engines-pixbuf=2.24.32-1ubuntu1 gtk2-engines-murrine=0.98.2-2ubuntu1 arc-theme=20180114-1ubuntu1 -y --allow-unauthenticated \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/*
#   Additional packages require ~600MB
#   libreoffice  pinta language-pack-zh-hant language-pack-gnome-zh-hant firefox-locale-zh-hant libreoffice-l10n-zh-tw
#   tini for subreap
ARG TINI_VERSION=v0.18.0
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /bin/tini https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini
RUN chmod +x /bin/tini
#   ffmpeg
RUN apt-get update \
 && apt-get install --no-install-recommends ffmpeg=7:3.4.11-0ubuntu0.1 -y --allow-unauthenticated \
 && rm -rf /var/lib/apt/lists/* \
 && mkdir /usr/local/ffmpeg \
 && ln -s /usr/bin/ffmpeg /usr/local/ffmpeg/ffmpeg
#   python library
COPY image/usr/local/lib/web/backend/requirements.txt /tmp/
RUN apt-get update \
 && dpkg-query -W -f='${Package}\n' > /tmp/a.txt \
 && apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-dev=2.7.15~rc1-1 build-essential=12.4ubuntu1 -y \
 && pip install setuptools==67.6.1 wheel==0.40.0 \
 && pip install -r /tmp/requirements.txt \
 && dpkg-query -W -f='${Package}\n' > /tmp/b.txt \
 && apt-get remove -y `diff --changed-group-format='%>' --unchanged-group-format='' /tmp/a.txt /tmp/b.txt | xargs ` \
 && apt-get autoclean -y \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/apt/* /tmp/a.txt /tmp/b.txt
#  ###############################################################################
#   builder
#  ###############################################################################
FROM ubuntu:18.04 AS builder
RUN sed -i 's#http://archive.ubuntu.com/#http://tw.archive.ubuntu.com/#' /etc/apt/sources.list
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 ca-certificates=20211016ubuntu0.18.04.1 gnupg=2.2.4-1ubuntu1.6 patch=2.7.6-2ubuntu1.1 -y
#   nodejs
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 -y
#   yarn
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update \
 && apt-get install --no-install-recommends yarn -y
#   build frontend
COPY web /src/web
RUN cd /src/web \
 && yarn \
 && npm run build
#  ###############################################################################
#   merge
#  ###############################################################################
FROM system
LABEL maintainer="fcwu.tw@gmail.com"
COPY --from=builder /src/web/dist/ /usr/local/lib/web/frontend/
COPY image /
EXPOSE 80/tcp
WORKDIR /root
ENV HOME="/home/ubuntu" \
    SHELL="/bin/bash"
HEALTHCHECK --interval=30s --timeout=5s CMD curl --fail http://127.0.0.1:6079/api/health
ENTRYPOINT ["/startup.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
