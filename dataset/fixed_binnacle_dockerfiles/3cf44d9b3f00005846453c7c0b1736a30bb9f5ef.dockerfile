FROM node:latest
LABEL org.label-schema.schema-version="= 1.0.0  org.label-schema.vendor = mkenney@webbedlam.com  org.label-schema.vcs-url = https://github.com/mkenney/docker-npm  org.label-schema.description = \"This image provides node based build tools.\"  org.label-schema.name = \"NPM\"  org.label-schema.url = http://mkenney.github.io/docker-npm/"
ENV TERM="xterm" \
    NLS_LANG="American_America.AL32UTF8" \
    LANG="C.UTF-8" \
    LANGUAGE="C.UTF-8" \
    LC_ALL="C.UTF-8" \
    TIMEZONE="America/Denver"
RUN set -x \
 && apt-get update -qq \
 && apt-get install --no-install-recommends apt-transport-https=2.2.4 apt-utils=2.2.4 -qqy \
 && apt-get -qq upgrade \
 && apt-get -qq dist-upgrade \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update -qq \
 && apt-get install --no-install-recommends acl=2.2.53-10 git=1:2.30.2-1+deb11u2 mercurial=5.6.1-4 rsync=3.2.3-4+deb11u1 subversion=1.14.1-3+deb11u1 sudo=1.9.5p2-3+deb11u1 wget=1.21-1+deb11u1 -qqy \
 && rm /bin/sh \
 && ln -s /bin/bash /bin/sh
#   install npm packages
RUN set -x \
 && npm install gulp-cli@2.3.0 grunt-cli@1.4.3 bower@1.8.14 markdown-styles@3.2.0 npx@10.2.2 --silent --global \
 && curl --compressed -o- -L https://yarnpkg.com/install.sh | sh
#  #############################################################################
#   UTF-8 Locale, timezone
#  #############################################################################
RUN set -x \
 && apt-get install --no-install-recommends locales=2.31-13+deb11u5 -qqy \
 && locale-gen C.UTF-8 ${UTF8_LOCALE} \
 && dpkg-reconfigure locales \
 && /usr/sbin/update-locale LANG=C.UTF-8 LANGUAGE=C.UTF-8 LC_ALL=C.UTF-8 \
 && export LANG=C.UTF-8 \
 && export LANGUAGE=C.UTF-8 \
 && export LC_ALL=C.UTF-8 \
 && echo $TIMEZONE > /etc/timezone \
 && dpkg-reconfigure -f noninteractive tzdata
#  #############################################################################
#   users
#  #############################################################################
RUN set -x \
 && echo "export NLS_LANG=$( echo $NLS_LANG ;)" >> /root/.bash_profile \
 && echo "export LANG=$( echo $LANG ;)" >> /root/.bash_profile \
 && echo "export LANGUAGE=$( echo $LANGUAGE ;)" >> /root/.bash_profile \
 && echo "export LC_ALL=$( echo $LC_ALL ;)" >> /root/.bash_profile \
 && echo "export TERM=xterm" >> /root/.bash_profile \
 && echo "export PATH=$( echo $PATH ;)" >> /root/.bash_profile \
 && echo "cd /src" >> /root/.bash_profile \
 && echo "source $HOME/.bashrc" >> /root/.bash_profile \
 && groupadd dev \
 && useradd dev -s /bin/bash -m -g dev \
 && echo "dev:password" | chpasswd \
 && echo "dev ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers \
 && rsync -a /root/ /home/dev/ \
 && chown -R dev:dev /home/dev/ \
 && chmod 0777 /home/dev \
 && chmod -R u+rwX,g+rwX,o+rwX /home/dev \
 && setfacl -R -d -m user::rwx,group::rwx,other::rwx /home/dev
#  #############################################################################
#   ~ fin ~
#  #############################################################################
RUN set -x \
 && wget -O /run-as-user https://raw.githubusercontent.com/mkenney/docker-scripts/master/container/run-as-user \
 && chmod 0755 /run-as-user \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
VOLUME /src
WORKDIR /src
ENTRYPOINT ["/run-as-user"]
CMD ["/usr/local/bin/npm"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
