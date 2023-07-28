ARG IMAGE_FROM=skycoin/skycoindev-cli:develop
FROM $IMAGE_FROM
ARG BDATE
ARG SCOMMIT
ARG VS_EXTENSIONS
#   Image labels (see ./hooks/build for ARGS)
LABEL org.label-schema.name="skycoindev-cli" \
      org.label-schema.description="Docker image with go, node, dev tools and Visual Studio Code for Skycoin developers" \
      org.label-schema.vendor="Skycoin project" \
      org.label-schema.url="skycoin.net" \
      org.label-schema.version="1.0.0-rc.1" \
      org.label-schema.schema-version="1.0" \
      org.label-schema.build-date="$BDATE" \
      org.label-schema.vcs-url="https://github.com/skycoin/skycoin.git" \
      org.label-schema.vcs-ref="$SCOMMIT" \
      org.label-schema.usage="https://github.com/skycoin/skycoin/blob/$SCOMMIT/docker/images/dev-vscode/README.md" \
      org.label-schema.docker.cmd="xhost +; cd src; docker run --rm -it -v /tmp/.X11-unix:/tmp/.X11-unix -v $PWD:/go/src -w /go/src -e DISPLAY=$DISPLAY skycoin/skycoindev-vscode:develop"
#   Tell debconf to run in non-interactive mode
ENV DEBIAN_FRONTEND="noninteractive"
#   Create a diferent user to run VS Code
ENV HOME="/home/skydev"
RUN useradd --create-home --home-dir $HOME skydev \
 && chown -R skydev:skydev $HOME
#   Install dependencies for vs code
#   Create and assign permissions to `user` folders
#   Install golang and npm necessaries dependencies to VS Code extensions
#   Add the vscode debian repo
#   Install VS Code extensions passed on build arg VS_EXTENSIONS
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=2.6.0 ca-certificates=20230311 curl=7.88.1-7ubuntu1 gnupg=2.2.40-1ubuntu2 apt-utils=2.6.0 libasound2=1.2.8-1build1 libatk1.0-0=2.47.1-1 libcairo2=1.16.0-7 libcups2=2.4.2-1ubuntu4 libexpat1=2.5.0-1 libfontconfig1=2.14.1-3ubuntu3 libfreetype6=2.12.1+dfsg-4 libgtk2.0-0=2.24.33-2ubuntu2 libpango-1.0-0=1.50.12+ds-1 libx11-xcb1=2:1.8.4-2 libxcomposite1=1:0.4.5-1build2 libxcursor1=1:1.2.1-1 libxdamage1=1:1.1.6-1 libxext6=2:1.3.4-1build1 libxfixes3=1:6.0.0-2 libxi6=2:1.8-1build1 libxrandr2=2:1.5.2-2 libxrender1=1:0.9.10-1.1 libxss1=1:1.2.3-1build2 libxtst6=2:1.2.3-1.1 openssh-client=1:9.0p1-1ubuntu8 xdg-utils=1.1.3-4.1ubuntu3 dconf-editor=43.0-1 dbus-x11=1.14.4-1ubuntu1 libfile-mimeinfo-perl=0.33-1 xdg-user-dirs=0.18-1 xsel=1.2.0+git9bfc13d.20180109-3 -y \
 && mkdir -p $HOME/.cache/dconf \
 && mkdir -p $HOME/.config/dconf \
 && chown skydev:skydev -R $HOME/.config \
 && chown skydev:skydev -R $HOME/.cache \
 && go get -v github.com/ramya-rao-a/go-outline \
 && go get -v github.com/uudashr/gopkgs/cmd/gopkgs \
 && go get -v github.com/acroca/go-symbols \
 && go get -v github.com/stamblerre/gocode \
 && go get -v github.com/ianthehat/godef \
 && go get -v github.com/sqs/goreturns \
 && ln -s /go/bin/gocode /go/bin/gocode-gomod \
 && ln -s /go/bin/godef /go/bin/godef-gomod \
 && npm install tslint@6.1.3 typescript@5.0.4 -g \
 && curl -sSL https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor | apt-key add - \
 && echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list \
 && apt-get update \
 && apt-get install --no-install-recommends code -y \
 && for ext in $VS_EXTENSIONS; do code --user-data-dir $HOME --install-extension $ext ; done \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   Change to `skydev` and generate default user folders to avoid config problems in future
USER skydev
RUN xdg-user-dirs-update --force
#   Back to root user
USER root
#   Copy start.sh script to use it as our Docker ENTRYPOINT
COPY ./start.sh /usr/local/bin/start.sh
#   backwards compat
RUN ln -s usr/local/bin/start.sh /
WORKDIR $GOPATH/src/github.com/skycoin/
ENTRYPOINT ["start.sh"]
#  CMD [ "su", "skydev", "-p", "-c", "/usr/share/code/code" ]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
