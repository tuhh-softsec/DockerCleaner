#   Dockerized nvm development environment
#
#   This Dockerfile is for building nvm development environment only,
#   not for any distribution/production usage.
#
#   Please note that it'll use about 1.2 GB disk space and about 15 minutes to
#   build this image, it depends on your hardware.
#   Use Ubuntu Trusty Tahr as base image as we're using on Travis CI
#   I also tested with Ubuntu 16.04, should be good with it!
FROM ubuntu:14.04
LABEL maintainer="Peter Dave Hello <hsu@peterdavehello.org>"
LABEL name="nvm-dev-env"
LABEL version="latest"
#   Set the SHELL to bash with pipefail option
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#   Prevent dialog during apt install
ENV DEBIAN_FRONTEND="noninteractive"
#   ShellCheck version
ENV SHELLCHECK_VERSION="0.5.0"
#   Pick a Ubuntu apt mirror site for better speed
#   ref: https://launchpad.net/ubuntu/+archivemirrors
ENV UBUNTU_APT_SITE="ubuntu.cs.utah.edu"
#   Disable src package source
RUN sed -i 's/^deb-src\ /\#deb-src\ /g' /etc/apt/sources.list
#   Replace origin apt package site with the mirror site
RUN sed -E -i "s/([a-z]+.)?archive.ubuntu.com/$UBUNTU_APT_SITE/g" /etc/apt/sources.list
RUN sed -i "s/security.ubuntu.com/$UBUNTU_APT_SITE/g" /etc/apt/sources.list
#   Install apt packages
RUN apt-get update \
 && apt-get upgrade -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confold" \
 && apt-get install --no-install-recommends coreutils=8.21-1ubuntu5.4 util-linux=2.20.1-5.1ubuntu20.9 bsdutils=1:2.20.1-5.1ubuntu20.9 file=1:5.14-2ubuntu3.4 openssl=1.0.1f-1ubuntu2.27 ca-certificates=20170717~14.04.2 ssh=1:6.6p1-2ubuntu2.13 wget=1.15-1ubuntu1.14.04.5 patch=2.7.1-4ubuntu2.4 sudo=1.8.9p5-1ubuntu1.4 htop=1.0.2-3 dstat=0.7.2-3build1 vim=2:7.4.052-1ubuntu3.1 tmux=1.8-5 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 jq=1.3-1.1ubuntu1.1 realpath=1.19 zsh=5.0.2-3ubuntu6.3 ksh=93u+20120801-1ubuntu0.14.04.1 gcc-4.8=4.8.4-2ubuntu1~14.04.4 g++-4.8=4.8.4-2ubuntu1~14.04.4 xz-utils=5.1.1alpha+20120614-2ubuntu2 build-essential=11.6ubuntu6 bash-completion=1:2.1-4ubuntu0.2 Dpkg::Options::="--force-confdef" Dpkg::Options::="--force-confold" -y -o -o \
 && apt-get clean
#   ShellCheck with Ubuntu 14.04 container workaround
RUN wget https://storage.googleapis.com/shellcheck/shellcheck-v$SHELLCHECK_VERSION.linux.x86_64.tar.xz -O- | tar xJvf - shellcheck-v$SHELLCHECK_VERSION/shellcheck \
 && mv shellcheck-v$SHELLCHECK_VERSION/shellcheck /bin \
 && rmdir shellcheck-v$SHELLCHECK_VERSION \
 && touch /tmp/libc.so.6 \
 && echo "alias shellcheck='LD_LIBRARY_PATH=/tmp /bin/shellcheck'" >> /etc/bash.bashrc
RUN LD_LIBRARY_PATH=/tmp shellcheck -V
#   Set locale
RUN locale-gen en_US.UTF-8
#   Print tool versions
RUN bash --version | head -n 1
RUN zsh --version
RUN ksh --version || true
RUN dpkg -s dash | grep ^Version | awk '{print $2}'
RUN git --version
RUN curl --version
RUN wget --version
#   Add user "nvm" as non-root user
RUN useradd -ms /bin/bash nvm
#   Copy and set permission for nvm directory
COPY . /home/nvm/.nvm/
RUN chown nvm:nvm -R "home/nvm/.nvm"
#   Set sudoer for "nvm"
RUN echo 'nvm ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers
#   Switch to user "nvm" from now
USER nvm
#   nvm
RUN echo 'export NVM_DIR="$HOME/.nvm"' >> "$HOME/.bashrc"
RUN echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> "$HOME/.bashrc"
RUN echo '[ -s "$NVM_DIR/bash_completion" ] \
 && . "$NVM_DIR/bash_completion" # This loads nvm bash_completion' >> "$HOME/.bashrc"
#   nodejs and tools
RUN bash -c 'source $HOME/.nvm/nvm.sh \
 && nvm install node \
 && npm install -g doctoc urchin eclint dockerfile_lint \
 && npm install --prefix "$HOME/.nvm/"'
#   Set WORKDIR to nvm directory
WORKDIR /home/nvm/.nvm
ENTRYPOINT ["/bin/bash"]
# Please add your HEALTHCHECK here!!!
