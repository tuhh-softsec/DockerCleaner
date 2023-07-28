FROM ubuntu:latest
MAINTAINER JAremko <w3techplaygound@gmail.com>
#   Fix "Couldn't register with accessibility bus" error message
ENV NO_AT_BRIDGE="1"
ENV DEBIAN_FRONTEND="noninteractive"
#   basic stuff
RUN echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf \
 && apt-get update \
 && apt-get install --no-install-recommends bash=5.2.15-2ubuntu1 build-essential=12.9ubuntu3 dbus-x11=1.14.4-1ubuntu1 fontconfig=2.14.1-3ubuntu3 git=1:2.39.2-1ubuntu1 gzip=1.12-1ubuntu1 language-pack-en-base=1:23.04+20230106 libgl1-mesa-glx=23.0.1-1ubuntu1 make=4.3-4.1build1 sudo=1.9.13p1-1ubuntu2 tar=1.34+dfsg-1.1 unzip=6.0-27ubuntu1 \
 && git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
 && cd /tmp/su-exec \
 && make \
 && chmod 770 su-exec \
 && mv ./su-exec /usr/local/sbin/ \
 && apt-get purge build-essential \
 && apt-get autoremove \
 && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*
COPY asEnvUser /usr/local/sbin/
#   Only for sudoers
RUN chown root /usr/local/sbin/asEnvUser \
 && chmod 700 /usr/local/sbin/asEnvUser
#   ^^^^^^^ Those layers are shared ^^^^^^^
#   Build Remacs
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.9ubuntu3 curl=7.88.1-7ubuntu1 \
 && curl https://sh.rustup.rs -sSf | sh -s -- -y \
 && /root/.cargo/bin/rustup install nightly \
 && git clone https://github.com/Wilfred/remacs.git /tmp/remacs-bd \
 && cd /tmp/remacs-bd \
 && export PATH=$PATH:/root/.cargo/bin \
 && rustup override set nightly \
 && apt-get install --no-install-recommends autoconf=2.71-3 libgif7=5.2.1-2.5 gnutls-bin=3.7.8-5ubuntu1 libgtk-3-0=3.24.37-1ubuntu1 libjpeg8=8c-2ubuntu11 libncurses5=6.4-2 libtiff5=4.4.0-6ubuntu1 libxml2=2.9.14+dfsg-1.1build2 libxpm4=1:3.5.12-1.1 libgif-dev=5.2.1-2.5 libgnutls-dev libgtk-3-dev=3.24.37-1ubuntu1 libjpeg-dev=8c-2ubuntu11 libncurses5-dev=6.4-2 libtiff5-dev=4.5.0-4ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 libxpm-dev=1:3.5.12-1.1 texinfo=6.8-6build2 \
 && ./autogen.sh \
 && ./configure \
 && make \
 && rustup self uninstall \
 && apt-get purge autoconf build-essential curl libgif-dev libgnutls-dev libgtk-3-dev libjpeg-dev libncurses5-dev libtiff-dev libxml2-dev libxpm-dev \
 && rm -rf /tmp/* /root/.cargo /var/lib/apt/lists/* /root/.cache/*
ENV UNAME="emacser" \
    GNAME="emacs" \
    UHOME="/home/emacs" \
    UID="1000" \
    GID="1000" \
    WORKSPACE="/mnt/workspace" \
    SHELL="/bin/bash"
WORKDIR "${WORKSPACE}"
ENTRYPOINT ["asEnvUser"]
CMD ["bash", "-c", "emacs", ";", "/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
