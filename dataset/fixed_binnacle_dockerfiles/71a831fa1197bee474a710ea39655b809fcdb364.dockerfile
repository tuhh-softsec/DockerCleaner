FROM ubuntu:14.04
MAINTAINER Stefan Houtzager <stefan.houtzager@gmail.com>
ENV DEBIAN_FRONTEND="noninteractive"
ENV REFRESHED_AT="27-06-2017"
ENV TERM="xterm"
WORKDIR /
RUN apt-get update \
 && apt-get upgrade -y \
 && (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 curl=7.35.0-1ubuntu2.20 git=1:1.9.1-1ubuntu0.10 unzip=6.0-9ubuntu1.5 -y )
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#   erlang install
RUN echo "deb http://packages.erlang-solutions.com/ubuntu trusty contrib" >> /etc/apt/sources.list \
 && apt-key adv --fetch-keys http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc \
 && apt-get update -qq \
 && (apt-get update ;apt-get install --no-install-recommends esl-erlang=1:20.3 build-essential=11.6ubuntu6 wget=1.15-1ubuntu1.14.04.5 -y ) \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Download and Install Specific Version of Elixir
WORKDIR /elixir
RUN wget -q https://github.com/elixir-lang/elixir/releases/download/v1.6.5/Precompiled.zip \
 && unzip Precompiled.zip \
 && rm -f Precompiled.zip \
 && ln -s /elixir/bin/elixirc /usr/local/bin/elixirc \
 && ln -s /elixir/bin/elixir /usr/local/bin/elixir \
 && ln -s /elixir/bin/mix /usr/local/bin/mix \
 && ln -s /elixir/bin/iex /usr/local/bin/iex
WORKDIR /
#   install Node.js (>= 5.0.0) and NPM in order to satisfy brunch.io dependencies
RUN curl -sL https://deb.nodesource.com/setup_9.x | bash - \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=0.10.25~dfsg2-2ubuntu1.2 inotify-tools=3.14-1ubuntu1 -y )
#   setup our Ubuntu sources (ADD breaks caching)
RUN echo "deb http://archive.ubuntu.com/ubuntu trusty main restricted universe multiverse\ndeb http://archive.ubuntu.com/ubuntu trusty-updates main restricted universe multiverse\ndeb http://archive.ubuntu.com/ubuntu trusty-backports main restricted universe multiverse\ndeb http://security.ubuntu.com/ubuntu trusty-security main restricted universe multiverse \n" > /etc/apt/sources.list
#   no Upstart or DBus
#   https://github.com/dotcloud/docker/issues/1724#issuecomment-26294856
RUN apt-mark hold initscripts udev plymouth mountall
RUN dpkg-divert --local --rename --add /sbin/initctl \
 && ln -sf /bin/true /sbin/initctl
RUN : \
 && apt-get upgrade -y
RUN (apt-get update ;apt-get install --no-install-recommends python-numpy=1:1.8.2-0ubuntu0.1 software-properties-common=0.92.37.8 libsecret-1-0=0.16-0ubuntu1 gnome-keyring=3.10.1-1ubuntu4.4 -y )
RUN (apt-get update ;apt-get install --no-install-recommends supervisor=3.0b2-1ubuntu0.1 openssh-server=1:6.6p1-2ubuntu2.13 pwgen=2.06-1ubuntu4 sudo=1.8.9p5-1ubuntu1.4 vim-tiny=2:7.4.052-1ubuntu3.1 net-tools=1.60-25ubuntu2.1 lxde=0.5.0-4ubuntu4 x11vnc=0.9.13-1.1 xvfb=2:1.15.1-0ubuntu2.11 gtk2-engines-murrine=0.98.2-0ubuntu2 ttf-ubuntu-font-family=0.80-0ubuntu6 libreoffice=1:4.2.8-0ubuntu5.5 firefox=66.0.3+build1-0ubuntu0.14.04.1 xserver-xorg-video-dummy=1:0.3.7-1build1 -y --force-yes ) \
 && apt-get autoclean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/*
RUN mkdir /etc/startup.aux/
RUN echo "#Dummy" > /etc/startup.aux/00.sh
RUN chmod +x /etc/startup.aux/00.sh
RUN mkdir -p /etc/supervisor/conf.d
RUN rm /etc/supervisor/supervisord.conf
#   create an ubuntu user who cannot sudo
#   RUN useradd --create-home --shell /bin/bash --user-group ubuntu
RUN useradd --create-home --shell /bin/bash --user-group --groups adm,sudo ubuntu
RUN echo "ubuntu:badpassword" | chpasswd
COPY elixir-dev-anywhere-docker/startup.sh /
COPY elixir-dev-anywhere-docker/supervisord.conf.xorg /etc/supervisor/supervisord.conf
COPY elixir-dev-anywhere-docker/openbox-config /openbox-config
RUN cp -r /openbox-config/.config ~ubuntu/
RUN chown -R ubuntu ~ubuntu/.config ; chgrp -R ubuntu ~ubuntu/.config
RUN rm -r /openbox-config
ENV HOME="/home/ubuntu"
#   Install phoenix, local Elixir hex and rebar (in ENV HOME)
RUN mix archive.install --force https://github.com/phoenixframework/archives/raw/master/phx_new.ez \
 && mix local.hex --force \
 && mix local.rebar --force \
 && mix hex.info
#   intellij
RUN sed 's/main$/main universe/' -i /etc/apt/sources.list \
 && apt-get update -qq \
 && echo 'Installing OS dependencies' \
 && (apt-get update ;apt-get install --no-install-recommends sudo=1.8.9p5-1ubuntu1.4 software-properties-common=0.92.37.8 libxext-dev=2:1.3.2-1ubuntu0.0.14.04.1 libxrender-dev=1:0.9.8-1build0.14.04.1 libxslt1.1=1.1.28-2ubuntu0.2 libxtst-dev=2:1.2.2-1 libgtk2.0-0=2.24.23-0ubuntu1.4 libcanberra-gtk-module=0.30-0ubuntu3 -qq -y --fix-missing ) \
 && echo 'Cleaning up' \
 && apt-get clean -qq -y \
 && apt-get autoclean -qq -y \
 && apt-get autoremove -qq -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/*
RUN mkdir -p /home/ubuntu/.IdeaIC2017.3/config/options \
 && mkdir -p /home/ubuntu/.IdeaIC2017.3/config/plugins
COPY elixir-dev-anywhere-docker/jdk.table.xml /home/ubuntu/.IdeaIC2017.3/config/options/jdk.table.xml
COPY elixir-dev-anywhere-docker/jdk.table.xml /home/ubuntu/.jdk.table.xml
COPY elixir-dev-anywhere-docker/intellij/run /usr/local/bin/intellij
COPY elixir-dev-anywhere-docker/intellij-elixir.zip /home/ubuntu/.IdeaIC2017.3/config/plugins/intellij-elixir.zip
RUN chmod +x /usr/local/bin/intellij
RUN echo 'Downloading IntelliJ IDEA' \
 && wget https://download.jetbrains.com/idea/ideaIC-2017.3.5.tar.gz -O /tmp/intellij.tar.gz -q \
 && echo 'Installing IntelliJ IDEA' \
 && mkdir -p /opt/intellij \
 && tar -xf /tmp/intellij.tar.gz --strip-components=1 -C /opt/intellij \
 && rm /tmp/intellij.tar.gz
RUN echo 'Installing Elixir plugin' \
 && cd /home/ubuntu/.IdeaIC2017.3/config/plugins/ \
 && unzip -q intellij-elixir.zip \
 && rm intellij-elixir.zip
#   noVNC
COPY elixir-dev-anywhere-docker/noVNC /noVNC/
#   store a password for the VNC service
RUN mkdir /home/root
RUN mkdir /home/root/.vnc
RUN x11vnc -storepasswd badpassword /home/root/.vnc/passwd
COPY elixir-dev-anywhere-docker/xorg.conf /etc/X11/xorg.conf
#   pgadmin3 and nano
#   prerequisites to install a new version of pgadmin3 https://undebugable.wordpress.com/2016/01/11/pgadmin-3-warning-the-server-you-are-connecting-to-is-not-a-version-that-is-supported-by-this-release/
#   add the repository
RUN sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
#   install their key
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends nano=2.2.6-1ubuntu1 postgresql-client=9.3+154ubuntu1.1 pgadmin3=1.18.1-2 -y ) \
 && rm -rf /var/lib/apt/lists/*
ENTRYPOINT ["/startup.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
