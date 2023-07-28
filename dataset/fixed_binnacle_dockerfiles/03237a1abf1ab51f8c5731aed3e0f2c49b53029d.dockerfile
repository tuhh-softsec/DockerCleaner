FROM microsoft/dotnet:latest
#  from cmiles74/dotnet:latest
#   get add-apt-repository
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.99.35 curl=7.88.1-7ubuntu1 apt-transport-https=2.6.0 -y )
#   add SQL Server tools PPA
#   run curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
#   run curl https://packages.microsoft.com/config/ubuntu/16.04/prod.list | tee /etc/apt/sources.list.d/msprod.list
#   add nodejs ppa
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
#   update apt cache
RUN :
#   vscode dependencies
RUN (apt-get update ;apt-get install --no-install-recommends libc6-dev=2.37-0ubuntu2 libgtk2.0-0=2.24.33-2ubuntu2 libgtk-3-0=3.24.37-1ubuntu1 libpango-1.0-0=1.50.12+ds-1 libcairo2=1.16.0-7 libfontconfig1=2.14.1-3ubuntu3 libgconf2-4 libnss3=2:3.87.1-1 libasound2=1.2.8-1build1 libxtst6=2:1.2.3-1.1 unzip=6.0-27ubuntu1 libglib2.0-bin=2.76.0-1ubuntu1 libcanberra-gtk-module=0.30-10ubuntu4 libgl1-mesa-glx=23.0.1-1ubuntu1 curl=7.88.1-7ubuntu1 build-essential=12.9ubuntu3 gettext=0.21-11 libstdc++6=13-20230320-1ubuntu1 software-properties-common=0.99.35 wget=1.21.3-1ubuntu1 git=1:2.39.2-1ubuntu1 xterm=379-1ubuntu1 automake=1:1.16.5-1.3 libtool=2.4.7-5 autogen=1:5.18.16-5 nodejs=18.13.0+dfsg1-1ubuntu2 libnotify-bin=0.8.1-1 aspell=0.60.8-4build1 aspell-en=2018.04.16-0-1 htop=3.2.2-1 git=1:2.39.2-1ubuntu1 emacs25 mono-complete=6.8.0.105+dfsg-3.3 gvfs-bin libxss1=1:1.2.3-1build2 rxvt-unicode-256color x11-xserver-utils=7.7+9build1 sudo=1.9.13p1-1ubuntu2 vim=2:9.0.1000-4ubuntu2 libxkbfile1=1:1.1.0-1build3 -y )
#   MS SQL Server tools
#
#   This doesn't work because it makes you agree to a license agreement. I've
#   tried "yes" but to no avail.
#
#   run apt-get install mssql-tools
#   update npm
RUN npm install npm@9.6.4 -g
#   install vscode
RUN wget -O vscode-amd64.deb https://go.microsoft.com/fwlink/?LinkID=760868
RUN dpkg -i vscode-amd64.deb
RUN rm vscode-amd64.deb
#   install flat plat theme
RUN wget 'https://github.com/nana-4/Flat-Plat/releases/download/3.20.20160404/Flat-Plat-3.20.20160404.tar.gz'
RUN tar -xf Flat-Plat*
RUN mv Flat-Plat /usr/share/themes
RUN rm Flat-Plat*gz
RUN mv /usr/share/themes/Default /usr/share/themes/Default.bak
RUN ln -s /usr/share/themes/Flat-Plat /usr/share/themes/Default
#   install hack font
RUN wget 'https://github.com/chrissimpkins/Hack/releases/download/v2.020/Hack-v2_020-ttf.zip'
RUN unzip Hack*.zip
RUN mkdir /usr/share/fonts/truetype/Hack
RUN mv Hack* /usr/share/fonts/truetype/Hack
RUN fc-cache -f -v
#   create our developer user
WORKDIR /root
RUN groupadd -r developer -g 1000
RUN useradd -u 1000 -r -g developer -d /developer -s /bin/bash -c "Software Developer" developer
COPY /developer /developer
WORKDIR /developer
#   default browser firefox
RUN ln -s /developer/.local/share/firefox/firefox /bin/xdg-open
#   enable sudo for developer
RUN echo "developer ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/developer
#   fix developer permissions
RUN chmod +x /developer/bin/*
RUN chown -R developer:developer /developer
USER developer
#   install firefox
RUN mkdir Applications
#  run wget "https://download.mozilla.org/?product=firefox-aurora-latest-ssl&os=linux64&lang=en-US" -O firefox.tar.bz2
RUN wget "https://ftp.mozilla.org/pub/firefox/nightly/2016/06/2016-06-30-00-40-07-mozilla-aurora/firefox-49.0a2.en-US.linux-x86_64.tar.bz2" -O firefox.tar.bz2
RUN tar -xf firefox.tar.bz2
RUN mv firefox .local/share
RUN rm firefox.tar.bz2
#   links for firefox
RUN ln -s /developer/.local/share/firefox/firefox /developer/bin/x-www-browser
RUN ln -s /developer/.local/share/firefox/firefox /developer/bin/gnome-www-browser
#   copy in test project
COPY project /developer/project
WORKDIR /developer/project
#   setup our ports
EXPOSE 5000/tcp
EXPOSE 3000/tcp
EXPOSE 3001/tcp
#   install spacemacs
USER developer
WORKDIR /developer
RUN git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d
#   set environment variables
ENV PATH="/developer/.npm/bin:$PATH"
ENV NODE_PATH="/developer/.npm/lib/node_modules:$NODE_PATH"
ENV BROWSER="/developer/.local/share/firefox/firefox-bin"
ENV SHELL="/bin/bash"
#   mount points
VOLUME ["/developer/.config/Code"]
VOLUME ["/developer/.vscode"]
VOLUME ["/developer/.ssh"]
VOLUME ["/developer/project"]
#   start vscode
ENTRYPOINT ["/developer/bin/start-shell"]
# Please add your HEALTHCHECK here!!!
