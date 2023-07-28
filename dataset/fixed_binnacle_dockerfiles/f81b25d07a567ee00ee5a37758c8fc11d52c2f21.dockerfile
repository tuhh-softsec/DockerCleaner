FROM debian:stretch
LABEL Name="cyph"
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends apt-transport-https=1.4.11 apt-utils=1.4.11 curl=7.52.1-5+deb9u16 gnupg=2.1.18-8~deb9u4 lsb-release=9.20161125 -y --allow-downgrades )
RUN dpkg --add-architecture i386
RUN echo "deb https://deb.nodesource.com/node_10.x stretch main" >> /etc/apt/sources.list
RUN echo 'deb https://dl.yarnpkg.com/debian/ stable main' >> /etc/apt/sources.list
RUN curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
RUN curl -s https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN :
RUN apt-get -y --allow-downgrades upgrade
RUN (apt-get update ;apt-get install --no-install-recommends autoconf=2.69-10 automake=1:1.15-6 build-essential=12.3 cmake=3.7.2-1 devscripts=2.17.6+deb9u2 expect=5.45-7+deb9u1 gcc-6=6.3.0-18+deb9u1 g++=4:6.3.0-4 git=1:2.11.0-3+deb9u7 golang-go=2:1.7~5 haxe=1:3.2.1+dfsg-1+b1 inotify-tools=3.14-2 lib32ncurses5=6.0+20161126-1+deb9u2 lib32z1=1:1.2.8.dfsg-5+deb9u1 libbz2-1.0:i386 libgconf-2-4=3.2.6-4+b1 libsodium-dev=1.0.11-2 libstdc++6:i386 libtool=2.4.6-2 mono-complete=4.6.2.7+dfsg-1 nano=2.7.4-1 nodejs=4.8.2~dfsg-1 openjdk-8-jdk=8u332-ga-1~deb9u1 perl=5.24.1-3+deb9u7 pinentry-curses=1.0.0-2 procps=2:3.3.12-3+deb9u1 python=2.7.13-2 python-pip=9.0.1-2+deb9u2 ruby=1:2.3.3 ruby-dev=1:2.3.3 shellcheck=0.4.4-4 sudo=1.8.19p1-2.1+deb9u3 tightvncserver=1:1.3.9-9+deb9u1 wget=1.18-5+deb9u3 yarn zopfli=1.0.1+git160119-1+b1 -y --allow-downgrades )
RUN :
RUN apt-get -y --allow-downgrades upgrade
RUN apt-get -y --allow-downgrades autoremove
RUN pip install grpcio==1.53.0
RUN gem update
RUN gem install sass --version 3.7.4
RUN echo ' source /home/gibson/emsdk-portable/emsdk_env.sh &> /dev/null; export GIT_EDITOR="vim"; export GOPATH="/home/gibson/go"; export ANDROID_HOME="/home/gibson/androidsdk"; export JAVA_HOME="$( update-alternatives --query javac | sed -n -e "s/Best: *\(.*\)\/bin\/javac/\\1/p" )"; export PATH="$( echo -n "/opt/local/bin:"; echo -n "/opt/local/sbin:"; echo -n "/usr/local/opt/go/libexec/bin:"; echo -n "${GOPATH}/bin:"; echo -n "${ANDROID_HOME}/platform-tools:"; echo -n "${ANDROID_HOME}/tools:"; echo -n "${PATH}:"; echo -n "/node_modules/.bin"; )"; if [ ! -d ~/.gnupg -a -d ~/.gnupg.original ] ; then cp -a ~/.gnupg.original ~/.gnupg ; fi; export GPG_TTY="$(tty)"; eval $(gpg-agent --daemon 2> /dev/null) &> /dev/null; eval $(ssh-agent 2> /dev/null) &> /dev/null; if [ -f /cyph/commands/.bashrc ] ; then source /cyph/commands/.bashrc ; fi ' >> /.bashrc
RUN echo 'gibson ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers
RUN useradd -ms /bin/bash gibson
RUN mkdir -p /home/gibson
RUN cp -f /.bashrc /home/gibson/.bashrc
RUN cp -f /.bashrc /root/.bashrc
RUN chmod 700 /home/gibson/.bashrc
USER gibson
ENV HOME="/home/gibson"
RUN wget https://s3.amazonaws.com/mozilla-games/emscripten/releases/emsdk-portable.tar.gz -O ~/emsdk.tar.gz
RUN wget https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-150.0.0-linux-x86_64.tar.gz -O ~/gcloud-sdk.tar.gz
RUN ls ~/*.tar.gz | xargs -I% tar xvzf % -C ~
RUN rm ~/*.tar.gz
#  RUN mkdir ~/androidsdk
#  RUN wget https://dl.google.com/android/repository/tools_r25.2.5-linux.zip -O ~/androidsdk.zip
#  RUN unzip ~/androidsdk.zip -d ~/androidsdk
#  RUN rm ~/androidsdk.zip
#  RUN bash -c ' \
#  	source ~/.bashrc; \
#  	mv $ANDROID_HOME/tools $ANDROID_HOME/balls; \
#  	ln -s $ANDROID_HOME/balls $ANDROID_HOME/tools; \
#  	yes | $ANDROID_HOME/tools/bin/sdkmanager --licenses; \
#  	$ANDROID_HOME/tools/bin/sdkmanager \
#  		"tools" \
#  		"platform-tools" \
#  		"platforms;android-25" \
#  		"build-tools;25.0.2" \
#  		"extras;android;m2repository" \
#  		"extras;google;m2repository" \
#  	; \
#  	rm -rf $ANDROID_HOME/balls; \
#  '
RUN mkdir ~/haxelib
RUN haxelib setup ~/haxelib
RUN haxelib install hxcpp
RUN haxelib install hxcs
RUN haxelib install hxjava
RUN haxelib install hxnodejs
RUN rm -rf ~/.gnupg
#  CIRCLECI:RUN sudo apt-get -y --allow-downgrades update
#  CIRCLECI:RUN sudo apt-get -y --allow-downgrades upgrade
#  CIRCLECI:RUN mkdir -p ~/getlibs/commands ~/getlibs/native ~/getlibs/shared/lib/js
#  CIRCLECI:BASE64_FILES
#  CIRCLECI:RUN chmod -R 777 ~/getlibs
#  CIRCLECI:RUN ~/getlibs/commands/updatedockerimage.sh
#  CIRCLECI:RUN ~/getlibs/commands/getlibs.sh
#  CIRCLECI:RUN ~/getlibs/commands/dockerpostmake.sh
#  CIRCLECI:RUN sudo mkdir /cyph
#  CIRCLECI:RUN sudo chmod 777 /cyph
VOLUME /cyph
VOLUME /home/gibson/.cyph
VOLUME /home/gibson/.gitconfig
VOLUME /home/gibson/.gnupg.original
VOLUME /home/gibson/.ssh
WORKDIR /cyph/commands
EXPOSE 9005/tcp 9876/tcp 31337/tcp 42000/tcp 42001/tcp 42002/tcp 44000/tcp
CMD /bin/bash
# Please add your HEALTHCHECK here!!!
