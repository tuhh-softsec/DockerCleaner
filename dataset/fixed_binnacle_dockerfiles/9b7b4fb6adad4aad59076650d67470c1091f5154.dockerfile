#   This image is intended to be used with fdroidserver for the purpose
#   of dynamic scanning of pre-built APKs during the fdroid build process.
#   Start with ubuntu 12.04 (i386).
FROM ubuntu:14.04
MAINTAINER fdroid.dscanner <fdroid.dscanner@gmail.com>
ENV DROZER_URL="https://github.com/mwrlabs/drozer/releases/download/2.3.4/drozer_2.3.4.deb"
ENV DROZER_DEB="drozer_2.3.4.deb"
ENV AGENT_URL="https://github.com/mwrlabs/drozer/releases/download/2.3.4/drozer-agent-2.3.4.apk"
ENV AGENT_APK="drozer-agent-2.3.4.apk"
#   Specially for SSH access and port redirection
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
#   Expose ADB, ADB control and VNC ports
EXPOSE 22/tcp
EXPOSE 5037/tcp
EXPOSE 5554/tcp
EXPOSE 5555/tcp
EXPOSE 5900/tcp
EXPOSE 5901/tcp
ENV DEBIAN_FRONTEND="noninteractive"
RUN echo "debconf shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
RUN echo "debconf shared/accepted-oracle-license-v1-1 seen true" | debconf-set-selections
#   Update packages
RUN :
#   Drozer packages
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.15-1ubuntu1.14.04.5 python2.7=2.7.6-8ubuntu0.5 python-dev=2.7.5-5ubuntu3 python2.7-dev=2.7.6-8ubuntu0.5 python-openssl=0.13-2ubuntu6 python-twisted=13.2.0-1ubuntu1.2 python-protobuf=2.5.0-9ubuntu1 bash-completion=1:2.1-4ubuntu0.2 -y )
#   First, install add-apt-repository, sshd and bzip2
RUN (apt-get update ;apt-get install --no-install-recommends python-software-properties=0.92.37.8 bzip2=1.0.6-5 ssh=1:6.6p1-2ubuntu2.13 net-tools=1.60-25ubuntu2.1 -y )
#   ubuntu 14.04 needs this too
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.92.37.8 -y )
#   Add oracle-jdk7 to repositories
RUN add-apt-repository ppa:webupd8team/java
#   Make sure the package repository is up to date
RUN echo "deb http://archive.ubuntu.com/ubuntu trusty main universe" > /etc/apt/sources.list
#   Update apt
RUN :
#   Add drozer
RUN useradd -ms /bin/bash drozer
#   Install oracle-jdk7
RUN (apt-get update ;apt-get install --no-install-recommends oracle-java7-installer -y )
#   Install android sdk
RUN wget http://dl.google.com/android/android-sdk_r23-linux.tgz
RUN tar -xvzf android-sdk_r23-linux.tgz
RUN mv -v android-sdk-linux /usr/local/android-sdk
#   Install apache ant
RUN wget http://archive.apache.org/dist/ant/binaries/apache-ant-1.8.4-bin.tar.gz
RUN tar -xvzf apache-ant-1.8.4-bin.tar.gz
RUN mv -v apache-ant-1.8.4 /usr/local/apache-ant
#   Add android tools and platform tools to PATH
ENV ANDROID_HOME="/usr/local/android-sdk"
ENV PATH="$PATH:$ANDROID_HOME/tools"
ENV PATH="$PATH:$ANDROID_HOME/platform-tools"
#   Add ant to PATH
ENV ANT_HOME="/usr/local/apache-ant"
ENV PATH="$PATH:$ANT_HOME/bin"
#   Export JAVA_HOME variable
ENV JAVA_HOME="/usr/lib/jvm/java-7-oracle"
#   Remove compressed files.
RUN cd / ; rm android-sdk_r23-linux.tgz \
 && rm apache-ant-1.8.4-bin.tar.gz
#   Some preparation before update
RUN chown -R root:root /usr/local/android-sdk/
#   Install latest android tools and system images
RUN echo "y" | android update sdk --filter platform-tool --no-ui --force
RUN echo "y" | android update sdk --filter platform --no-ui --force
RUN echo "y" | android update sdk --filter build-tools-22.0.1 --no-ui -a
RUN echo "y" | android update sdk --filter sys-img-x86-android-19 --no-ui -a
#  RUN echo "y" | android update sdk --filter sys-img-x86-android-21 --no-ui -a
#  RUN echo "y" | android update sdk --filter sys-img-x86-android-22 --no-ui -a
RUN echo "y" | android update sdk --filter sys-img-armeabi-v7a-android-19 --no-ui -a
#  RUN echo "y" | android update sdk --filter sys-img-armeabi-v7a-android-21 --no-ui -a
#  RUN echo "y" | android update sdk --filter sys-img-armeabi-v7a-android-22 --no-ui -a
#   Update ADB
RUN echo "y" | android update adb
#   Create fake keymap file
RUN mkdir /usr/local/android-sdk/tools/keymaps
RUN touch /usr/local/android-sdk/tools/keymaps/en-us
#   Run sshd
RUN (apt-get update ;apt-get install --no-install-recommends openssh-server=1:6.6p1-2ubuntu2.13 -y )
RUN mkdir /var/run/sshd
RUN echo "root:$ROOTPASSWORD" | chpasswd
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed -i 's/PermitEmptyPasswords no/PermitEmptyPasswords yes/' /etc/ssh/sshd_config
#   SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
ENV NOTVISIBLE="\"in users profile\""
RUN echo "export VISIBLE=now" >> /etc/profile
#   Install socat
RUN (apt-get update ;apt-get install --no-install-recommends socat=1.7.2.3-1 -y )
#   symlink android bins
RUN ln -sv /usr/local/android-sdk/tools/android /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/tools/emulator /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/tools/ddms /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/tools/scheenshot2 /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/tools/monkeyrunner /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/tools/monitor /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/tools/mksdcard /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/tools/uiautomatorviewer /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/tools/traceview /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/platform-tools/adb /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/platform-tools/fastboot /usr/local/bin/
RUN ln -sv /usr/local/android-sdk/platform-tools/sqlite3 /usr/local/bin/
#   Setup DROZER...
#   https://labs.mwrinfosecurity.com/tools/drozer/
#   Run as drozer user
WORKDIR /home/drozer
#   Site lists the shasums, however, I'm not sure the best way to integrate the
#   checks here. No real idiomatic way for Dockerfile to do that and most of
#   the examples online use chained commands but we want things to *BREAK* when
#   the sha doesn't match. So far, I can't seem to reliably make Docker not
#   finish the image build process.
#   Download the console
RUN wget -c $DROZER_URL
#   Install the console
RUN dpkg -i $DROZER_DEB
#   Download agent
RUN wget -c $AGENT_URL
#   Keep it version agnostic for other scripts such as install_drozer.py
RUN mv -v $AGENT_APK drozer-agent.apk
#   Port forwarding required by drozer
RUN echo 'adb forward tcp:31415 tcp:31415' >> /home/drozer/.bashrc
#   Alias for Drozer
RUN echo "alias drozer='drozer console connect'" >> /home/drozer/.bashrc
#   add extra scripting
COPY install_agent.py /home/drozer/install_agent.py
RUN chmod 755 /home/drozer/install_agent.py
COPY enable_service.py /home/drozer/enable_service.py
RUN chmod 755 /home/drozer/enable_service.py
COPY drozer.py /home/drozer/drozer.py
RUN chmod 755 /home/drozer/drozer.py
#   fix ownerships
RUN chown -R drozer.drozer /home/drozer
RUN (apt-get update ;apt-get install --no-install-recommends python-pkg-resources=3.3-1ubuntu1 -y --force-yes )
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=1.5.4-1ubuntu4 python-setuptools=3.3-1ubuntu2 git=1:1.9.1-1ubuntu0.10 -y )
RUN pip install "git+https://github.com/dtmilano/AndroidViewClient.git#egg=androidviewclient"
RUN (apt-get update ;apt-get install --no-install-recommends python-pexpect=3.1-1ubuntu0.1 -y )
#   Add entrypoint
COPY entrypoint.sh /home/drozer/entrypoint.sh
RUN chmod +x /home/drozer/entrypoint.sh
ENTRYPOINT ["/home/drozer/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
