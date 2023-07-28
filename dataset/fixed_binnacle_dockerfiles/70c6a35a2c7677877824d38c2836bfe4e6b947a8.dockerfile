#  ###########################################################
#   Dockerfile to build Pwntools container
#   Based on Ubuntu
#  ###########################################################
FROM pwntools/pwntools:stable
MAINTAINER Maintainer Gallopsled et al.
USER root
RUN :
#   Use UTF-8
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.31-13+deb11u5 -y )
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#   Dependencies from .travis.yml addons -> apt -> packages
RUN (apt-get update ;apt-get install --no-install-recommends ash=0.5.11+git20200708+dd9ef66-5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends bash=5.1-2+deb11u1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends dash=0.5.11+git20200708+dd9ef66-5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gcc-multilib=4:10.2.1-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gcc-arm-linux-gnueabi=4:10.2.1-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gcc-aarch64-linux-gnu=4:10.2.1-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gcc-mips-linux-gnu=4:10.2.0-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gcc-powerpc-linux-gnu=4:10.2.1-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gcc=4:10.2.1-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends gdb=10.1-1.7 -y )
RUN (apt-get update ;apt-get install --no-install-recommends ksh=2020.0.0+really93u+20120801-9 -y )
RUN (apt-get update ;apt-get install --no-install-recommends lib32stdc++6=10.2.1-6 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libc6-dev-i386=2.31-13+deb11u5 -y )
RUN (apt-get update ;apt-get install --no-install-recommends mksh=59c-9+b2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends pandoc=2.9.2.1-1+b1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends zsh=5.8-6+deb11u1 -y )
#   Dependencies from travis/install.sh
RUN (apt-get update ;apt-get install --no-install-recommends binutils=2.35.2-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends qemu-user-static=1:5.2+dfsg-11+deb11u2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends binutils-* -y )
RUN (apt-get update ;apt-get install --no-install-recommends libcapstone3 -y )
#   Required for various other things
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.74.0-1.3+deb11u7 -y )
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.21-1+deb11u1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends unzip=6.0-26+deb11u1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-8-jre-headless -y )
RUN (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.10+dfsg-6.7+deb11u3 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libxslt1-dev=1.1.34-4+deb11u1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends ssh=1:8.4p1-5+deb11u1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends lsb-release=11.1.0 -y )
#  ==============================================================================
#                                 ANDROID EMULATOR
#  ==============================================================================
#   Android emulator from travis/install.sh
WORKDIR /usr/local
RUN wget -nv https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz ; tar xf android-sdk_r24.4.1-linux.tgz ; rm -f android-sdk_r24.4.1-linux.tgz
RUN ln -s android-sdk-linux android-sdk
ENV PATH="/usr/local/android-sdk/tools:$PATH"
ENV PATH="/usr/local/android-sdk/platform-tools:$PATH"
RUN wget -nv https://dl.google.com/android/repository/android-ndk-r13b-linux-x86_64.zip ; unzip android-ndk-r13b-linux-x86_64.zip ; rm -f android-ndk-r13b-linux-x86_64.zip
RUN ln -s android-ndk-r13b android-ndk
#   Ensure that all executables can be run by other users e.g. travis
RUN find android-sdk/ -perm 744 -type f -executable | xargs chmod +x
ENV NDK="/usr/local/android-ndk"
ENV PATH="$NDK:$PATH"
RUN echo y | android update sdk --no-ui --all --filter platform-tools,extra-android-support
RUN echo y | android update sdk --no-ui --all --filter android-21
RUN echo y | android update sdk --no-ui --all --filter sys-img-armeabi-v7a-android-21
#   Upgrade pip
RUN pip install pip==23.1 --upgrade
#  ==============================================================================
#                            PWNTOOLS TEST REQUIREMENTS
#  ==============================================================================
#   Install pwntools from 'dev', to get all of the latest dependencies
#   Then uninstall pwntools so we have a clean slate, but still have
#   all of its dependencies installed.
WORKDIR /root
RUN git clone https://github.com/Gallopsled/pwntools
WORKDIR /root/pwntools
RUN pip install --upgrade --editable .
RUN pip install --upgrade --requirement docs/requirements.txt
RUN pip uninstall --yes pwntools
WORKDIR /root
RUN rm -rf pwntools
#  ==============================================================================
#                             PWNTOOLS SSH TEST SETUP
#  ==============================================================================
#   Start the container as travis
RUN useradd -m travis
RUN echo "travis ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/travis
#   Set up SSH stuff so we can SSH into localhost
USER travis
WORKDIR /home/travis
RUN ssh-keygen -t rsa -f ~/.ssh/travis -N ''
RUN echo 'from="127.0.0.1"' $( cat .ssh/travis.pub ;) > .ssh/authorized_keys
RUN echo Host "example.pwnme\n User travis\n HostName 127.0.0.1\n IdentityFile ~/.ssh/travis\n" > ~/.ssh/config
#  ==============================================================================
#                              ANDROID EMULATOR SETUP
#  ==============================================================================
RUN echo no | android --silent create avd --name android-armeabi-v7a --target android-21 --force --snapshot --abi armeabi-v7a
RUN emulator64-arm -avd android-armeabi-v7a -no-window -no-boot-anim -no-skin -no-audio -no-window -no-snapshot &; adb wait-for-device ; adb shell id ; adb shell getprop ; adb emu kill
#   Final touchup
USER root
RUN (apt-get update ;apt-get install --no-install-recommends strace=5.10-1 nano=5.4-2+deb11u2 vim=2:8.2.2434-3+deb11u1 tmux=3.1c-1+deb11u1 -y )
#   Entry point
USER travis
RUN mkdir /home/travis/pwntools
WORKDIR /home/travis/pwntools
COPY run.sh .
COPY pwntools.tar.gz .
RUN tar xf pwntools.tar.gz
RUN sudo rm -f pwntools.tar.gz
RUN sudo pip install -U -e .
ENTRYPOINT bash run.sh
# Please add your HEALTHCHECK here!!!
