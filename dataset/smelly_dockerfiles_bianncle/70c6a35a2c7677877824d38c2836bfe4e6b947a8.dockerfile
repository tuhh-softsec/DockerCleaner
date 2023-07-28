# ###########################################################
#  Dockerfile to build Pwntools container
#  Based on Ubuntu
# ###########################################################
FROM pwntools/pwntools:stable
MAINTAINER Maintainer Gallopsled et al.
USER root
RUN apt-get update
#  Use UTF-8
RUN apt-get install locales -y
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"
#  Dependencies from .travis.yml addons -> apt -> packages
RUN apt-get install ash -y
RUN apt-get install bash -y
RUN apt-get install dash -y
RUN apt-get install gcc-multilib -y
RUN apt-get install gcc-arm-linux-gnueabi -y
RUN apt-get install gcc-aarch64-linux-gnu -y
RUN apt-get install gcc-mips-linux-gnu -y
RUN apt-get install gcc-powerpc-linux-gnu -y
RUN apt-get install gcc -y
RUN apt-get install gdb -y
RUN apt-get install ksh -y
RUN apt-get install lib32stdc++6 -y
RUN apt-get install libc6-dev-i386 -y
RUN apt-get install mksh -y
RUN apt-get install pandoc -y
RUN apt-get install zsh -y
#  Dependencies from travis/install.sh
RUN apt-get install binutils -y
RUN apt-get install qemu-user-static -y
RUN apt-get install binutils-* -y
RUN apt-get install libcapstone3 -y
#  Required for various other things
RUN apt-get install curl -y
RUN apt-get install wget -y
RUN apt-get install unzip -y
RUN apt-get install openjdk-8-jre-headless -y
RUN apt-get install libxml2-dev -y
RUN apt-get install libxslt1-dev -y
RUN apt-get install ssh -y
RUN apt-get install lsb-release -y
# ==============================================================================
#                                ANDROID EMULATOR
# ==============================================================================
#  Android emulator from travis/install.sh
WORKDIR /usr/local
RUN wget -nv https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz ; tar xf android-sdk_r24.4.1-linux.tgz ; rm -f android-sdk_r24.4.1-linux.tgz
RUN ln -s android-sdk-linux android-sdk
ENV PATH="/usr/local/android-sdk/tools:$PATH"
ENV PATH="/usr/local/android-sdk/platform-tools:$PATH"
RUN wget -nv https://dl.google.com/android/repository/android-ndk-r13b-linux-x86_64.zip ; unzip android-ndk-r13b-linux-x86_64.zip ; rm -f android-ndk-r13b-linux-x86_64.zip
RUN ln -s android-ndk-r13b android-ndk
#  Ensure that all executables can be run by other users e.g. travis
RUN find android-sdk/ -perm 744 -type f -executable | xargs chmod +x
ENV NDK="/usr/local/android-ndk"
ENV PATH="$NDK:$PATH"
RUN echo y | android update sdk --no-ui --all --filter platform-tools,extra-android-support
RUN echo y | android update sdk --no-ui --all --filter android-21
RUN echo y | android update sdk --no-ui --all --filter sys-img-armeabi-v7a-android-21
#  Upgrade pip
RUN pip install pip --upgrade
# ==============================================================================
#                           PWNTOOLS TEST REQUIREMENTS
# ==============================================================================
#  Install pwntools from 'dev', to get all of the latest dependencies
#  Then uninstall pwntools so we have a clean slate, but still have
#  all of its dependencies installed.
WORKDIR /root
RUN git clone https://github.com/Gallopsled/pwntools
WORKDIR /root/pwntools
RUN pip install --upgrade --editable .
RUN pip install --upgrade --requirement docs/requirements.txt
RUN pip uninstall --yes pwntools
WORKDIR /root
RUN rm -rf pwntools
# ==============================================================================
#                            PWNTOOLS SSH TEST SETUP
# ==============================================================================
#  Start the container as travis
RUN useradd -m travis
RUN echo "travis ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/travis
#  Set up SSH stuff so we can SSH into localhost
USER travis
WORKDIR /home/travis
RUN ssh-keygen -t rsa -f ~/.ssh/travis -N ''
RUN echo 'from="127.0.0.1"' $( cat .ssh/travis.pub ;) > .ssh/authorized_keys
RUN echo Host "example.pwnme\n User travis\n HostName 127.0.0.1\n IdentityFile ~/.ssh/travis\n" > ~/.ssh/config
# ==============================================================================
#                             ANDROID EMULATOR SETUP
# ==============================================================================
RUN echo no | android --silent create avd --name android-armeabi-v7a --target android-21 --force --snapshot --abi armeabi-v7a
RUN emulator64-arm -avd android-armeabi-v7a -no-window -no-boot-anim -no-skin -no-audio -no-window -no-snapshot &; adb wait-for-device ; adb shell id ; adb shell getprop ; adb emu kill
#  Final touchup
USER root
RUN apt-get install strace nano vim tmux -y
#  Entry point
USER travis
RUN mkdir /home/travis/pwntools
WORKDIR /home/travis/pwntools
ADD run.sh .
COPY pwntools.tar.gz .
RUN tar xf pwntools.tar.gz
RUN sudo rm -f pwntools.tar.gz
RUN sudo pip install -U -e .
ENTRYPOINT bash run.sh
