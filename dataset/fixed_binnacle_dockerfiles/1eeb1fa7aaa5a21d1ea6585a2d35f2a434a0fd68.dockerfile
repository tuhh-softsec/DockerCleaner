FROM ubuntu:16.04
MAINTAINER Samuel "samuel.zhao.yue@live.com"
#   Specially for SSH access and port redirection
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
#   Expose ADB, ADB control and VNC ports
EXPOSE 22/tcp
EXPOSE 5037/tcp
EXPOSE 5554/tcp
EXPOSE 5555/tcp
EXPOSE 5900/tcp
EXPOSE 80/tcp
EXPOSE 443/tcp
ENV DEBIAN_FRONTEND="noninteractive"
#   kvm env
ENV RAM="2048"
ENV SMP="1"
ENV CPU="qemu64"
ENV DISK_DEVICE="scsi"
ENV IMAGE="/data/disk-image"
ENV IMAGE_FORMAT="qcow2"
ENV IMAGE_SIZE="10G"
ENV IMAGE_CACHE="none"
ENV IMAGE_DISCARD="unmap"
ENV IMAGE_CREATE="0"
ENV ISO_DOWNLOAD="0"
ENV NETWORK="tap"
ENV VNC="none"
ENV VNC_IP="\"
ENV VNC_ID="0"
ENV VNC_PORT="5500"
ENV VNC_SOCK="/data/vnc.sock"
ENV TCP_PORTS="\"
ENV UDP_PORTS="\"
WORKDIR /root
#  COPY ./etc/apt/sources.list_backup /etc/apt/sources.list
#  RUN apt-get update
RUN apt-get update \
 && apt-get install --no-install-recommends qemu-kvm=1:2.5+dfsg-5ubuntu10.51 qemu-utils=1:2.5+dfsg-5ubuntu10.51 bridge-utils=1.5-9ubuntu1 dnsmasq=2.75-1ubuntu0.16.04.10 uml-utilities=20070815-1.4 iptables=1.6.0-2ubuntu3 wget=1.17.1-1ubuntu1.5 net-tools=1.60-26ubuntu1 -y \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 git=1:2.7.4-0ubuntu1.10 vim=2:7.4.1689-3ubuntu1.5 make=4.1-6 zip=3.0-11 unzip=6.0-20ubuntu1.1 curl=7.47.0-1ubuntu2.19 wget=1.17.1-1ubuntu1.5 bzip2=1.0.6-8ubuntu0.2 ssh=1:7.2p2-4ubuntu2.10 openssh-server=1:7.2p2-4ubuntu2.10 socat=1.7.3.1-1 -y \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 -y \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y \
 && apt-get install --no-install-recommends net-tools=1.60-26ubuntu1 iputils-ping=3:20121221-5ubuntu2 dnsutils=1:9.10.3.dfsg.P4-8ubuntu1.19 -y \
 && apt-get install --no-install-recommends python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 -y \
 && apt-get install --no-install-recommends apt-utils=1.2.35 usbutils=1:007-4 locales=2.23-0ubuntu11.3 udev=229-4ubuntu21.31 -y \
 && apt-get autoremove -y \
 && apt-get clean
#   Install packages needed for android sdk tools
RUN dpkg --add-architecture i386 \
 && apt-get update \
 && apt-get install --no-install-recommends libstdc++6:i386 libgcc1:i386 zlib1g:i386 libncurses5:i386 -y
#   Java Environment Path
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
ENV JRE_HOME="${JAVA_HOME}/jre"
ENV CLASSPATH=".:${JAVA_HOME}/lib:${JRE_HOME}/lib"
ENV PATH="${JAVA_HOME}/bin:$PATH"
#   Install Android SDK
ENV ANDROID_HOME="/opt/android-sdk-linux"
ENV ANDROID_NDK_HOME="$ANDROID_HOME/android-ndk-r14b"
ENV PATH="$PATH:$ANDROID_HOME/tools/:$ANDROID_HOME/platform-tools:$ANDROID_NDK_HOME"
RUN curl -o android-sdk.tgz https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz \
 && tar -C /opt -zxvf android-sdk.tgz > /dev/null
RUN curl -o ndk-bundle.zip https://dl.google.com/android/repository/android-ndk-r14b-linux-x86_64.zip \
 && unzip ndk-bundle.zip -d $ANDROID_HOME > /dev/null
RUN mkdir "$ANDROID_HOME/licenses" || true
RUN echo -e "\n8933bad161af4178b1185d1a37fbf41ea5269c55" > "$ANDROID_HOME/licenses/android-sdk-license"
RUN echo -e "\d56f5187479451eabf01fb78af6dfcb131a6481e" >> "$ANDROID_HOME/licenses/android-sdk-license"
RUN echo -e "\n84831b9409646a918e30573bab4c9c91346d8abd" > "$ANDROID_HOME/licenses/android-sdk-preview-license"
#   Install Android Build Tools and the required version of Android SDK
#   You can create several versions of the Dockerfile if you need to test several versions
RUN (sleep 4 \
 && while [ 1 ] ; do sleep 1 ;echo y ; done ) | android update sdk --no-ui --force -a --filter platform-tool,android-25,android-26,build-tools-25.0.2,build-tools-26.0.1,extra-android-support,extra-android-m2repository,extra-google-m2repository \
 && echo "y" | android update adb
#   RUN which adb
#   RUN which android
#   Gradle 4.2
ENV GRADLE_HOME="/usr/local/gradle-4.2"
ENV PATH="$GRADLE_HOME/bin:$PATH"
RUN curl -o gradle-4.2-all.zip -L https://services.gradle.org/distributions/gradle-4.2-all.zip \
 && unzip gradle-4.2-all.zip -d /usr/local > /dev/null
#   Nodejs Environment Path
ENV PATH="$PATH:/opt/node-v6.11.4-linux-x64/bin"
RUN curl -o node-v6.11.4-linux-x64.tar.xz https://nodejs.org/dist/v6.11.4/node-v6.11.4-linux-x64.tar.xz \
 && tar -C /opt -Jxvf node-v6.11.4-linux-x64.tar.xz > /dev/null
RUN npm install cnpm@9.1.0 -g --registry=https://registry.npm.taobao.org
RUN export CHROMEDRIVER_CDNURL=http://npm.taobao.org/mirrors/chromedriver/
RUN cnpm i -g macaca-cli
RUN cnpm i -g macaca-android
RUN cnpm i -g nosmoke
RUN macaca -v
RUN macaca doctor
#   Run sshd
RUN mkdir /var/run/sshd \
 && echo "root:$ROOTPASSWORD" | chpasswd \
 && sed -i 's/PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config \
 && sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd \
 && echo "export VISIBLE=now" >> /etc/profile
RUN echo "y" | android update sdk -a --no-ui --filter sys-img-x86_64-android-21,Android-21
VOLUME /data
COPY entrypoint.sh /entrypoint.sh
COPY kvmconfig.sh /kvmconfig.sh
RUN chmod +x /entrypoint.sh
RUN chmod +x /kvmconfig.sh
#   ENTRYPOINT ["/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
