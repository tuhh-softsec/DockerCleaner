FROM ubuntu:16.04
WORKDIR /
#   Update source
RUN :
#   Basic tools
RUN (apt-get update ;apt-get install --no-install-recommends apt-utils=1.2.35 build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 curl=7.47.0-1ubuntu2.19 git=1:2.7.4-0ubuntu1.10 libcurl3-dev libgoogle-glog-dev=0.3.4-0.1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libpng12-dev=1.2.54-1ubuntu1.1 libprotobuf-dev=2.6.1-1.3 libzmq3-dev=4.1.4-7ubuntu0.1 pkg-config=0.29.1-0ubuntu1 protobuf-compiler=2.6.1-1.3 libprotoc-dev=2.6.1-1.3 rsync=3.1.1-3ubuntu1.3 software-properties-common=0.96.20.10 unzip=6.0-20ubuntu1.1 zip=3.0-11 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openjdk-8-jre-headless=8u292-b10-0ubuntu1~16.04.1 openssh-server=1:7.2p2-4ubuntu2.10 wget=1.17.1-1ubuntu1.5 bsdmainutils=9.0.6ubuntu3 -y )
#   install pyenv
RUN (apt-get update ;apt-get install --no-install-recommends make=4.1-6 libssl-dev=1.0.2g-1ubuntu4.20 libbz2-dev=1.0.6-8ubuntu0.2 libreadline-dev=6.3-8ubuntu2 libsqlite3-dev=3.11.0-1ubuntu1.5 llvm=1:3.8-33ubuntu3.1 libncurses5-dev=6.0+20160213-1ubuntu1 libncursesw5-dev=6.0+20160213-1ubuntu1 xz-utils=5.1.1alpha+20120614-2ubuntu2 tk-dev=8.6.0+9 libffi-dev=3.2.1-4 liblzma-dev=5.1.1alpha+20120614-2ubuntu2 python-openssl=0.15.1-2ubuntu0.2 -y )
RUN curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
ENV PATH="/root/.pyenv/bin:/root/.pyenv/shims/:${PATH}"
RUN eval "$( pyenv init - ;)"
RUN eval "$( pyenv virtualenv-init - ;)"
RUN pyenv install 3.6.3
RUN pyenv global 3.6.3
#   Setup vim
RUN (apt-get update ;apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 vim=2:7.4.1689-3ubuntu1.5 -y )
RUN locale-gen en_US.UTF-8
ENV LC_CTYPE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV TERM="xterm-256color"
#   Set up Bazel.
#   Running bazel inside a `docker build` command causes trouble, cf:
#     https://github.com/bazelbuild/bazel/issues/134
#   The easiest solution is to set up a bazelrc file forcing --batch.
RUN echo "startup --batch" >> /etc/bazel.bazelrc
#   Similarly, we need to workaround sandboxing issues:
#     https://github.com/bazelbuild/bazel/issues/418
RUN echo "build --spawn_strategy=standalone --genrule_strategy=standalone" >> /etc/bazel.bazelrc
#   Install the most recent bazel release.
ENV BAZEL_VERSION="0.16.0"
RUN mkdir /tmp/bazel \
 && cd /tmp/bazel \
 && wget https://github.com/bazelbuild/bazel/releases/download/$BAZEL_VERSION/bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && chmod +x bazel-*.sh \
 && ./bazel-$BAZEL_VERSION-installer-linux-x86_64.sh \
 && cd / \
 && rm -rf /tmp/bazel
#   Install SDK
ENV ANDROID_SDK_VERSION="4333796"
ENV ANDROID_BUILD_TOOLS_VERSION="26.0.2"
ENV ANDROID_SDK_FILENAME="sdk-tools-linux-${ANDROID_SDK_VERSION}.zip"
ENV ANDROID_SDK_URL="https://dl.google.com/android/repository/${ANDROID_SDK_FILENAME}"
ENV ANDROID_API_LEVELS="android-26"
ENV ANDROID_HOME="/opt/sdk"
ENV PATH="${PATH}:${ANDROID_HOME}/tools:${ANDROID_HOME}/platform-tools"
RUN mkdir -p /opt/sdk \
 && cd /opt \
 && wget -q ${ANDROID_SDK_URL} \
 && unzip ${ANDROID_SDK_FILENAME} -d ${ANDROID_HOME} \
 && rm ${ANDROID_SDK_FILENAME} \
 && yes | android update sdk --no-ui -a --filter tools,platform-tools,${ANDROID_API_LEVELS},build-tools-${ANDROID_BUILD_TOOLS_VERSION}
RUN ${ANDROID_HOME}/tools/bin/sdkmanager "cmake;3.6.4111459"
#   Download NDK 19c
RUN cd /opt/ \
 && wget -q https://dl.google.com/android/repository/android-ndk-r19c-linux-x86_64.zip \
 && unzip -q android-ndk-r19c-linux-x86_64.zip \
 && rm -f android-ndk-r19c-linux-x86_64.zip
ENV ANDROID_NDK_VERSION="r19c"
ENV ANDROID_NDK_HOME="/opt/android-ndk-${ANDROID_NDK_VERSION}"
#   Install tools
RUN (apt-get update ;apt-get install --no-install-recommends android-tools-adb=5.1.1r36+git20160322-0ubuntu3 -y )
#   fix docker in docker error: `error while loading shared libraries: libltdl.so.7`
#   refer to: https://github.com/jenkinsci/docker/issues/506
RUN (apt-get update ;apt-get install --no-install-recommends libltdl7=2.4.6-0.1 -y )
RUN pip install mirrors.aliyun.com==null pip==23.1 setuptools==67.6.1 -i http://mirrors.aliyun.com/pypi/simple/ --trusted-host --upgrade
RUN pip install mirrors.aliyun.com==null numpy==1.15.4 scipy==1.2.0 Jinja2==2.10 PyYAML==3.13 sh==1.12.14 pycodestyle==2.4.0 filelock==3.0.10 PTable==0.9.2 -i http://mirrors.aliyun.com/pypi/simple/ --trusted-host
RUN pip install mirrors.aliyun.com==null tensorflow==1.8.0 -i http://mirrors.aliyun.com/pypi/simple/ --trusted-host
#   Install pytorch (refer to: https://pytorch.org/get-started/locally/)
RUN pip install mirrors.aliyun.com==null future==0.17.1 Pillow==5.4.1 torch==1.1.0 torchvision==0.2.2.post3 -i http://mirrors.aliyun.com/pypi/simple/ --trusted-host
RUN pip install mirrors.aliyun.com==null onnx==1.5.0 onnx-tf==1.2.0 -i http://mirrors.aliyun.com/pypi/simple/ --trusted-host
RUN pip install mirrors.aliyun.com==null cpplint==1.4.4 -i http://mirrors.aliyun.com/pypi/simple/ --trusted-host
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
