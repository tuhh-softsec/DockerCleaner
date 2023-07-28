FROM ubuntu:16.04 AS ubuntu1604-nojava
ENV DEBIAN_FRONTEND="noninteractive"
COPY --from=gcr.io/bazel-public/base:bazelisk /usr/local/bin/bazel /usr/local/bin/bazel
COPY --from=gcr.io/bazel-public/base:buildifier /usr/local/bin/buildifier /usr/local/bin/buildifier
COPY --from=gcr.io/bazel-public/base:github-release /usr/local/bin/github-release /usr/local/bin/github-release
COPY --from=gcr.io/bazel-public/base:saucelabs /usr/local/bin/sc /usr/local/bin/sc
#  ## Install required packages.
RUN dpkg --add-architecture i386 \
 && apt-get update -qqy \
 && echo "Installing base packages" \
 && apt-get install --no-install-recommends apt-utils=1.2.35 curl=7.47.0-1ubuntu2.19 lsb-release=9.20160110ubuntu0.2 software-properties-common=0.96.20.10 -qqy \
 && echo "Installing packages required by Bazel" \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 clang=1:3.8-33ubuntu3.1 curl=7.47.0-1ubuntu2.19 ed=1.10-2 git=1:2.7.4-0ubuntu1.10 iproute2=4.3.0-1ubuntu3.16.04.5 iputils-ping=3:20121221-5ubuntu2 netcat-openbsd=1.105-7ubuntu1 python=2.7.12-1~16.04 python-dev=2.7.12-1~16.04 python3=3.5.1-3 python3-dev=3.5.1-3 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 xvfb=2:1.18.4-0ubuntu0.12 zip=3.0-11 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -qqy \
 && echo "Installing packages required by Android SDK" \
 && apt-get install --no-install-recommends expect=5.45-7 libbz2-1.0:i386 libncurses5:i386 libstdc++6:i386 libz1:i386 -qqy \
 && echo "Installing packages required by Tensorflow" \
 && apt-get install --no-install-recommends libcurl3-dev swig=3.0.8-0ubuntu3 python-enum34=1.1.2-1 python-mock=1.3.0-2.1ubuntu1 python-numpy=1:1.11.0-1ubuntu1 python-pip=8.1.1-2ubuntu0.6 python-wheel=0.29.0-1 python3-mock=1.3.0-2.1ubuntu1 python3-numpy=1:1.11.0-1ubuntu1 python3-pip=8.1.1-2ubuntu0.6 python3-wheel=0.29.0-1 -qqy \
 && echo "Installing packages required by Envoy" \
 && apt-get install --no-install-recommends automake=1:1.15-4ubuntu1 autotools-dev=20150820.1 cmake=3.5.1-1ubuntu3 libtool=2.4.6-0.1 m4=1.4.17-5 ninja-build=1.5.1-0.1ubuntu1 -qqy \
 && echo "Installing packages required by Android emulator" \
 && apt-get install --no-install-recommends cpio=2.11+dfsg-5ubuntu1.1 cpu-checker=0.7-0ubuntu7 lsof=4.89+dfsg-0.1 qemu-kvm=1:2.5+dfsg-5ubuntu10.51 qemu-system-x86=1:2.5+dfsg-5ubuntu10.51 unzip=6.0-20ubuntu1.1 xvfb=2:1.18.4-0ubuntu0.12 -qqy \
 && echo "Installing packages required by Bazel release process" \
 && apt-get install --no-install-recommends devscripts=2.16.2ubuntu3 gnupg=1.4.20-1ubuntu3.3 pandoc=1.16.0.2~dfsg-1 reprepro=4.17.0-1 -qqy \
 && echo "Installing packages required by C++ coverage tests" \
 && apt-get install --no-install-recommends lcov=1.12-2 llvm=1:3.8-33ubuntu3.1 -qqy \
 && echo "Installing packages required by Swift toolchain" \
 && apt-get install --no-install-recommends clang=1:3.8-33ubuntu3.1 libicu-dev=55.1-7ubuntu0.5 -qqy \
 && echo "Installing packages required by rules_webtesting" \
 && apt-get install --no-install-recommends python-urllib3=1.13.1-2ubuntu0.16.04.4 python3-urllib3=1.13.1-2ubuntu0.16.04.4 -qqy \
 && echo "Installing packages required by Kythe" \
 && apt-get install --no-install-recommends bison=2:3.0.4.dfsg-1 flex=2.6.0-11 uuid-dev=2.27.1-6ubuntu3.10 asciidoc=8.6.9-3 graphviz=2.38.0-12ubuntu2.1 source-highlight=3.1.8-1.1 -qqy \
 && echo "Installing packages required by upb" \
 && apt-get install --no-install-recommends libreadline-dev=6.3-8ubuntu2 -qqy \
 && echo "Installing packages required by Bazel (Ubuntu 14.04 and 16.04 only)" \
 && apt-get install --no-install-recommends realpath=8.25-2ubuntu3~16.04 libssl-dev=1.0.2g-1ubuntu4.20 -qqy \
 && apt-get -qqy purge apport \
 && rm -rf /var/lib/apt/lists/*
#  ## Install Python packages required by Tensorflow.
RUN pip install keras_applications==1.0.8 keras_preprocessing==1.1.2 future==0.18.3 \
 && pip3 install keras_applications keras_preprocessing future
#  ## Install Google Cloud SDK.
#  ## https://cloud.google.com/sdk/docs/quickstart-debian-ubuntu
RUN export CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl -L https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends google-cloud-sdk -qqy \
 && rm -rf /var/lib/apt/lists/*
#  ## Install Docker.
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 ca-certificates=20210119~16.04.1 -qqy \
 && curl -sSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - \
 && add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $( lsb_release -cs ;) stable" \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends docker-ce -qqy \
 && rm -rf /var/lib/apt/lists/*
#  ## Install Node.js and packages required by Gerrit.
#  ## (see https://gerrit.googlesource.com/gerrit/+show/master/polygerrit-ui/README.md)
RUN curl -L https://deb.nodesource.com/setup_10.x | bash - \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -qqy \
 && npm install typescript@5.0.4 polylint@2.10.4 polymer-cli@1.9.11 web-component-tester@6.8.0 eslint@"=4.19.0" eslint-config-google@"=0.12.0" eslint-plugin-html@"=5.0.3" eslint-plugin-promise@"=4.0.1" fried-twinkie@"^0.2.2" --unsafe-perm -g \
 && rm -rf /var/lib/apt/lists/*
#  ## Install Python (required by our own bazelci.py script).
RUN export PYTHON_VERSION="3.6.8" \
 && mkdir -p /usr/local/src \
 && cd /usr/local/src \
 && curl -LO "https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tar.xz" \
 && tar xfJ "Python-${PYTHON_VERSION}.tar.xz" \
 && rm "Python-${PYTHON_VERSION}.tar.xz" \
 && cd "Python-${PYTHON_VERSION}" \
 && echo "_ssl _ssl.c -DUSE_SSL -I/usr/include -I/usr/include/openssl -L/usr/lib -lssl -lcrypto" >> Modules/Setup.dist \
 && echo "Compiling Python ${PYTHON_VERSION} ..." \
 && ./configure --quiet --enable-ipv6 \
 && make -s -j all \
 && echo "Installing Python ${PYTHON_VERSION} ..." \
 && make -s altinstall \
 && pip3.6 install requests uritemplate pyyaml github3.py \
 && rm -rf "/usr/local/src/Python-${PYTHON_VERSION}"
#  ## Install Go.
ENV GO_HOME="\"/opt/go1.12.6.linux-amd64\""
ENV PATH="\"${PATH}:${GO_HOME}/bin\""
#  ## Install Swift toolchain (required by rules_swift).
ENV SWIFT_HOME="\"/opt/swift-4.2.1-RELEASE-ubuntu16.04\""
ENV PATH="\"${PATH}:${SWIFT_HOME}/usr/bin\""
FROM ubuntu1604-nojava AS ubuntu1604-java8
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 -qqy \
 && rm -rf /var/lib/apt/lists/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
